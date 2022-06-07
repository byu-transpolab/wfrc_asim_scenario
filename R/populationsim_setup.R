#' Run populationsim
#' 
run_populationsim <- function(write_result, data_path, out_path){
  popsimStatus <- system2(
    command = "source",
    args = c("sh/run_popsim.sh"),
    env = c(paste0("POPSIM_DATA_PATH=", data_path),
            paste0("POPSIM_OUTPUT_PATH=", out_path))
  )
  
  if(popsimStatus != 0){
    stop("\n\nPopulationSim failed. Check console and/or log(s) for details.\n")
  }
  
  file.path(out_path, "synthetic_persons.csv")
}


#' Get a crosswalk between PUMA's and tracts
#' 
#' @param statefips Which state are we looking for?
#' @param pumas which pumas to keep?
get_puma_tr_cwalk <- function(st_fips, puma_list){
  puma_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt"
  
  # read US census crosswalk, filter, and re
  puma_tract <- read_csv(puma_url, col_types = list(PUMA5CE = col_integer())) %>%
    filter(STATEFP %in% st_fips) %>% 
    transmute(
      TRACT = str_c(STATEFP, COUNTYFP, TRACTCE),  
      PUMA = PUMA5CE
    ) %>%
    filter(PUMA %in% puma_list) %>%
    mutate(
      PUMA = formatC(PUMA, width = 5, flag = "0")
    )
  
  puma_tract
}

#' Get tracts geometry
#'
#' @param puma_tract
#'
#'
get_tracts <- function(st_fips, puma_tract){
  tracts(st_fips, county = unique(substr(puma_tract$TRACT, 3, 5)), 
         class = "sf", progress_bar = FALSE) %>%
    st_transform(4326) %>%
    transmute(GEOID) %>%
    left_join(puma_tract, by = c("GEOID" = "TRACT")) %>%
    filter(!is.na(PUMA))
}

#' Create taz geographic layer
#'
#' @param taz_geo geojson for taz
#' @param ivt0 file with tazes to omit
#' 
#' 
get_taz <- function(taz_geo, ivt0, tr){
  
  # WFRC has 2,881 zones, including zones that are uninhabited and zones
  # that are external to the region.
  taz <- st_read(taz_geo)
  
  # explicitly removing external zones gets us down to 2,858
  interntaz <- taz %>%
    # remove external stations
    filter(!EXTERNAL == 1) %>%
    arrange(TAZID) %>%
    transmute(
      TAZ = TAZID, 
      ACRES, DEVACRES,
      DISTRICT = DISTLRG,
      SD = DISTSML
    ) 
  
  
  # each taz needs to belong to at least one tract / PUMA
  tigris_taz <- interntaz %>%
    
    # remove taz that do not map to a tract
    st_join(tr) %>% 
    filter(!is.na(PUMA)) %>%
    # remove duplicates of TAZ that map to many tracts
    group_by(TAZ) %>% slice(1) %>%  ungroup()
  
  # Additionally, some tazs should be excluded  because they are 
  # not connected to the highway network in 2019 (Utah Lake, Eagle Mountain)
  # These were put into a list by Christian Hunter.
  IVT0 <- read_csv(ivt0)
  IVT0$TAZ <- as.character(IVT0$TAZ)
  
  
  # after removing these TAZ, we have 2,817 TAZs left
  finaltaz <- tigris_taz %>% 
    filter(!TAZ %in% IVT0$TAZ) %>%
    arrange(TAZ) %>%
    mutate(
      asim_taz = row_number()
    )
    
  finaltaz
}

write_taz_map <- function(taz) {
  mapfile <- "inputs/skims/skim_taz_map.csv"
  
  m <- tibble(
    wfrc_taz = 1:2881
  ) %>%
    left_join(
      taz %>% st_set_geometry(NULL) %>% select(TAZ, asim_taz),
      by = c("wfrc_taz" = "TAZ")) 
  
  write_csv(m, mapfile)
  return(mapfile)
}

#' Get taz / puma/ tract crosswalk
#' 
#' @param taz
#' @param tr
#' 
get_crosswalk <- function(taz, tr){
  crosswalk <- taz %>%
    # determine which tract the taz is in
    select(TAZ) %>% st_centroid() %>%
    st_join(tr, st_within) %>% filter(!is.na(GEOID)) %>%
    # fill out remainder of columns
    rename(TRACT = GEOID) %>% mutate(REGION = "1")
  
  
  crosswalk
}


#' Get TAZ control
#' 
#' @param taz_control_file
#'
#'
get_taz_control <- function(se, crosswalk){
  se %>%
    transmute(
      TAZ = as.character(zone_id), 
      HHBASE = as.integer(TOTHH)
    ) %>% 
    filter(TAZ %in% crosswalk$TAZ)
}


#' Get size work variables
#' 
#' @param acvars
#' @param mycounties
#' 
#' Because these two values come in the same table, we will build them together.
#' In both cases the top-line category contains all households with that many
#' workers / persons or more.
#' 
get_sizework_controls <- function(acsvars, mycounties){
  swvars <- str_c("B08202_", sprintf("%03d", c(2:5, 6, 9, 13, 18)))
  raw_sw <- get_acs("tract", variables = swvars, state = "UT", county = mycounties)
  
  size_work <- raw_sw %>% 
    left_join(acsvars, by = c("variable" = "name")) %>%
    separate(label, c("VAR", "total", "label"), sep = "!!") %>%
    select(GEOID, label, estimate) 
  
  works <- size_work %>%
    filter(grepl("work", label)) %>%
    mutate(
      num_work = str_extract(label, "\\d+"),
      workcat = case_when(
        num_work == 1 ~ "HHWORK1",
        num_work == 2 ~ "HHWORK2",
        num_work == 3 ~ "HHWORK3",
        TRUE ~ "HHWORK0"
      )
    ) %>% 
    group_by(GEOID, workcat) %>% summarize(count = as.integer(sum(estimate)))
  
  sizes <- size_work %>%
    filter(!grepl("work", label)) %>%
    mutate(
      num_size = str_extract(label, "\\d+"),
      sizecat = str_c("HHSIZE", num_size) 
    ) %>%
    group_by(GEOID, sizecat) %>% summarize(count = as.integer(sum(estimate)))
  
  list( "sizes" = sizes, "works" =  works )
}

#' Get age variables
#' 
#' @param acsvars
#' @param mycounties
#'
#' This is the number of people in each age category.
#' 
get_age_controls <- function(acsvars, mycounties){
  agevars <- str_c("B01001_", sprintf("%03d", c(3:25, 27:49)))
  raw_ages <- get_acs("tract", variables = agevars, state = "UT", county = mycounties)
  
  ages <- raw_ages %>%
    left_join(acsvars, by = c("variable" = "name")) %>%
    separate(label, c("VAR", "total", "sex", "age"), sep = "!!") %>%
    select(GEOID, sex, age, estimate)  %>%
    
    # regroup age categories
    mutate(
      numage = as.numeric(substr(age, 1, 2)),
      agecat = case_when(
        numage %in% c(15:24) ~ "PAGE1",
        numage %in% c(25:54) ~ "PAGE2",
        numage %in% c(55:64) ~ "PAGE3",
        numage %in% c(65:99) ~ "PAGE4",
        TRUE ~ "PAGE0" # children less than 15 not categorized in demo
      )
    ) %>%
    
    # consolidate men and women
    group_by(GEOID, agecat) %>%
    summarise(count = as.integer(sum(estimate)))
  
  ages
}

#' Get age variables
#' 
#' @param acsvars
#' @param mycounties
#' 
#' This is the household income variable, which is categorized as follows:
#'   - <$15k
#'   - \$15k - \$30k
#'   - \$30k - \$60k
#'   - > $60k
#' 
get_income_controls <- function(acsvars, mycounties){
  incvars <- str_c("B19001_", sprintf("%03d", c(2:17)))
  raw_incs <- get_acs("tract", variables = incvars, state = "UT", county = mycounties)
  
  incs <- raw_incs %>%
    left_join(acsvars, by = c("variable" = "name")) %>%
    separate(label, c("VAR", "total", "income"), sep = "!!") %>%
    select(GEOID, income, estimate)  %>%
    # regroup income categories
    mutate(
      numinc  = stringr::str_extract(income, "\\d+"),
      inccat = case_when(
        numinc <  15 ~ "HHINC1",
        numinc <  30 ~ "HHINC2",
        numinc <  60 ~ "HHINC3",
        numinc >= 60 ~ "HHINC4",
        TRUE ~ as.character(NA)
      )
    ) %>%
    group_by(GEOID, inccat) %>%
    summarise(count = as.integer(sum(estimate)))
  
  incs
}

#' Get age variables
#' 
#' @param mytracts
#' @param ages
#' @param incs
#' @param sizes includes workers
#' 
#' @details  When all of the controls have been gathered, we can put them into one large table.
#'
make_controls <- function(mytracts, ages, incs, sizes){
  tibble(TRACT = mytracts) %>%
     left_join(ages  %>% spread(agecat,  count), by = c("TRACT" = "GEOID")) %>%
     left_join(incs  %>% spread(inccat,  count), by = c("TRACT" = "GEOID")) %>%
     left_join(sizes$works %>% spread(workcat, count), by = c("TRACT" = "GEOID")) %>%
     left_join(sizes$sizes %>% spread(sizecat, count), by = c("TRACT" = "GEOID"))
}

#' make meta information
#' 
#' @param tract_controls
#' PopulationSim requires (we think) at least some region-level controls. We will
#' simply sum up the total population in the controls data to work with this.
get_meta <- function(tract_controls){
  tract_controls %>%
    summarise(
      REGION = 1, 
      totalPOP = sum(PAGE0) +  sum(PAGE1) +  sum(PAGE2) + 
        sum(PAGE3) +  sum(PAGE4))
}

#' A function to replace NA values with non-missing stupid numbers
replace_na <- function(x) {
  ifelse(is.na(x),-8,x)
}

#' Make seed tables from input data
#' 
#' @param hh_seed_file
#' @param pp_seed_file
#' @param crosswalk
#'
#'
make_seed <- function(hh_seed_file, pp_seed_file, crosswalk){
  # read in pums HH file, specifying datatypes for key columns
  pums_hh <- read_csv(
    "inputs/psam_h49.csv.zip",  
    col_types = list(SERIALNO = col_character(), NP = col_integer(), 
                     FINCP = col_number(), ADJINC = col_number(),
                     WGTP = col_number())
  )
  

  seed_hh <- pums_hh %>%
    # remove empty households
    filter(NP > 0) %>% filter(WGTP > 0) %>%
    # remove households from outside the region
    filter(PUMA %in% crosswalk$PUMA) %>%
    mutate(
      # create unique hh_id field
      hh_id = 1:nrow(.),
      # compute adjusted numeric income
      HHINCADJ = FINCP * ADJINC/10^6
    ) %>%
    # apply replacement function to key variables
    mutate_at(.vars = vars(NP, WIF, WGTP, HHINCADJ), replace_na)
  
  
  pums_persons <- read_csv(
    "inputs/psam_p49.csv.zip", 
    col_types = list(SERIALNO = col_character(), PWGTP = col_number())
  )
  
  seed_per <- pums_persons %>%
    # join hhid field, and only keep households we filtered down to.
    inner_join(seed_hh %>% select(SERIALNO, hh_id, WGTP)) %>%
    # replace NA values with something less stupid
    mutate_at(.vars = vars(PWGTP, AGEP), replace_na)
  
  
  list("persons" = seed_per, "households" = seed_hh)
}


#' Write out populationsim files
#' 
#' 
#' @param meta
#' @param tract_controls
#' @param taz_control
#' @param seed
#' @param crosswalk
#' @param path Path to output folder
#'
write_files <- function(meta, tract_controls, taz_control, seed, crosswalk, path){
  dir.create(path)
  
  # Controls
  write_csv(meta, file.path(path, "control_totals_meta.csv"))
  write_csv(tract_controls, file.path(path, "control_totals_tract.csv"))
  write_csv(taz_control, file.path(path, "control_totals_taz.csv"))
  
  # Seed
  write_csv(seed$households, file.path(path, "seed_households.csv"))
  write_csv(seed$persons, file.path(path, "seed_persons.csv"))
 
  # Crosswalk
  write_csv(crosswalk, file.path(path, "geo_cross_walk.csv"))
}
