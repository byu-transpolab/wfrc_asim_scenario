#' Run populationsim
#' 
run_populationsim <- function(write_result, data_path, out_path){
  popsimStatus <- system2(
    command = "bash",
    args = c("./sh/run_popsim.sh", data_path, out_path)
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
get_tracts <- function(st_fips, puma_tract, crs = 4326){
  tigris::tracts(st_fips, county = unique(substr(puma_tract$TRACT, 3, 5)), 
         class = "sf", progress_bar = FALSE, year = 2019) %>%
    st_transform(crs) %>%
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
get_taz <- function(taz_geo, tr, ivt0 = NULL){
  
  # WFRC has a lot of zones, including zones that are uninhabited and zones
  # that are external to the region.
  taz <- st_read(taz_geo)
  
  if ("EMPTY" %in% colnames(taz)){
    taz <- taz %>% 
      filter(!EMPTY)
  }
  if (!is.null(ivt0)){
    exclude <- read_lines(ivt0)
    taz <- taz %>% 
      filter(!TAZID %in% exclude)
  }
  
  clean_taz <-
    taz %>% 
    transmute(
      TAZ = TAZID,
      ACRES,
      DEVACRES,
      DISTRICT = DISTLRG,
      SD = DISTSML
    )
  
  
  # each taz needs to belong to at least one tract / PUMA
  tigris_taz <- clean_taz %>%
    # remove taz that do not map to a tract
    st_join(tr) %>% 
    filter(!is.na(PUMA)) %>%
    # remove duplicates of TAZ that map to many tracts
    group_by(TAZ) %>% slice(1) %>%  ungroup()
  
  # after removing these TAZ, we have some TAZs left
  final_taz <- tigris_taz %>% 
    arrange(TAZ) %>%
    mutate(
      asim_taz = row_number(),
      .after = TAZ
    )
    
  final_taz
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
    read_csv() %>% 
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
  raw_sw <- get_acs("tract", variables = swvars, state = "UT", county = mycounties, 
                    year = 2019)
  
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
  raw_ages <- get_acs("tract", variables = agevars, state = "UT", county = mycounties, year = 2019)
  
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
  raw_incs <- get_acs("tract", variables = incvars, state = "UT", county = mycounties, year = 2019)
  
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
    hh_seed_file,  
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
    pp_seed_file, 
    col_types = list(SERIALNO = col_character(), PWGTP = col_number())
  )
  
  seed_per <- pums_persons %>%
    # join hhid field, and only keep households we filtered down to.
    inner_join(seed_hh %>% select(SERIALNO, hh_id, WGTP)) %>%
    # replace NA values with something less stupid
    mutate_at(.vars = vars(PWGTP, AGEP), replace_na)
  
  
  list("persons" = seed_per, "households" = seed_hh)
}


#' Set up and write out input data files for PopulationSim
#' 
setup_popsim <- function(se_taz, out_dir, meta, tract_controls, seed, crosswalk){
  
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Clean taz_control data
  # NOTE: The only TAZ-level control we get from WFRC is the number of
  # households in each TAZ. We get this from the travel model SE file
  taz_control <- get_taz_control(se_taz, crosswalk)
  
  # Write files
  # Controls
  write_csv(meta, file.path(out_dir, "control_totals_meta.csv"))
  write_csv(tract_controls, file.path(out_dir, "control_totals_tract.csv"))
  write_csv(taz_control, file.path(out_dir, "control_totals_taz.csv"))
  
  # Seed
  write_csv(seed$households, file.path(out_dir, "seed_households.csv"))
  write_csv(seed$persons, file.path(out_dir, "seed_persons.csv"))
  
  # Crosswalk
  write_csv(crosswalk, file.path(out_dir, "geo_cross_walk.csv"))
  
}