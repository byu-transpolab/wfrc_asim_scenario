#' Run populationsim
#' 
run_populationsim <- function(write_result, data_path, out_path){
  # TODO: I couldn't figure out a way to run the python scripts directly from targets.
  # We'll have to work on this later.
  message("You are ready to run populationsim.",  
          "To do this, run the following shell commands:\n \t", 
          "conda activate popsim\n \t",
          "python py/runpopsim.py --config configs_popsim --data ", data_path,  " --output ", out_path)
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
  taz <- st_read(taz_geo) %>% 
    mutate(TAZ = as.character(TAZID)) %>%
    # remove external stations
    filter(CO_FIPS > 0) %>%
    select(TAZ) %>%
    
    # remove taz that do not map to a tract
    st_join(tr) %>% filter(!is.na(PUMA)) %>%
    
    # remove duplicates of TAZ that map to many tracts
    group_by(TAZ) %>% slice(1) %>% ungroup()
  
  # remove TAZs with IVT = 0
  IVT0 <- read_csv(ivt0)
  IVT0$TAZ <- as.character(IVT0$TAZ)
  taz <- taz %>% 
    filter(!TAZ %in% IVT0$TAZ)
  
  taz
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
get_taz_control <- function(taz_control_file, crosswalk){
  read_csv(taz_control_file) %>% 
    transmute(TAZ = as.character(TAZ2), HHBASE = as.integer(ceiling(YEAR2020))) %>% 
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
