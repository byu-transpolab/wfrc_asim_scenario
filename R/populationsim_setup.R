#' Set up and write out input data files for PopulationSim
#' 
setup_popsim <- function(se_taz, out_dir_data, meta, tract_controls, seed, crosswalk){
  
  if(!dir.exists(out_dir_data)) dir.create(out_dir_data, recursive = TRUE)
  # Make popsim output dir
  dir_output <-
    str_replace(out_dir_data, "^populationsim/data/", "populationsim/output/")
  if((!dir_output == out_dir_data) && !dir.exists(dir_output)){
    dir.create(dir_output, recursive = TRUE)
  }
  
  # Clean taz_control data
  # NOTE: The only TAZ-level control we get from WFRC is the number of
  # households in each TAZ. We get this from the travel model SE file
  taz_control <- get_taz_control(se_taz, crosswalk)
  
  # Write files
  # Controls
  write_csv(meta, file.path(out_dir_data, "control_totals_meta.csv"))
  write_csv(tract_controls, file.path(out_dir_data, "control_totals_tract.csv"))
  write_csv(taz_control, file.path(out_dir_data, "control_totals_taz.csv"))
  
  # Seed
  write_csv(seed$households, file.path(out_dir_data, "seed_households.csv"))
  write_csv(seed$persons, file.path(out_dir_data, "seed_persons.csv"))
  
  # Crosswalk
  write_csv(crosswalk, file.path(out_dir_data, "geo_cross_walk.csv"))
  
  out_dir_data
  
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
    mutate_at(.vars = vars(NP, WIF, WGTP, HHINCADJ), \(x) ifelse(is.na(x), -8, x))
  
  
  pums_persons <- read_csv(
    pp_seed_file, 
    col_types = list(SERIALNO = col_character(), PWGTP = col_number())
  )
  
  seed_per <- pums_persons %>%
    # join hhid field, and only keep households we filtered down to.
    inner_join(seed_hh %>% select(SERIALNO, hh_id, WGTP)) %>%
    # replace NA values with something less stupid
    mutate_at(.vars = vars(PWGTP, AGEP), \(x) ifelse(is.na(x), -8, x))
  
  
  list("persons" = seed_per, "households" = seed_hh)
}
