#' Set up files etc. as needed for ActivitySim run.
#'
#'
#'
setup_asim <- function(se_file, popsim_out_dir, asim_data_dir, taz, skims_file, ...){
  
  if(!dir.exists(asim_data_dir)) dir.create(asim_data_dir, recursive = TRUE)
  
  dir_output <-
    str_replace(asim_data_dir, "^activitysim/data/", "activitysim/output/")
  if((!dir_output == asim_data_dir) && !dir.exists(dir_output)){
    dir.create(dir_output, recursive = TRUE)
  }
  
  land_use <- make_land_use(se_file, popsim_out_dir, taz, out_dir = asim_data_dir) 
  land_use %>% 
    mutate(geometry = st_as_text(geometry)) %>% 
    write_csv(file.path(asim_data_dir, "land_use.csv"))
  
  #Put skims in the right place and with the right name
  file.copy(skims_file, file.path(asim_data_dir, "skims.omx"), overwrite = TRUE)
  
  #Make/write asim persons and households
  write_csv(
    make_asim_persons(popsim_out_dir),
    file.path(asim_data_dir, "synthetic_persons.csv")
  )

  write_csv(
    make_asim_households(popsim_out_dir, "data/AddressCoordinates.csv", taz),
    file.path(asim_data_dir, "synthetic_households.csv")
  )
  
  asim_data_dir
}


make_asim_persons <- function(popsim_out_dir){ 
  persons <- read_csv(
    file.path(popsim_out_dir, "synthetic_persons.csv"),
    col_types = list(PUMA = col_character(), TRACT = col_character()))
  
  asim_persons <- persons %>%
    # ActivitySim really wants to have sequential person numbers.
    mutate(person_id = row_number(), .before = 1) %>%
    rename(age = AGEP, PNUM = per_num) %>%
    mutate(across(c(age, SCH, WKHP, ESR, SCHG), as.numeric)) %>% 
    mutate(
      # create person type variables
      # ['ptype', 'pemploy', 'pstudent']
      ptype = case_when(
        age >= 18 & SCH == 1 & WKHP >= 30 ~ 1,
        age >= 18 & SCH == 1 & WKHP > 0 & WKHP < 30 ~ 2,
        age >= 18 & SCH %in% c(2,3) ~ 3,
        age >= 18 & age < 65 & SCH == 1 & ESR %in% c(3,6) ~ 4,
        age >= 65 & SCH == 1 & ESR %in% c(3,6) ~ 5,
        age > 15 & age < 18 ~ 6,
        age > 5 & age < 16 ~ 7,
        age >= 0 & age < 6 ~ 8
      ),
      pstudent = case_when(
        SCHG >= 2 & SCHG <= 14 ~ 1,
        SCHG > 14 & SCHG <= 16 ~ 2,
        T ~ 3),
      pemploy = case_when(
        WKHP >= 30 ~ 1,
        WKHP > 0 & WKHP < 30 ~ 2,
        age >= 16 & ESR %in% c(3,6) ~ 3,
        T ~ 4
      ),
      pjobcat = case_when(
        OCCP == 0 ~ "none",
        OCCP < 2200 ~ "OFFI",
        OCCP < 2600 ~ "GVED",
        OCCP < 3000 ~ "OTHR",
        OCCP < 3700 ~ "HLTH",
        OCCP < 4000 ~ "GVED",
        OCCP < 4200 ~ "FOOD",
        OCCP < 4700 ~ "OTHR",
        OCCP < 5000 ~ "RETL",
        OCCP < 6000 ~ "GVED",
        OCCP < 6200 ~ "AGRI",
        OCCP < 6800 ~ "CONS",
        OCCP < 7000 ~ "MING",
        OCCP < 7700 ~ "OTHR",
        OCCP < 9000 ~ "MANU",
        OCCP < 10000 ~ "OTHR",
        TRUE ~ "none"
      )
    )
  
  asim_persons %>%
    mutate(
      across(!where(is.character), \(x) replace_na(x,0))
    )
}


#' Make Activitysim households file
#' 
#' @param popsim_outputs Folder with popsim outputs
#' @param addressfile Path to address points file from WFRC
#' @param taz SF boundary file for TAZ numbering
#' 
#' @details 
#'  ActivitySim requires sequential zone numbering beginning at 1. We have 
#'  kept the zone numbers from the WFRC zones up to this point, with the `taz`
#'  input file holding a list of all WFRC / ASIM ids. It was necessary to keep the
#'  WFRC id's up to this point because 
make_asim_households <- function(popsim_out_dir, addressfile, taz) {
  hh <- read_csv(
    file.path(popsim_out_dir, "synthetic_households.csv"),
    col_types = list(
      household_id = col_character(),
      PUMA = col_character(),
      TRACT = col_character(),
      TAZ = col_character(),
      NP = col_integer(),
      WIF = col_integer(),
      HHT = col_integer(),
      VEH = col_integer()
    )) %>%
    arrange(TAZ)
  
  # Addresses from WFRC 
  addresses <- read_csv(addressfile, col_types = list(TAZID = col_character()))  %>%
    filter(!is.na(xcoord)) %>%
    mutate(TAZ = as.character(TAZID)) 
  
  # how many households do we need per zone?
  n_hh <- hh %>%
    group_by(TAZ) %>%
    summarise(n_hh = n())
  
  # how many properties are in each zone?
  n_adr <- addresses %>%
    group_by(TAZ) %>%
    summarise(n_adr = n())
  
  
  # Generate random points in polygon for zones without addresses -------------
  # which zones have households but no addresses?
  random_points <- taz %>%
    mutate(TAZ = as.character(TAZ)) %>%
    group_by(TAZ) %>%
    nest() %>%
    left_join(n_hh) %>%
    left_join(n_adr)  %>%
    filter(!is.na(n_hh)) %>%
    filter(is.na(n_adr)) %>%
    mutate(
      points = map(data, mysample, size = n_hh)
    )
  
  
  # Sample random addresses in polygon for zones with addresses ---------------
  random_addresses <- addresses %>% 
    select(TAZ = TAZID, xcoord, ycoord) %>%
    group_by(TAZ) %>%
    nest() %>%
    inner_join(n_hh)  %>%
    mutate(
      points = map(data, slice_sample, n = n_hh, replace = T)
    )
  
  # bind random points together, join to hh, and write out -----------------
  allpts <- bind_rows(
    random_addresses %>%
      select(TAZ, points) %>%
      unnest(cols = c(points)),
    random_points %>%
      select(TAZ, points) %>%
      unnest(cols = c(points))
  ) %>%
    arrange(TAZ) %>%
    rename(home_x = xcoord, home_y = ycoord, ptTAZ = TAZ)
  
  
  out_hh <- bind_cols(hh, allpts)
  
  
  # output checks
  #  are there households where the TAZ and TAZ of the random point are different?
  if(nrow(filter(out_hh, TAZ != ptTAZ)) !=0 ) {
    stop("Some points have been assigned a home address outside their TAZ")
  } 
  
  
  out_hh %>%
    select(-ptTAZ) %>% 
    relocate(household_id, TAZ) %>% 
    arrange(as.integer(household_id)) %>%
    mutate(
      across(!where(is.character), \(x) replace_na(x,0))
    )
}


mysample <- function(sf, size){
  pts <- st_sample(sf, size)
  
  tibble(
    xcoord = st_coordinates(pts)[, 1],
    ycoord = st_coordinates(pts)[, 2],
  )  
    
}

bind_population <- function(base_dir, diff_dir, out_dir = diff_dir) {
  base_hh <- read_csv(file.path(base_dir, "synthetic_households.csv")) %>% 
    arrange(household_id)
  base_per <- read_csv(file.path(base_dir, "synthetic_persons.csv"))
  
  ### Don't do this because the new population will overwrite the diff
  ### Do it manually
  # if(file.exists(file.path(diff_dir, "synthetic_persons.csv"))) file.rename(
  #   file.path(diff_dir, "synthetic_persons.csv"),
  #   file.path(diff_dir, "synthetic_persons_diff.csv"))
  # if(file.exists(file.path(diff_dir, "synthetic_households.csv"))) file.rename(
  #   file.path(diff_dir, "synthetic_households.csv"),
  #   file.path(diff_dir, "synthetic_households_diff.csv"))
  
  # The files will need to be renamed manually (for now) to avoid accidental overwriting
  diff_hh <- read_csv(file.path(diff_dir, "synthetic_households_diff.csv")) %>% 
    arrange(household_id)
  diff_per <- read_csv(file.path(diff_dir, "synthetic_persons_diff.csv"))
  
  hh_bound <- bind_rows(base = base_hh, diff = diff_hh, .id = "source") %>%
    mutate(new_household_id = 1:nrow(.))
  
  #Check in case base hh_ids aren't sequential
  #### TODO: add handling of non-sequential IDs
  if(any(
    filter(hh_bound, source == "base")$new_household_id
    != filter(hh_bound, source == "base")$household_id
    )) stop("Misarranged household IDs in combined file")
  
  #translation table from "diff" hh_ids to unique ones
  hh_trans <- hh_bound %>% 
    filter(household_id != new_household_id) %>% 
    select(household_id, new_household_id) %>% 
    mutate(source = "diff")
  
  per_bound <- bind_rows(base = base_per, diff = diff_per, .id = "source") %>% 
    left_join(hh_trans, join_by(source, household_id)) %>% 
    mutate(new_household_id = case_when(
      is.na(new_household_id) ~ household_id,
      TRUE ~ new_household_id
    ))
  
  
  hh <- hh_bound %>% 
    select(-c(source, household_id)) %>% 
    rename(household_id = new_household_id) %>% 
    relocate(household_id)
    
  per <- per_bound %>% 
    select(-c(source, household_id)) %>% 
    rename(household_id = new_household_id) %>% 
    relocate(household_id, .before = per_num)
  
  write_csv(hh, file.path(out_dir, "synthetic_households.csv"))
  write_csv(per, file.path(out_dir, "synthetic_persons.csv"))
  
}
