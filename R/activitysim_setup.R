#' Set up files etc. as needed for ActivitySim run.
#'
#'
#'
setup_asim <- function(se_file, popsim_out_dir, asim_data_dir, taz, skims_file){
  
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
  
  # build_skims(skims_dir, asim_out_dir_data)
  
}


make_asim_persons <- function(popsim_out_dir){ 
  persons <- read_csv(
    file.path(popsim_out_dir, "synthetic_persons.csv"),
    col_types = list(PUMA = col_character(), TRACT = col_character()))
  
  asim_persons <- persons %>%
    # ActivitySim really wants to have sequential person numbers.
    mutate(person_id = row_number(), .before = 1) %>%
    rename(age = AGEP, PNUM = per_num) %>%
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
    )
  
  asim_persons
}


# make_asim_persons <- function(popsim_outputs, taz) {
#   perfile <- file.path(popsim_outputs, "synthetic_persons.csv")
#   persons <- read_csv(perfile, col_types = list(
#     PUMA = col_character(),
#     TRACT = col_character()
#   ))
#   
#   persons %>%
#     
#     # ActivitySim really wants to have sequential person numbers.
#     mutate(person_id = row_number(),
#            PNUM = per_num)%>%
#     rename(age = AGEP) %>%
#     mutate(
#       # create person type variables
#       # ['ptype', 'pemploy', 'pstudent', 'PNUM']
#       ptype = case_when(
#         age >= 18 & SCH == 1 & WKHP >= 30 ~ 1,
#         age >= 18 & SCH == 1 & WKHP > 0 & WKHP < 30 ~ 2,
#         age >= 18 & age < 65 & SCH == 1 & ESR == 3 | age >= 18 & age < 65 & SCH == 1 & ESR == 6 ~ 4,
#         age >= 65 & SCH == 1 & ESR == 3 | age >= 65 & SCH == 1 & ESR == 6 ~ 5,
#         age >= 18 & SCH == 2 | age >= 18 & SCH == 3 ~ 3,
#         age > 15 & age < 18 ~ 6,
#         age > 5 & age < 16 ~ 7,
#         age >= 0 & age < 6 ~ 8
#       ),
#       pstudent = case_when(
#         SCHG >= 2 & SCHG <= 14 ~ 1,
#         SCHG > 14 & SCHG <= 16 ~ 2,
#         T ~ 3
#       ),
#       pemploy = case_when(
#         WKHP >= 30 ~ 1,
#         WKHP > 0 & WKHP < 30 ~ 2,
#         age >= 16 & ESR == 3 | age >= 16 & ESR == 6 ~ 3,
#         T ~ 4
#       ),
#     )  %>%
#     left_join(
#       taz %>% transmute(TAZ = TAZ, asim_taz) %>% st_set_geometry(NULL)
#     ) %>%
#     rename(zone_id = asim_taz, wfrc_taz = TAZ) %>%
#     relocate(zone_id, .before = wfrc_taz)
# }




#' Make Activitysim households file
#' 
#' @param popsim_outputs Folder with popsim outputs
#' @param addressfile Path to address points file from WFRC
#' @param taz SF boundary file for TAZ numbering
#' @param popsim_success 
#' 
#' @details 
#'  ActivitySim requires sequential zone numbering beginning at 1. We have 
#'  kept the zone numbers from the WFRC zones up to this point, with the `taz`
#'  input file holding a list of all WFRC / ASIM ids. It was necessary to keep the
#'  WFRC id's up to this point because 
make_asim_hholds <- function(popsim_outputs, addressfile, taz, popsim_success) {
  hhfile <- file.path(popsim_outputs, "synthetic_households.csv")
  
  # households data table
  hh <- read_csv(hhfile, col_types = list(
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
    select(-ptTAZ)  %>%
    left_join(
      taz %>% transmute(TAZ = as.character(TAZ), asim_taz) %>% st_set_geometry(NULL)
    ) %>%
    rename(zone_id = asim_taz, wfrc_taz = TAZ) %>%
    relocate(zone_id, .before = wfrc_taz)
}

#' Write households and population to activitysim
#' 
#' @param popsim_outputs
#' @param parcelsfile
#' @param taz
#' @param popsim_success Only necessary for targets
#'
#' @details This function does quite a bit of cleaning to move the populationsim
#' outputs over and get them ready for activitysim.
#' 
move_population <- function(asim_persons, asim_hholds, activitysim_inputs){

  asim_persons %>% 
    write_csv(file.path(activitysim_inputs, "synthetic_persons.csv"))
  
  # read households file and append random coordinate =================
  asim_hholds %>%
    write_csv(file.path(activitysim_inputs, "synthetic_households.csv"))
  
  # return path to the households file
  file.path(activitysim_inputs, "synthetic_households.csv")
  
}

mysample <- function(sf, size){
  pts <- st_sample(sf, size)
  
  tibble(
    xcoord = st_coordinates(pts)[, 1],
    ycoord = st_coordinates(pts)[, 2],
  )  
    
}


