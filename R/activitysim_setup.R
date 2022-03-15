#' Run activitysim scenario
#' 
#' @param data_path
#' @param config_path
#' @param output_path
#' 
run_activitysim <- function(data_path, config_path, output_path, ...){
  # TODO: I couldn't figure out a way to run the python scripts directly from targets.
  # We'll have to work on this later.
  message("You are ready to run activitysim.",  
          "To do this, run the following shell commands:\n \t", 
          "conda activate ASIM_DEV\n \t",
          "activitysim run --config ", config_path, " --data ", data_path,  " --output ", output_path)
  
  return(TRUE)
}





#' Make land use table
#' 
#' 
#' @param se SE file from WFRC model
#' @param perdata Person data gleaned from from synthetic pop
#' @param hhdata HH data gleaned from synthetic pop
#' @param urbanization urbanization data
#' @param buildings buildings data
#' @param topo topography data
#' @param schools college enrollment data (HS is in se)
#' @param taz TAZ geographic boundary layer
#' 
#'  The ActivitySim Model requires an input of specific socioeconomic (SE) data
#  for certain counties in the Wasatch Front (Box Elder, Weber, Davis, Salt Lake,
#  Utah). This data can also be classified as landuse data. This document
#  explains what data is needed, where the data comes from, and how the data is
#  changed into the correct format in order to be processed.
#  
#  The socioeconomic file requires 42 different inputs which can be described as
#  different column headings. These headings are copied directly from the
#  ActivitySim Example's input file. The headings are the following:
#  
#  > ##### ZONE, DISTRICT, SD, COUNTY, TOTHH, HHPOP, TOTPOP, EMPRES, SFDU, MFDU,
#  HHINCQ1, HHINCQ2, HHINCQ3, HHINCQ4, TOTACRE, RESACRE, CIACRE, SHPOP62P,
#  TOTEMP, AGE0004, AGE0519, AGE2044, AGE4564, AGE65P, RETEMPN, FPSEMPN, HEREMPN,
#  OTHEMPN, AGREMPN, MWTEMPN, PRKCST, OPRKCST, area_type, HSENROLL, COLLFTE,
#  COLLPTE, TOPOLOGY, TERMINAL, ZERO, hhlds, sftaz, gqpop, geometry
#  
#  The description for each column heading can be found at this link:
#  https://github.com/BayAreaMetro/modeling-website/wiki/TazData
#  
#  The next section describes the data input sources, and then the rest of the
#  page describes how each of the 42 sections are calculated using the data
#  sources. A final table is created at the end. Specifically, the final results
#  are presented as a csv file named land_use.csv.

#' 
make_land_use <- function(se, perdata, hhdata, urbanization, buildings, topo, schools, taz){
  se %>%
    inner_join(taz %>% mutate(zone_id = as.character(TAZ)), 
               by = "zone_id")  %>%
    left_join(perdata, by = "zone_id") %>%
    left_join(hhdata, by = "zone_id") %>%
    left_join(urbanization, by = "zone_id") %>%
    left_join(buildings, by = "zone_id") %>%
    left_join(topo, by = "zone_id") %>%
    left_join(schools, by = "zone_id") %>%
    # relocate columns for my own sanity
    transmute(
      zone_id = asim_taz,
      wfrc_taz,
      DISTRICT, SD,
      TOTHH, HHPOP, TOTPOP,
      EMPRES, SFDU, MFDU, HHINCQ1, HHINCQ2, HHINCQ3, HHINCQ4, 
      TOTACRE, CIACRE, 
      RESACRE = case_when(
        RESACRE == 0 ~ 1,
        T ~ RESACRE
      ), SHPOP62P,
      TOTEMP, AGE0004, AGE0519, AGE2044, AGE4564, AGE65P, RETEMPN, FPSEMPN, HEREMPN,
      OTHEMPN, AGREMPN, MWTEMPN, PRKCST, OPRKCST, area_type, HSENROLL, COLLFTE,
      COLLPTE, TOPOLOGY, TERMINAL, gqpop = 0, geometry
    )  
    

    
}

#' Turn NA values to zeros, make integer
#' 
#' @param x variable
#'
na_int <- function(x){
  ifelse(is.na(x), 0, as.integer(x))
}

#' Write out land use csv file
#' 
#' @param land_use
#' @param file
#' 
#' @details converts the geometry sf column to WKT geometry 
#' required by activitysim's output file writer.
#' 
write_land_use <- function(land_use, file){
  dir.create(dirname(file))
  
  land_use %>%
    mutate(geometry = st_as_text(geometry)) %>% 
    write_csv(file)
  
  file
}


#' Read SE data for WFRC region
#' 
#' @param se_wfrc path to model input se file
#' @param se_boxelder path to model input file for box elder county, which 
#' is inexplicably a different file
#' 
#' @details In order to put together all 42 columns of input data, the data must
#'   be collected from various sources. The data sources are as follows: the
#'   2018 SE WFRC data files, the synthetic population files, the buildings,
#'   parcel, and urbanization datasets from WFRC, the TAZ shapefile data,
#'   topology data from the AGRC website joint with the TAZ shapefile, and
#'   school data from the AGRC website joint with the TAZ shapefile.
#'
#' For the WFRC SE data files, data from Box Elder is only used for Zones 1-135.
#' The rest of the data is used from the SE 2018 file (which includes the other
#' counties, but excludes Box Elder).
#' 
read_sedata <- function(se_wfrc, se_boxelder){
  #wfrc Box Elder Se Data, extract needed rows
  wfrc <- read_csv(se_wfrc) %>%
    rename(zone_id = `;TAZID`, TOTEMP = ALLEMP, HSENROLL = Enrol_High)
  
  boxelder <- read_csv(se_boxelder) %>%
    rename(zone_id = Index, TOTEMP = ALLEMP, HSENROLL = Enrol_High) %>%
    filter(! (zone_id %in% wfrc$zone_id))
  
  bind_rows(boxelder, wfrc) %>%
    mutate(zone_id = as.character(zone_id)) %>%
    transmute(
      zone_id,
      TOTPOP,
      TOTEMP,
      # RETL,retail jobs
      # FOOD,food/accommodation jobs
      # MANU,manufacturing jobs
      # WSLE,wholesale/transportation jobs
      # OFFI,office jobs
      # GVED,government/education jobs
      # HLTH,health care jobs
      # OTHR,other jobs
      # FM_AGRI,agriculture jobs
      # FM_MING,mining jobs
      # FM_CONS,construction jobs
      # HBJ,home-based jobs
      RETEMPN = RETL,
      FPSEMPN = OFFI,
      HEREMPN = HLTH + GVED + FOOD,
      OTHEMPN = OTHR + HBJ + FM_CONS,
      AGREMPN = FM_AGRI + FM_MING,
      MWTEMPN = MANU + WSLE,
      HSENROLL,
    ) %>%
    mutate(
      across(-zone_id, na_int)
    )
}

#' Read persons data
#' 
#' @param popsim_outputs
#' @param popsim_success
#'
#' @details The number of employed residents is determined by the data in the
#'   synthetic population persons file. If the COW (class of worker) categoty
#'   isn't empty, then the person is assumed to be employed.
#'   The data for the age grouping columns are determined from the
#'   persons/population table within the synthetic population data. The persons
#'   are separated and totaled into specific age ranges.
#'   
read_perdata <- function(popsim_outputs, popsim_success){
  
  pd <- read_csv(file.path(popsim_outputs, "synthetic_persons.csv")) %>%
    mutate(
      zone_id = as.character(TAZ),
      EMPRES = ifelse(COW %in% 1:9, 1, 0)
    ) %>%
    mutate(
      HHPOP = 1,
      AGE = ifelse(AGEP %in% 0:4, 1, ifelse(AGEP %in% 5:19,2,
                                            ifelse(AGEP %in% 20:44, 3, ifelse(AGEP %in% 45:64, 4, 5)))),
      AGE2 = ifelse(AGEP >= 62, 1, 0),
      AGE0004 = ifelse(AGE == 1, 1,0),
      AGE0519 = ifelse(AGE == 2, 1,0),
      AGE2044 = ifelse(AGE == 3, 1,0),
      AGE4564 = ifelse(AGE == 4, 1,0),
      AGE65P = ifelse(AGE == 5, 1,0),
      AGE62P = ifelse(AGE2 == 1, 1,0),
    )
  
  emps <- pd %>%
    select(zone_id, EMPRES) %>%
    group_by(zone_id) %>%
    summarise(EMPRES = sum(EMPRES))
  
  ages <- pd %>%
    select(zone_id, HHPOP, AGE0004, AGE0519, AGE2044, AGE4564, AGE65P, AGE62P) %>%
    group_by(zone_id) %>%
    summarize_all("sum") %>%
    mutate(
      SHPOP62P = AGE62P/(AGE0004+AGE0519+AGE2044+AGE4564+AGE65P)
    )
  
  
  left_join(emps, ages, by = "zone_id") %>%
    mutate(across(-zone_id, na_int))
}


#' Read household data
#' 
#' @param popsim_outputs
#' @param popsim_success
#'
#' @details The total household (TOTHH) and total population (TOTPOP) data comes
#'   from the 2018 SE WFRC data. Household population (HHPOP) is a funciton of
#'   household size (HHSIZE), which is also found in the same data set.
#'   
#'  
read_hhdata <- function(popsim_outputs, popsim_success){
  #create a table of the household and total population data from the wfrc data
  read_csv(file.path(popsim_outputs, "synthetic_households.csv")) %>%
    mutate(
      zone_id = as.character(TAZ),
      TOTHH = 1,
      inc = HHINCADJ/(10^6),
      money = ifelse(inc < 30000, 1, ifelse(inc >= 30000 & inc < 60000, 2, 
                                            ifelse(inc >= 60000 & inc < 100000, 3, 4))),
      HHINCQ1 = ifelse(money == 1, 1, 0),
      HHINCQ2 = ifelse(money == 2, 1, 0),
      HHINCQ3 = ifelse(money == 3, 1, 0),
      HHINCQ4 = ifelse(money == 4, 1, 0)
    ) %>%
    select(zone_id, TOTHH, HHINCQ1, HHINCQ2, HHINCQ3, HHINCQ4) %>%
    
    #group the data by ZONE and sum all the values to get the total counts for each ZONE
    group_by(zone_id) %>%
    summarize_all("sum") %>%
    mutate(across(-zone_id, na_int))
  
  
}


read_urbanization <- function(urbanfile){
  
  read_csv(urbanfile) %>%
    transmute(
      zone_id = as.character(num),
      TOTACRE = ACRES,
      PRKCST = PRKCSTPERM * 100,
      OPRKCST = PRKCSTTEMP * 100,
      area_type = ATYPE,
      TERMINAL = TERMTIME
    )
  
}

make_buildings <- function(buildfile, parcelsfile){
  pc <- read_csv(parcelsfile) %>% rename()
  bd <- read_csv(buildfile)
  
  
  left_join(bd, pc, by = 'parcel_id') %>%
    mutate(
      zone_id = as.character(zone_id),
      SFDU = ifelse(building_type_id == 1, residential_units, 0),
      MFDU = ifelse(building_type_id == 2, residential_units, 0),
      RESACRE = ifelse(building_type_id == 1, parcel_acres, 
                       ifelse(building_type_id == 2, parcel_acres, 0)),
      CIACRE = ifelse(building_type_id == 3, parcel_acres, 
                      ifelse(building_type_id == 4, parcel_acres, 
                             ifelse(building_type_id == 5, parcel_acres, 0)))
    ) %>% 
    select(zone_id, SFDU, MFDU, RESACRE, CIACRE) %>%
    group_by(zone_id) %>%
    summarize_all("sum") %>%
    mutate( across(c(SFDU, MFDU), na_int) )
  
}

#' Make topography dataset
#' @param topofile
#'
#' @details
#' Utah topology information comes from the AGRC website. Elevations for each
#' Zone was found by joining the utah elevation shapefile, located on the
#' website, with the TAZ shapefile within QGIS.
#'
#' https://gis.utah.gov/data/elevation-and-terrain/contours/
#'  (Note: This elevation shapefile measures elevations in multiples of 200 ft)
#'
#' A table was made from joining the TAZ and Contour shapefiles, and the various
#' elevations for each range are displayed within the table. The table below
#' shows the relationship between the difference in elevation in regards to its
#' steepness.
#' 
#'   | Change in Elevation (ft) | Topology/Steepness |
#'   | ------------------------ | ------------------ |
#'   | 0                        | 1 - Flat           |
#'   | < 600                    | 2 - In Between     |
#'   | >= 600                   | 3 - Steep          |
#'   
#'   Note: I decided that  a change of 600ft or more in elevation is steep, so if that is incorrect, it must be changed.
make_topo <- function(topofile){
  
  read_csv(topofile) %>%
    transmute(zone_id = as.character(ZONE), ContourEle) %>%
    group_by(zone_id) %>%
    summarize(
      mi = min(ContourEle),
      ma = max(ContourEle)
    ) %>%
    mutate(
      TOPOLOGY = ifelse((ma -mi) == 0, 1, ifelse((ma - mi) < 600, 2, 3))
    ) %>%
    select(zone_id, TOPOLOGY)
}

#' Make college enrollment data
#' 
#' @param schoolfile
#' 
#' @details College data was determined from a variety of sources. First, a
#'   joined table using the TAZ shapefile and a schools shapefile from the AGRC
#'   website was made on QGIS. The website of the schools shapefile is found
#'   here:
#' 
#' https://gis.utah.gov/data/society/schools-libraries/
#'   
#'   This table was then edited to include information from the web on full time
#'   and part time enrollments for many of the "Higher Education" schools
#'   listed. The websites for these can be seen in the data source column of
#'   that table. Some data wasn't found on the web, so data from WFRC's college
#'   information was used. The WFRC information is shown in the table below:
#'  WFRC only had information on total enrolled students, so part time students
#'  for these colleges is represented as 0, even if that is not necessarily
#'  true. This is why data from the web was prioritized over the WFRC
#'  information (it had part time student data). Also, not all the "Higher
#'  Education" schools from the table have completed information; a deeper
#'  search on the web could find these potential numbers. The majority of the
#'  colleges have information though, and so we use the data that we do have.
#'
#'
make_schools <- function(schoolfile){
  colleges <- read_csv("inputs/other/schools.csv", col_types = list(
    TAZID = col_character()
  ) ) %>%
    rename(zone_id = TAZID) %>%
    mutate(
      COLLFTE = ifelse(EDTYPE == "Higher Education", ENROLL_FT, 0),
      COLLPTE = ifelse(EDTYPE == "Higher Education", ENROLL_PT, 0),
    ) %>%
    select(zone_id, COLLFTE, COLLPTE) %>%
    mutate( across(c(COLLFTE, COLLPTE), na_int) )
  
  colleges[!duplicated(colleges$zone_id), ]
}


#' Make persons file for activitysim
#' 
#' @param popsim_outputs
#' 
make_asim_persons <- function(popsim_outputs, popsim_success, taz) {
  perfile <- file.path(popsim_outputs, "synthetic_persons.csv")
  persons <- read_csv(perfile, col_types = list(
    PUMA = col_character(),
    TRACT = col_character()
  )) 
  
  persons %>%
    
    # ActivitySim really wants to have sequential person numbers.
    mutate(person_id = row_number(),
           PNUM = person_id)%>%
    rename(age = AGEP) %>%
    mutate(
      # create person type variables
      # ['ptype', 'pemploy', 'pstudent', 'PNUM']
      ptype = case_when(
        age >= 18 & SCH == 1 & WKHP >= 30 ~ 1,
        age >= 18 & SCH == 1 & WKHP > 0 & WKHP < 30 ~ 2,
        age >= 18 & age < 65 & SCH == 1 & ESR == 3 | age >= 18 & age < 65 & SCH == 1 & ESR == 6 ~ 4,
        age >= 65 & SCH == 1 & ESR == 3 | age >= 65 & SCH == 1 & ESR == 6 ~ 5,
        age >= 18 & SCH == 2 | age >= 18 & SCH == 3 ~ 3,
        age > 15 & age < 18 ~ 6,
        age > 5 & age < 16 ~ 7,
        age >= 0 & age < 6 ~ 8
      ),
      pstudent = case_when(
        SCHG >= 2 & SCHG <= 14 ~ 1,
        SCHG > 14 & SCHG <= 16 ~ 2,
        T ~ 3
      ),
      pemploy = case_when(
        WKHP >= 30 ~ 1,
        WKHP > 0 & WKHP < 30 ~ 2,
        age >= 16 & ESR == 3 | age >= 16 & ESR == 6 ~ 3,
        T ~ 4
      ),
    )  
}




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
      taz %>% transmute(TAZ = as.character(TAZ), wfrc_taz, asim_taz) %>% st_set_geometry(NULL)
    )
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
  
  # create directory
  dir.create(activitysim_inputs)
  
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



