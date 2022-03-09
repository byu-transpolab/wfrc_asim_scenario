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
    left_join(perdata, by = "zone_id") %>%
    left_join(hhdata, by = "zone_id") %>%
    left_join(urbanization, by = "zone_id") %>%
    left_join(buildings, by = "zone_id") %>%
    left_join(topo, by = "zone_id") %>%
    left_join(schools, by = "zone_id") %>%
    left_join(taz, by = c("zone_id" = "TAZ")) %>%
    mutate(
      gqpop = TOTPOP - HHPOP
    )
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
    mutate(
      RETEMPN = RETL,
      FPSEMPN = OFFI,
      HEREMPN = HLTH + GVED + FOOD,
      OTHEMPN = OTHR + HBJ + FM_CONS,
      AGREMPN = FM_AGRI + FM_MING,
      MWTEMPN = MANU + WSLE,
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
    select(zone_id, AGE0004, AGE0519, AGE2044, AGE4564, AGE65P, AGE62P) %>%
    group_by(zone_id) %>%
    summarize_all("sum") %>%
    mutate(
      SHPOP62P = AGE62P/(AGE0004+AGE0519+AGE2044+AGE4564+AGE65P)
    )
  
  
  left_join(emps, ages, by = "zone_id")
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
      inc = HHINCADJ/(10^6),
      money = ifelse(inc < 30000, 1, ifelse(inc >= 30000 & inc < 60000, 2, 
                                            ifelse(inc >= 60000 & inc < 100000, 3, 4))),
      HHINCQ1 = ifelse(money == 1, 1, 0),
      HHINCQ2 = ifelse(money == 2, 1, 0),
      HHINCQ3 = ifelse(money == 3, 1, 0),
      HHINCQ4 = ifelse(money == 4, 1, 0)
    ) %>%
    select(zone_id, HHINCQ1, HHINCQ2, HHINCQ3, HHINCQ4) %>%
    
    #group the data by ZONE and sum all the values to get the total counts for each ZONE
    group_by(zone_id) %>%
    summarize_all("sum")
  
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
      MFDU = ifelse(building_type_id == 2, residential_units, 0)
    ) %>% 
    select(zone_id, SFDU, MFDU) %>%
    group_by(zone_id) %>%
    summarize_all("sum")
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
      COLLFTE = ifelse(is.na(COLLFTE), 0, COLLFTE),
      COLLPTE = ifelse(is.na(COLLPTE), 0, COLLPTE),
    ) %>%
    select(zone_id, COLLFTE, COLLPTE) 
  
  colleges[!duplicated(colleges$zone_id), ]
}
