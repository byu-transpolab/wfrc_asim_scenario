#!/usr/bin/env Rscript

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, sf)

###################################################################

activitysim_output_dir <- "output_activitysim"
beam_files_dir <- "data_beam"

# Read in csvs
persons <- read_csv(paste0(activitysim_output_dir, "/final_persons.csv"))
hh <- read_csv(paste0(activitysim_output_dir, "/final_households.csv"))
plans <- read_csv(paste0(activitysim_output_dir, "/final_plans.csv"))
trips <- read_csv(paste0(activitysim_output_dir, "/final_trips.csv"))


#housing type for hh attributes file
hType <- "House"


#### Fix colnames ##################################################

persons %<>% 
  rename(personId = person_id,
         householdId = household_id,
         isFemale = female,
         valueOfTime = value_of_time)

hh %<>% 
  rename(householdId = household_id,
         incomeValue = income,
         locationX = home_x,
         locationY = home_y, 
         TAZ = home_zone_id)

plans %<>% 
  rename(personId = person_id,
         planElementType = ActivityElement,
         planElementIndex = PlanElementIndex,
         activityType = ActivityType,
         activityLocationX = x,
         activityLocationY = y,
         activityEndTime = departure_time,
         legMode = trip_mode)

trips %<>%
  rename(primaryPurpose = primary_purpose)


#### Households ####################################################

#convert coords and add auto_work_ratio variable to hh
hh %<>%
  select(householdId, TAZ, incomeValue, hhsize, auto_ownership, num_workers,
         locationX, locationY) %>% 
  #convert WGS84 coords to UTM 12N
  st_as_sf(coords = c("locationX", "locationY")) %>%
  `st_crs<-`(4326) %>% #WGS84
  st_transform(26912) %>% #UTM 12N
  {mutate(.,
          locationX = unlist(map(.$geometry,1)),
          locationY = unlist(map(.$geometry,2))
  )} %>%
  as_tibble() %>%
  select(-geometry) %>%
  #add auto_work_ratio
  mutate(num_workers = ifelse(num_workers == -8, 0, num_workers),
         autoWorkRatio =
           case_when(auto_ownership == 0 ~ "no_auto",
                     auto_ownership / num_workers < 1 ~ "auto_deficient",
                     auto_ownership / num_workers >= 1 ~ "auto_sufficient",
                     #if num_workers is 0, R will return 'Inf', which is > 1
                     T ~ "we messed up, check asim to beam script"))

#create hh attribute file
hhattr <- hh %>% 
  select(householdId, locationX, locationY) %>% 
  mutate(housingType = hType)



#### Persons #########################################################

persons %<>% 
  select(personId, householdId, age, sex, isFemale, valueOfTime) %>% 
  left_join(hh, by = "householdId") %>% 
  rename(income = incomeValue) #needs to be `incomeValue` in hh, but `income` in persons


#### Plans #########################################################

#fix person ids
plans$personId %<>% as.integer()

#planIndex is needed for BEAM, but only applies if there are multiple
#potential plans. Our plans file only has one potential plan.
plans %<>% mutate(planIndex = 0)

#add primaryPurpose from trips to plans
trips %<>% select(trip_id, primaryPurpose)
plans %<>%
  left_join(trips, by = "trip_id") %>% 
  select(personId, legMode, planIndex, planElementIndex, planElementType,
         activityType, activityLocationX, activityLocationY, activityEndTime,
         primaryPurpose) %>% 
  left_join(select(persons, personId, householdId), by = "personId") %>% 
  #copy primaryPurpose to non-trip (non-leg) elements (possibly not necessary)
  #all plans alternate activity->leg (starting w/activity), so copy from below
  mutate(primaryPurpose = ifelse(is.na(primaryPurpose), lead(primaryPurpose),
                                  primaryPurpose)) %>% 
  #plans also end with an activity, so now copy from above
  mutate(primaryPurpose = ifelse(is.na(primaryPurpose), lag(primaryPurpose),
                                  primaryPurpose))

#convert from wgs to utm
activities <- plans %>%
  filter(planElementType == "activity") %>% 
  st_as_sf(coords = c("activityLocationX", "activityLocationY")) %>%
  `st_crs<-`(4326) %>% #WGS84
  st_transform(26912) %>% #UTM 12N
  {mutate(.,
          activityLocationX = unlist(map(.$geometry,1)),
          activityLocationY = unlist(map(.$geometry,2))
  )} %>%
  as_tibble() %>%
  select(personId, planElementIndex, activityLocationX, activityLocationY)

plans %<>%
  select(-activityLocationX, -activityLocationY) %>% 
  left_join(activities, by = c("personId", "planElementIndex"))

####fix modes
#list of modes that BEAM accepts
avail_modes <- c("bike", "walk", "car", "hov2", "hov2_teleportation",
                 "hov3", "hov3_teleportation", "drive_transit",
                 "walk_transit", "ride_hail", "ride_hail_pooled")

#add random number for hov_teleport
plans %<>% mutate(rand = runif(nrow(plans)))

#change from activitysim modes to BEAM modes
plans %<>% mutate(legMode = case_when(
  is.na(legMode) ~ legMode,
  legMode %in% avail_modes ~ legMode,
  legMode == "BIKE" ~ "bike",
  legMode == "WALK" ~ "walk",
  legMode %in% c("DRIVEALONEFREE","DRIVEALONEPAY") ~ "car",
  legMode %in% c("SHARED2FREE","SHARED2PAY") & rand < 1/2 ~ "hov2",
  legMode %in% c("SHARED2FREE","SHARED2PAY") ~ "hov2_teleportation",
  legMode %in% c("SHARED3FREE","SHARED3PAY") & rand < 1/3 ~ "hov3",
  legMode %in% c("SHARED3FREE","SHARED3PAY") ~ "hov3_teleportation",
  legMode %in% c("DRIVE_COM","DRIVE_EXP","DRIVE_LOC",
                 "DRIVE_LRF","DRIVE_HVY") ~ "drive_transit",
  legMode %in% c("WALK_COM","WALK_EXP","WALK_LOC",
                 "WALK_LRF","WALK_HVY") ~ "walk_transit",
  legMode %in% c("TNC_SINGLE","TAXI") ~ "ride_hail",
  legMode == "TNC_SHARED" ~ "ride_hail_pooled",
  T ~ "we messed up mode conversion (check activitysim to BEAM R script)"
)) %>% 
  select(-rand)


#### Create vehicles file ######################################

nveh <- sum(hh$auto_ownership)
veh_hh <- map2(hh$householdId,
               hh$auto_ownership,
               function(.x,.y) rep.int(.x, .y)
               ) %>%
  unlist() %>%
  sort()

vehicles <- tibble(vehicleId = 1:nveh,
                   vehicleTypeId = "CAR",
                   householdId = veh_hh)

#### Write files #################################################

write_csv(hh, paste0(beam_files_dir, "/households.csv"), na = "")
write_csv(hhattr, paste0(beam_files_dir, "/household_attributes.csv"), na = "")
write_csv(persons, paste0(beam_files_dir, "/persons.csv"), na = "")
write_csv(plans, paste0(beam_files_dir, "/plans.csv"), na = "")
write_csv(vehicles, paste0(beam_files_dir, "/vehicles.csv"), na = "")

##############################################################

#sanity check
plans$legMode[plans$legMode %in% c("hov2","hov2_teleportation")] %>%
  table() %>% 
  as_tibble_row() %>% 
  mutate(pct = hov2/(hov2+hov2_teleportation))
plans$legMode[plans$legMode %in% c("hov3","hov3_teleportation")] %>%
  table() %>% 
  as_tibble_row() %>% 
  mutate(pct = hov3/(hov3+hov3_teleportation))
plans$legMode %>% unique()
hh$autoWorkRatio %>% unique()
