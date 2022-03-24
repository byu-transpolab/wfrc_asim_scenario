if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, sf)

######################################################

beam_files_dir <- "data_beam"
output_dir <- "sample_data_beam"

#read in csvs
hh <- read_csv(paste0(beam_files_dir, "/households.csv"))
hhattr <- read_csv(paste0(beam_files_dir, "/household_attributes.csv"))
persons <- read_csv(paste0(beam_files_dir, "/persons.csv"))
plans <- read_csv(paste0(beam_files_dir, "/plans.csv"))
vehicles <- read_csv(paste0(beam_files_dir, "/vehicles.csv"))

######################################################

keep_hh <- sample(hh$householdId, 100000)

hh %<>% filter(householdId %in% keep_hh)
hhattr %<>% filter(householdId %in% keep_hh)
persons %<>% filter(householdId %in% keep_hh)
plans %<>% filter(householdId %in% keep_hh)
vehicles %<>% filter(householdId %in% keep_hh)

######################################################

write_csv(hh, paste0(output_dir, "/households.csv"), na = "")
write_csv(hhattr, paste0(output_dir, "/household_attributes.csv"), na = "")
write_csv(persons, paste0(output_dir, "/persons.csv"), na = "")
write_csv(plans, paste0(output_dir, "/plans.csv"), na = "")
write_csv(vehicles, paste0(output_dir, "/vehicles.csv"), na = "")
