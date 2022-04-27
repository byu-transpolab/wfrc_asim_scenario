#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
library(tidyverse)

args <- as.numeric(args)

print(args)

#check args
if(length(args)==0){
  stop("Please indicate the fraction(s) to sample")
} else if(any(!is.numeric(args))){
  stop("All arguments must be a number between 0 and 1")
} else if(any(args <= 0 | args >= 1)){
  stop("All arguments must be a number between 0 and 1")
}

#check file existence
if(!all(c(file.exists("data_beam/persons.csv"),
          file.exists("data_beam/plans.csv"),
          file.exists("data_beam/households.csv"),
          file.exists("data_beam/household_attributes.csv"),
          file.exists("data_beam/vehicles.csv")))
){
  stop("Please ensure all the correct files are in 'data_beam':\n
       persons.csv\n
       plans.csv\n
       households.csv\n
       household_attributes.csv\n
       vehicles.csv")
}

#read in files
per <- read_csv("data_beam/persons.csv")
pl <- read_csv("data_beam/plans.csv")
hh <- read_csv("data_beam/households.csv")
hhattr <- read_csv("data_beam/household_attributes.csv")
veh <- read_csv("data_beam/vehicles.csv")

#set sample sizes and out_dirs
pct <- round(args,3)
out_dir <- paste0("data_beam/sample_", pct)

#create empty lists
households <- list()
hh_attributes <- list()
persons <- list()
plans <- list()
vehicles <- list()

#take samples
for(i in 1:length(pct)){
  rows <- sample(nrow(hh), nrow(hh)*pct[i])
  
  households[[i]] <- hh %>% 
    filter(row_number() %in% rows)
  
  hh_attributes[[i]] <- hhattr %>% 
    filter(householdId %in% households[[i]]$householdId)
  
  persons[[i]] <- per %>% 
    filter(householdId %in% households[[i]]$householdId)
  
  plans[[i]] <- pl %>% 
    filter(personId %in% persons[[i]]$personId)
  
  vehicles[[i]] <- veh %>% 
    filter(householdId %in% households[[i]]$householdId)
}

#write output files
for(i in 1:length(households)){
  if(!dir.exists(out_dir[i])) dir.create(out_dir[i])
  
  write_csv(persons[[i]], paste0(out_dir[i], "/persons.csv"))
  write_csv(plans[[i]], paste0(out_dir[i], "/plans.csv"))
  write_csv(households[[i]], paste0(out_dir[i], "/households.csv"))
  write_csv(hh_attributes[[i]], paste0(out_dir[i], "/household_attributes.csv"))
  write_csv(vehicles[[i]], paste0(out_dir[i], "/vehicles.csv"))
}