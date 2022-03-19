#!/usr/bin/env Rscript
if(!require(pacman)) install.packages("pacman")
pacman::p_load(targets, tidyverse, sf, tigris, tidycensus)

#census_api_key("b98cbafba45824de8d061bcb54876d6db5be882b", install = T)
#readRenviron("~/.Renviron")

tar_make()

quit("no")
