#!/usr/bin/env Rscript

if(!require(pacman)) install.packages("pacman")
pacman::p_load(targets, tidyverse, sf, tigris, tidycensus)

tar_make(skims_file)

quit("no")
