library(tidyverse)
library(omxr)
library(magrittr)

by_file <- "data/skims/_built/BY_2019.omx"
tr_file <- "data/skims/_built/doubletrack.omx"

transit_skims_names <- list_omx(by_file) %>% 
  use_series(Matrices) %>% 
  filter(
    str_detect(name, "COM"),
    str_detect(name, "IVT"),
    str_detect(name, "PM")
  ) %>% 
  pull(name)

separate_omx <- function(omx) {
  omx %>% 
    pivot_longer(-c(origin, destination), names_to = "matrix") %>% 
    separate_wider_delim(
      matrix,
      names = c("mode1", "mode2", "mode3", "key", "time"),
      delim = stringr::regex("_+"))
}

by <- read_all_omx(by_file, transit_skims_names)
tr <- read_all_omx(tr_file, transit_skims_names)

omx <- bind_rows(
  by = separate_omx(by),
  tr = separate_omx(tr),
  .id = "scenario"
) %>% 
  pivot_wider(names_from = scenario) %>% 
  mutate(diff = tr-by)

omx %>% 
  filter(diff != 0) %>% 
  pull(diff) %>% 
  hist()

omx2 <- omx %>% 
  filter(diff != 0) %>% 
  select(-mode2) %>% 
  rename(before = mode1, after = mode3) %>% 
  mutate(mode = paste(before, "to COM to", after))
  
ggplot(omx2) +
  facet_grid(rows = vars(key, time), cols = vars(mode), scales = "free") +
  geom_histogram(aes(x = diff), boundary = 0)
