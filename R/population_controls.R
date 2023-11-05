#' Get TAZ control
#' 
#' @param taz_control_file
#'
#'
get_taz_control <- function(se, crosswalk){
  se %>%
    read_csv() %>% 
    transmute(
      TAZ = as.character(zone_id), 
      HHBASE = as.integer(TOTHH)
    ) %>% 
    filter(TAZ %in% crosswalk$TAZ)
}


#' Get size work variables
#' 
#' @param acvars
#' @param mycounties
#' 
#' Because these two values come in the same table, we will build them together.
#' In both cases the top-line category contains all households with that many
#' workers / persons or more.
#' 
get_sizework_controls <- function(acsvars, mycounties){
  swvars <- str_c("B08202_", sprintf("%03d", c(2:5, 6, 9, 13, 18)))
  raw_sw <- get_acs("tract", variables = swvars, state = "UT", county = mycounties, 
                    year = 2019)
  
  size_work <- raw_sw %>% 
    left_join(acsvars, by = c("variable" = "name")) %>%
    separate(label, c("VAR", "total", "label"), sep = "!!") %>%
    select(GEOID, label, estimate) 
  
  works <- size_work %>%
    filter(grepl("work", label)) %>%
    mutate(
      num_work = str_extract(label, "\\d+"),
      workcat = case_when(
        num_work == 1 ~ "HHWORK1",
        num_work == 2 ~ "HHWORK2",
        num_work == 3 ~ "HHWORK3",
        TRUE ~ "HHWORK0"
      )
    ) %>% 
    group_by(GEOID, workcat) %>% summarize(count = as.integer(sum(estimate)))
  
  sizes <- size_work %>%
    filter(!grepl("work", label)) %>%
    mutate(
      num_size = str_extract(label, "\\d+"),
      sizecat = str_c("HHSIZE", num_size) 
    ) %>%
    group_by(GEOID, sizecat) %>% summarize(count = as.integer(sum(estimate)))
  
  list( "sizes" = sizes, "works" =  works )
}

#' Get age variables
#' 
#' @param acsvars
#' @param mycounties
#'
#' This is the number of people in each age category.
#' 
get_age_controls <- function(acsvars, mycounties){
  agevars <- str_c("B01001_", sprintf("%03d", c(3:25, 27:49)))
  raw_ages <- get_acs("tract", variables = agevars, state = "UT", county = mycounties, year = 2019)
  
  ages <- raw_ages %>%
    left_join(acsvars, by = c("variable" = "name")) %>%
    separate(label, c("VAR", "total", "sex", "age"), sep = "!!") %>%
    select(GEOID, sex, age, estimate)  %>%
    
    # regroup age categories
    mutate(
      numage = as.numeric(substr(age, 1, 2)),
      agecat = case_when(
        numage %in% c(15:24) ~ "PAGE1",
        numage %in% c(25:54) ~ "PAGE2",
        numage %in% c(55:64) ~ "PAGE3",
        numage %in% c(65:99) ~ "PAGE4",
        TRUE ~ "PAGE0" # children less than 15 not categorized in demo
      )
    ) %>%
    
    # consolidate men and women
    group_by(GEOID, agecat) %>%
    summarise(count = as.integer(sum(estimate)))
  
  ages
}

#' Get income variables
#' 
#' @param acsvars
#' @param mycounties
#' 
#' This is the household income variable, which is categorized as follows:
#'   - <$15k
#'   - \$15k - \$30k
#'   - \$30k - \$60k
#'   - > $60k
#' 
get_income_controls <- function(acsvars, mycounties){
  incvars <- str_c("B19001_", sprintf("%03d", c(2:17)))
  raw_incs <- get_acs("tract", variables = incvars, state = "UT", county = mycounties, year = 2019)
  
  incs <- raw_incs %>%
    left_join(acsvars, by = c("variable" = "name")) %>%
    separate(label, c("VAR", "total", "income"), sep = "!!") %>%
    select(GEOID, income, estimate)  %>%
    # regroup income categories
    mutate(
      numinc  = stringr::str_extract(income, "\\d+"),
      inccat = case_when(
        numinc <  15 ~ "HHINC1",
        numinc <  30 ~ "HHINC2",
        numinc <  60 ~ "HHINC3",
        numinc >= 60 ~ "HHINC4",
        TRUE ~ as.character(NA)
      )
    ) %>%
    group_by(GEOID, inccat) %>%
    summarise(count = as.integer(sum(estimate)))
  
  incs
}

#' Combine variables
#' 
#' @param mytracts
#' @param ages
#' @param incs
#' @param sizes includes workers
#' 
#' @details  When all of the controls have been gathered, we can put them into one large table.
#'
make_controls <- function(mytracts, ages, incs, sizes, ...){
  tibble(TRACT = mytracts) %>%
     left_join(ages  %>% spread(agecat,  count), by = c("TRACT" = "GEOID")) %>%
     left_join(incs  %>% spread(inccat,  count), by = c("TRACT" = "GEOID")) %>%
     left_join(sizes$works %>% spread(workcat, count), by = c("TRACT" = "GEOID")) %>%
     left_join(sizes$sizes %>% spread(sizecat, count), by = c("TRACT" = "GEOID"))
}

# `from` and `to` must be the same size unless `length(from)` is 1
replace_tract_controls <- function(from, to, popsim_data_dir) {
  tract_controls_file <- file.path(popsim_data_dir, "control_totals_tract.csv")
  tract_controls_old <- read_csv(tract_controls_file)
  
  if(length(from) == 1) {
    new_row <- tract_controls_old %>% 
      filter(TRACT == from)
    if(nrow(new_row) > 1) stop(paste("Duplicate tract controls for tract", from))
    new_rows <- uncount(new_row, length(to)) %>% 
      mutate(TRACT = to)
    
    tract_controls_new <- bind_rows(
      tract_controls_old %>% 
        filter(!TRACT %in% to),
      new_rows
    )
    
  }
  ### TODO: write for case where `length(from)` > 1
  
  write_csv(tract_controls_new, tract_controls_file)
  tract_controls_new
}