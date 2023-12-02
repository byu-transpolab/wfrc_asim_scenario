library(tidyverse)

odir <- "activitysim/output"
cdir <- "activitysim/calibrate_wfh_configs"

while(iter <= 15) {
  wfh_targets <- read_csv("data/ControlTotal_WorkAtHome.csv") %>% 
    filter(CO_FIPS == 1049) %>% 
    select(YEAR, contains("Tel"))
  wfh_targets_2019 <- wfh_targets %>% 
    filter(YEAR == 2019) %>% 
    select(-YEAR) %>% 
    pivot_longer(everything(), names_to = "pjobcat", values_to = "target_pct") %>% 
    mutate(pjobcat = str_remove(pjobcat, "Tel_"))
  
  prev_iter <- list.files(cdir, recursive = FALSE) %>% 
    str_extract("telecommute_frequency_coefficients_\\d+") %>% 
    {.[!is.na(.)]} %>% 
    str_extract("\\d+") %>% 
    as.integer() %>% 
    max()
  
  iter <- prev_iter + 1
  
  last_telecommute <- read_csv(
    file.path(odir, paste0("calibrate_wfh_", prev_iter), "wfh_persons.csv")
  ) %>% 
    select(ptype, pemploy, pjobcat, telecommute_frequency) %>% 
    filter(
      pjobcat != "none",
      pemploy %in% c(1,2)
    ) %>% 
    count(pjobcat, telecommute_frequency) %>% 
    left_join(
      tribble(
        ~telecommute_frequency, ~pct_tc,
        "No_Telecommute", 0 / 5,
        "1_day_week", 1 / 5,
        "2_3_days_week", 2.5 / 5,
        "4_days_week", 4 / 5
      )
    ) %>% 
    mutate(
      pct_tc = replace_na(pct_tc, 0),
      tc = n*pct_tc
    ) %>% 
    group_by(pjobcat) %>% 
    summarise(tc_pct = sum(tc)/sum(n)) 
  
  telecommute_adj <- last_telecommute %>% 
    left_join(wfh_targets_2019) %>% 
    mutate(
      adj_overall = log(target_pct/tc_pct),
      # Centroid of a triangle 1 = x/5 + y/2 + 4z/5 where {x, y, z} >= 0
      # Based on how many days each category telecommutes (1, 2.5, and 4)
      adj_1 = adj_overall * 5/3,
      adj_23 = adj_overall * 2/3,
      adj_4 = adj_overall * 5/12) %>% 
    pivot_longer(adj_1:adj_4, names_to = "days", values_to = "adj") %>% 
    mutate(
      days = str_replace(days, "adj_(\\d+)", "\\1day"),
      coefficient_name = paste("coefj", pjobcat, days, sep = "_")
    ) %>% 
    select(coefficient_name, adj)
  
  
  prev_coeffs <- file.path(cdir, paste0("telecommute_frequency_coefficients_", prev_iter, ".csv")) %>% 
    read_csv()
  
  new_coeffs <- prev_coeffs %>% 
  # for testing#######################
  # filter(str_detect(coefficient_name, "coefj")) %>%
  left_join(telecommute_adj, join_by(coefficient_name)) %>% 
    mutate(
      across(c(value, adj), \(x) replace_na(x, 0)),
      # don't adjust coefficients if they're already big
      adj = if_else(abs(value) > 5, 0, adj),
      value = round(value + adj, 3)) %>% 
    select(coefficient_name, value, constrain)
  
  new_coeffs_file <- paste0("telecommute_frequency_coefficients_", iter, ".csv")
  
  write_csv(new_coeffs, file.path(cdir, new_coeffs_file))
  
  read_lines(file.path(cdir, "telecommute_frequency.yaml")) %>% 
    str_replace("COEFFICIENTS:.+", paste("COEFFICIENTS:", new_coeffs_file)) %>% 
    write_lines(file.path(cdir, "telecommute_frequency.yaml"))
  
  system2("./activitysim/calibrate_wfh.sh")
  
}