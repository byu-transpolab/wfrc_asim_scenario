library(tidyverse)

########
wfrc_se <- "data/taz_se/TAZ_SE_2019_WFRC.csv"
be_se <- "data/taz_se/TAZ_SE_2019_BOXELDER.csv"
out_file <- "data/taz_se/taz_se_2019_all.csv"

# landuse_se <- "data/taz_se/TAZ_SE_new_landuse_WFRC.csv"
########

be <-
  read_csv(be_se) %>%
  filter(DLRG_NAME == "Box Elder - WFRC") %>% 
  select(-`;CO_TAZID`) %>% 
  rename(zone_id = Index, HSENROLL = Enrol_High) 

wfrc <-
  read_csv(wfrc_se) %>%
  select(-CO_TAZID) %>% 
  rename(zone_id = `;TAZID`, HSENROLL = Enrol_High) %>% 
  filter(!zone_id %in% be$zone_id)



se_data <-
  bind_rows(wfrc, be) %>% 
  {if(!is.character(.$zone_id)) arrange(., zone_id)} %>% 
  transmute(
    zone_id = as.character(zone_id),
    TOTPOP = HHPOP,
    TOTHH,
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
    GRADEENROLL = Enrol_Elem + Enrol_Midl
  )

se_data %>% 
  write_csv(out_file)
