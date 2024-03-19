get_transit_skims_names <- function(omx_file) {
  omxr::list_omx(omx_file) %>% 
    magrittr::use_series(Matrices) %>% 
    filter(
      str_detect(name, "COM"),
      str_detect(name, "TOTIVT"),
      # # Time of day doesn't matter so we only take PM skims
      # str_detect(name, "PM"),
      # # Mode also doesn't make much difference so we only take D_C_W
      # str_detect(name, "DRV_COM_WLK")
    ) %>% 
    pull(name)
}

combine_omx <- function(...) {
  bind_rows(..., .id = "scenario") %>% 
    # filter(!near(value, 0)) %>% 
    pivot_wider(names_from = scenario) %>% 
    mutate(increase = tr - by)
}

clean_omx <- function(omx) {
  omx %>% 
    pivot_longer(-c(origin, destination), names_to = "matrix") %>% 
    separate_wider_delim(
      matrix,
      names = c("mode1", "mode2", "mode3", "key", "time"),
      delim = stringr::regex("_+")) %>% 
    mutate(mode = paste(mode1, mode2, mode3, sep = " to ")) %>% 
    select(-c(mode1, mode2, mode3))
}

plot_omx_diff <- function(omx) {
  omx %>% 
    ggplot(aes(x = increase)) +
    # facet_grid(rows = vars(time), cols = vars(mode), scales = "free") +
    geom_histogram(boundary = 0)
}

plot_taz_numbers <- function(taz) {
  taz %>% 
    ggplot() +
    geom_sf(color = "black", fill = NA) +
    geom_sf_text(aes(label = TAZID), size = 1)
}

function() {
  trace_omx %>% 
    filter(time == "AM")
  
  taz %>% 
    st_transform(4326) %>% 
    ggplot() +
    annotation_map_tile(zoom = 15) +
    geom_sf(fill = NA) +
    geom_sf_label(aes(label = TAZID))# +
    coord_sf(xlim = c(-111.7, -111.6), ylim = c(40.2, 40.25))
    
    taz %>% 
      filter(TAZID %in% otaz) %>% 
      ggplot() +
      geom_sf()
  
  omx_origin_taz %>% 
    ggplot() +
    geom_sf(aes(fill = increase)) +
    scale_fill_gradient2()
  
  omx %>% 
    filter(destination == 3001) %>%
    # summarise(
    #   increase = sum(increase, na.rm = TRUE),
    #   by = sum(by, na.rm = TRUE),
    #   tr = sum(tr, na.rm = TRUE),
    #   .by = c(origin, key, time, mode)
    # ) %>% 
    left_join(taz, join_by(origin == TAZID)) %>% 
    filter(!near(by, 0)) %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(aes(fill = by)) +
    scale_fill_gradient2()

  
  
  }