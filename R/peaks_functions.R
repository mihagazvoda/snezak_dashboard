get_peaks <- function(path_peaks) {
  osm_points <- if (file.exists(path_peaks)) {
    readr::read_rds(path_peaks)
  } else {
    bbox_coord <- tibble(
      min_lon = seq(11, 15.75, 0.25),
      max_lon = min_lon + 0.25,
      min_lat = 45.5,
      max_lat = 48
      )
    
    pmap(bbox_coord, peak_osm_points) %>% 
      bind_rows() %>% 
      save_rds(path_peaks)
  }
  
  get_peaks_from_osm_points(osm_points)
}

peak_osm_points <- function(min_lon, max_lon, min_lat, max_lat) {
  Sys.sleep(5)
  cat(min_lon, "\n")
  
  opq(bbox = c(min_lon, min_lat, max_lon, max_lat)) %>% # lon: 11-16, lat: 45.5-48
    add_osm_feature(key = "natural", value = "peak") %>%
    osmdata_sf() %>%
    purrr::pluck("osm_points") %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    ) %>% 
    st_drop_geometry()
}

get_peaks_from_osm_points <- function(peak_osm_points) {
  peak_osm_points %>%
    mutate(ele = as.integer(ele)) %>% 
    filter(!is.na(name) & ele > 1000) %>% # TODO ele filter doesnt work
    separate_rows(name, sep = " / ") %>% 
    group_by(name) %>%
    arrange(desc(ele), wikipedia, wikidata) %>%
    filter(row_number() == 1) %>%
    select(name, lat, lon)
}

save_rds <- function(file, path) {
  readr::write_rds(file, path = path)
  invisible(file)
}
