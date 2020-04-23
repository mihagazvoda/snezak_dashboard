# TODO remove rds file
get_peaks <- function(path_peaks) {
  osm_points <- if (file.exists(path_peaks)) {
    readr::read_rds(path_peaks)
  } else {
    get_osm_points() %>% save_rds(path_peaks)
  }
  
  get_peaks_from_osm_points(osm_points)
}

get_osm_points <- function() {
  opq(bbox = c(13.47, 46.11, 14.30, 46.54)) %>% # c(11, 45.5, 16, 48)
    add_osm_feature(key = "natural", value = "peak") %>%
    osmdata_sf() %>%
    purrr::pluck("osm_points") %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    )
  # %>% tibble()
}

get_peaks_from_osm_points <- function(peak_osm_points) {
  peak_osm_points %>%
    filter(!is.na(name) & ele > 1000) %>%
    group_by(name) %>%
    arrange(desc(prominence), wikipedia, wikidata) %>%
    filter(row_number() == 1) %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    ) %>%
    select(name, lat, lon)
}

save_rds <- function(file, path) {
  readr::write_rds(file, path = path)
  invisible(file)
}
