get_peaks <- function(path_peaks) {
  ifelse(
    file.exists(path_peaks),
    readr::read_rds(path_peaks), 
    get_peaks_from_osm() %>% save_rds(path_peaks)
  ) 
}

save_rds <- function(file, path){
  readr::write_rds(deduplicated_peaks, path = path)
  file
}

get_peaks_from_osm <- function() {
  peak_osm_points <- opq(bbox = c(11, 45.5, 16, 48), ) %>% # 
    add_osm_feature(key = "natural", value = "peak") %>%
    osmdata_sf() %>%
    purrr::pluck("osm_points")
  
  tidy_peaks <- peak_osm_points %>%
    filter(!is.na(name)) %>%
    select(name, ele, geometry, wikipedia, prominence, importance, massif, wikidata) %>%
    `[<-`(, c("lat", "lon"), map(transpose(.$geometry), unlist))
  
  deduplicated_peaks <- tidy_peaks %>%
    group_by(name) %>%
    arrange(desc(prominence), importance, wikipedia, wikidata) %>%
    filter(row_number() == 1) %>% 
    select(name, lat, lon)
}