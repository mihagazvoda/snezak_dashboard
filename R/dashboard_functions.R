pal <- colorNumeric(
  palette = "RdYlBu",
  domain = c(1, 5)
)

add_palette_column <- function(data, palette_column) {
  data %>% 
    mutate(palette_column = !!sym(palette_column))
}

peak_url <- function(peak) {
  paste0(
    "https://www.snezak.si/search.html?key=",
    stringr::str_replace_all(peak, pattern = " ", replacement = "+")
  )
}

specify_decimal <- function(x, k) {
  format(round(x, k), nsmall=k)
} 

ski_tours_between_dates <- function(df, start_date, stop_date) {
  df %>%
    filter(!is.na(lat)
           & between(date, start_date, stop_date)) %>%
    group_by(peak, lat, lon) %>%
    summarise(
      n = n(),
      avg_ski_condition_rating = mean(ski_condition_rating),
      avg_safety_rating = mean(safety_rating)
    )
}