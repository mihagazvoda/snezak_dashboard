pal <- leaflet::colorNumeric(
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
  format(round(x, k), nsmall = k)
}

ski_tours_between_dates <- function(df, start_date, stop_date) {
  df %>%
    filter(between(date, start_date, stop_date)) %>%
    group_by(peak, lat, lon) %>%
    summarise(
      n = n(),
      avg_ski_condition_rating = mean(ski_condition_rating),
      avg_safety_rating = mean(safety_rating)
    ) %>%
    ungroup()
}

create_table <- function(df) {
  df %>%
    filter(is.na(lat)) %>%
    select(-lat, -lon) %>%
    mutate(across(starts_with("avg"), specify_decimal, 1)) %>%
    rename(
      count = n,
      conditions = avg_ski_condition_rating,
      safety = avg_safety_rating
    ) %>%
    reactable(
      # compact = TRUE,
      defaultPageSize = 5,
      columns = list(
        peak = colDef(cell = function(value) {
          htmltools::tags$a(href = peak_url(value), target = "_blank", as.character(value))
        })
      )
    )
}
