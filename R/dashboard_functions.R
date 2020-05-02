deploy_app <- function(file, ...) {
  deployApp(
    appFiles = file,
    appName = "snezak",
    forceUpdate = TRUE
  )
}

pal <- colorNumeric(
  palette = "RdBu",
  domain = c(1, 5)
)

peak_url <- function(peak) {
  paste0(
    "https://www.snezak.si/search.html?key=",
    stringr::str_replace_all(peak, pattern = " ", replacement = "+")
  )
}

specify_decimal <- function(x, k) {
  format(round(x, k), nsmall=k)
} 