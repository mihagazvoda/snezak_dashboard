deploy_app <- function(file) {
  rsconnect::deployApp(
    appFiles = file,
    appName = "snezak",
    forceUpdate = TRUE
  )
}