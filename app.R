source("R/packages.R")
source("R/ski_tours_functions.R")
source("R/peaks_functions.R")
source("R/dashboard_functions.R")

ski_tours = extract_all_ski_tours()
peaks = get_peaks("./data/peak_osm_points.rds")
df = left_join(ski_tours, rename(peaks, peak = 'name'), by = 'peak')

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10,
    left = 10,
    radioButtons(
      "rating_type",
      label = "Rating type: ",
      choices = c(
        "Ski conditions" = "avg_ski_condition_rating",
        "Safety" = "avg_safety_rating"
      )
    )
  ),
  absolutePanel(
    top = 10,
    right = 10,
    dateRangeInput(
      "date_range",
      label = "Date range:",
      start = Sys.Date() - 60,
      end = Sys.Date(),
      min = min(df$date),
      max = Sys.Date()
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression for the data subsetted to what the user selected
  update_df <- reactive({
    ski_tours_between_dates(df, 
                  start_date = input$date_range[1], 
                  stop_date = input$date_range[2])
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filter(df, !is.na(lat)),
      options = leafletOptions(zoomControl = FALSE)
    ) %>%
      addTiles() %>%
      fitBounds(~ min(lon), ~ min(lat), ~ max(lon), ~ max(lat)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }")
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = update_df()) %>%
      clearMarkers() %>%
      clearControls() %>% 
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~ sqrt(n) * 5,
        color = "black", #~ pal(avg_ski_condition_rating),
        weight = 1,
        fillOpacity = 0.8,
        fillColor = ~ pal(avg_ski_condition_rating), # TODO fix and create bins instead
        label = ~peak,
        popup = ~ paste(
          "<a href=", peak_url(peak), ">", peak, "</a>", "<br>",
          # "Tours:", as.character(n), "<br>",
          "Conditions:", specify_decimal(avg_ski_condition_rating, 1), "<br>",
          "Safety:", specify_decimal(avg_safety_rating, 1)
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = c(1, 5), 
        position = "bottomleft", 
        bins = 4, 
        title = "Rating",
        opacity = 0.75)
  })
}

shinyApp(ui, server)
