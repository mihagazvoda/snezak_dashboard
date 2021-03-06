source("R/packages.R")
source("R/ski_tours_functions.R")
source("R/peaks_functions.R")
source("R/dashboard_functions.R")

ski_tours <- extract_all_ski_tours()
peaks <- get_peaks("./data/peak_osm_points.rds")
df <- left_join(ski_tours, rename(peaks, peak = "name"), by = "peak")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10,
    left = 10,
    HTML("<p style='font-size:10px'><a href='https://github.com/mihagazvoda/snezak_dashboard'>code </a>by <a href='https://mihagazvoda.com/about.html'>Miha Gazvoda</a></p>"),
    radioButtons(
      "rating_type",
      label = "Rating type:",
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
      start = today("CET") - days(14),
      end = today("CET"),
      min = min(df$date),
      max = today("CET")
    )
  ),
  absolutePanel(
    bottom = 10,
    left = 10,
    # actionButton("collapse_btn", "Collapse rows"),
    tags$h4("Tours with unknown locations"),
    reactableOutput("table")
  )
)

server <- function(input, output, session) {
  update_df <- reactive({
    ski_tours_between_dates(df,
      start_date = input$date_range[1],
      stop_date = input$date_range[2]
    )
  })

  output$table <- renderReactable({
    create_table(update_df())
  })

  # observeEvent(input$collapse_btn, {
  #   # Collapse all rows
  #   updateReactable("table", expanded = FALSE)
  # })

  output$map <- renderLeaflet({
    leaflet(filter(df, !is.na(lat)),
      options = leafletOptions(zoomControl = FALSE)
    ) %>%
      addTiles() %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }")
  })

  observe({
    data <- update_df() %>%
      filter(!is.na(lat)) %>%
      add_palette_column(input$rating_type)

    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      clearControls() %>%
      fitBounds(~ min(lon), ~ min(lat), ~ max(lon), ~ max(lat)) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~ sqrt(n) * 8,
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        fillColor = ~ pal(palette_column), # TODO fix and create bins instead
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
        position = "bottomright",
        bins = 4,
        title = "Rating",
        opacity = 0.75
      )
  })
}

shinyApp(ui, server)
