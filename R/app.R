library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

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
  filter_df <- reactive({
    df %>%
      filter(!is.na(lat)
      & between(date, input$date_range[1], input$date_range[2])) %>%
      group_by(peak, lat, lon) %>%
      summarise(
        n = n(),
        avg_ski_condition_rating = mean(ski_condition_rating),
        avg_safety_rating = mean(safety_rating)
      )
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
    leafletProxy("map", data = filter_df()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~ sqrt(n) * 5,
        color = "black",
        weight = 1,
        fillOpacity = 0.9,
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
        title = "Rating")
  })
}

shinyApp(ui, server)
