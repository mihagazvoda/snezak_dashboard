library(shiny)
library(leaflet)
library(dplyr)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
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
      group_by(peak) %>%
      # TODO has to be summarise
      mutate(
        n = n(),
        avg_condition_rating = mean(ski_condition_rating),
        avg_safety_rating = mean(safety_rating)
      )
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filter(df, !is.na(lat))) %>%
      addTiles() %>% # Add default OpenStreetMap map tiles
      # addMarkers(lng = lon, lat = lat, popup = mountain)
      fitBounds(~ min(lon), ~ min(lat), ~ max(lon), ~ max(lat)) 
      # addProviderTiles(providers$CartoDB.Positron)
    # leaflet(quakes) %>%
    #   addTiles() %>%
    #   fitBounds(~ min(long), ~ min(lat), ~ max(long), ~ max(lat))
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
        radius = ~ sqrt(n) * 2,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~ paste(
          "Count", as.character(n), "<br>",
          "Conditions:", avg_condition_rating, "<br>",
          "Safety:", avg_safety_rating
        ),
        label = ~ paste(
          "Count", as.character(n), "<br>",
          "Conditions:", avg_condition_rating, "<br>",
          "Safety:", avg_safety_rating
        )
      )
  })
}

shinyApp(ui, server)
