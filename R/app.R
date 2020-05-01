library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

pal <- colorNumeric(
  palette = "Blues",
  domain = c(1,5)
  )

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10,
    left = 10,
    radioButtons(
    "rating_type",
    label = "Rating type: ",
    choices = c("Ski conditions" = "ski_condition",
                "Safety" = "safety")
  )),
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
        avg_condition_rating = mean(ski_condition_rating),
        avg_safety_rating = mean(safety_rating)
      )
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filter(df, !is.na(lat)), 
            options = leafletOptions(zoomControl = FALSE)) %>%
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
        color = pal(5),
        weight = 1,
        fillOpacity = 0.75,
        fillColor = ~pal(avg_condition_rating),
        popup = ~paste(
          "Count", as.character(n), "<br>",
          "Conditions:", avg_condition_rating, "<br>",
          "Safety:", avg_safety_rating
        ),
        label = ~peak
        # ,
        # label = ~ paste(
        #   "Count", as.character(n), "<br>",
        #   "Conditions:", avg_condition_rating, "<br>",
        #   "Safety:", avg_safety_rating
        # )
      )
  })
}

shinyApp(ui, server)
