mapUI <- function(id)
{
    ns <- NS(id)
    
    leafletOutput(ns("map"), width="100%", height="100%")

}

mapServer <- function(id, eventsData, markersColor, clusteringOptions = NULL)
{
    moduleServer(
        id,
        function(input, output, session) 
        {
            # Renders naked map.
            output$map <- renderLeaflet(
            {
                # Static maps without markers, hence leaflet() instead of leafletProxy().
                leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
                    addTiles()
            })

            # Responsible for drawing markers on the map according to the species
            # selected.
            observe(
            {
                map_data <- eventsData()

                if (map_data %>% is_empty())
                    return()

                lat_long_popups <- paste("<b>Place</b>:", map_data$coord)
                datetimes_popups <- paste("<br><strong>Time:</strong>",
                                          map_data$date,
                                          map_data$time)

                map_data %<>%
                    dplyr::mutate(., popup = paste(lat_long_popups, datetimes_popups))

                leafletProxy("map", data = map_data) %>%
                    clearMarkers() %>%
                    clearMarkerClusters() %>%
                    addCircleMarkers(
                        # If there are thousands of events for certain species,
                        # the markers will be smaller
                        radius = ~1 / log(length(lat), 1000000),
                        clusterOptions = clusteringOptions(),
                        stroke = FALSE,
                        fillColor = markersColor(),
                        fillOpacity = 0.6,
                        popup = map_data$popup) %>%
                    fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat),
                              options = list(maxZoom = 5))
            })
        }
    )
}
