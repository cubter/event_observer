timelineUI <- function(id)
{
    ns <- NS(id)
    
    # Timeline's panel
    absolutePanel(
        id = "timeline_panel",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = "auto",
        left = 40,
        right = 40,
        bottom = 20,
        width = "auto",
        height = 200,
        style = "opacity: 0.75; padding: 0 10px 5px 5px",
        
        plotlyOutput(ns("timeline"), height = "180px", width = "100%"),
        
        h5("Species timeline", align = "center", top = "10px"),
    )
}

timelineServer <- function(id, observationsInBoundsData, color)
{
    moduleServer(
        id,
        function(input, output, session) 
        {
            output$timeline <- renderPlotly(
            {
                # progress <- shiny::Progress$new()
                # on.exit(progress$close())
                # progress$set(message = "Making plot", value = 0)
                
                timeline_data <- observationsInBoundsData()
                
                # TODO: proper handling
                if (timeline_data %>% is_empty())
                    return()
                
                # Drawing a plot.
                time_plot <- plot_ly(
                    data = timeline_data, 
                    x = ~date, 
                    y = ~weight,
                    type = "scatter",
                    color = I(color()),
                    # popup text
                    hovertemplate = ~paste(
                        "<b>Place</b>:", coord, 
                        "<br><b>Date</b>:", date,
                        "<br><b>Time</b>:", time, 
                        "<extra></extra>")
                    ) %>% 
                    toWebGL()
                # Styling the plot.
                time_plot %<>% config(., scrollZoom = TRUE) %>%
                    layout(
                        yaxis = list(
                            showgrid = TRUE,
                            zeroline = FALSE,
                            gridcolor = "#f6f6f6",
                            title = FALSE,
                            showticklabels = FALSE),
                        xaxis = list(
                            showgrid = TRUE,
                            zeroline = FALSE,
                            gridcolor = "#f6f6f6",
                            title = FALSE,
                            tickangle = 45,
                            tickfont = list(
                                family = "Courier",
                                size = 13
                                )
                            )
                    )
                
                # WebGL allows to significantly improve the performance of 
                # plots' rasterisation.
                # time_plot %<>% toWebGL()
                
                # progress$inc(100)
                return(time_plot)
            })
        }
    )
}