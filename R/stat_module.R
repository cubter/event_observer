statisticsUI <- function(id) 
{
    ns <- NS(id)
    
    tagList(
        h4(textOutput(ns("total_species")), align = "right"),
        h5(textOutput(ns("total_events")), align = "right"),
        h6(textOutput(ns("last_update_date")), align = "right"),
        plotlyOutput(ns("top_years"), height = "240px", width = "auto"),
        plotlyOutput(ns("last_ten_years"), height = "240px", width = "auto")
    )
}

statisticsServer <- function(
    id, 
    top_years_data, 
    last_ten_years_data, 
    num_species, 
    num_events, 
    last_update_date,
    colorOption)
{
    moduleServer(
        id,
        function(input, output, session) 
        {
            # Displays the total number of species in the DB.
            output$total_species <- renderText(paste(
                prettyNum(num_species, big.mark=","), "species"))
            # Displays the total number of events in the DB.
            output$total_events <- renderText(paste(
                prettyNum(num_events, big.mark=","), "events"))
            
            # Displays the date of the last update.
            if (last_update_date %>% is_empty() %>% not())
            {
                output$last_update_date <- renderText(paste(
                    "Update on: ", format(as.Date(last_update_date[[1]]), "%d %B %Y")))
            }
            else
            {
                output$last_update_date <- renderText("Last update's date uknown")
            }
            
            # Plots' part
            
            # Renders the last 10 years plot.
            observe(
            {
                # TODO: replace with validate()
                if (last_ten_years_data %>% is_empty())
                    return()
                
                last_years_plot <- plot_ly(
                    data = last_ten_years_data, 
                    color = I(colorOption()),
                    type = "bar",
                    showlegend = FALSE
                )
                last_years_plot %<>% add_bars(
                    x = ~year, 
                    y = ~log(score, 2),
                    width = 0.3,
                    hovertemplate = ~paste(
                        "<b>Year</b>:", year, "<br><b>Count</b>:",
                        score, "<extra></extra>"),
                    marker = list(opacity = 0.7)
                )
                
                last_years_plot %<>% config(., scrollZoom = TRUE) %>%
                    layout(
                        xaxis = list(
                            showgrid = FALSE, 
                            zeroline = FALSE,
                            title = list(
                                text = "<b>Last ten years dynamics</b>",
                                family = "Courier",
                                size = 13),
                            tickfont = list(
                                family = "Courier",
                                size = 13)
                        ),
                        yaxis = list(
                            showgrid = FALSE, 
                            zeroline = FALSE,
                            title = list(
                                text = "N events (log2)",
                                family = "Courier",
                                size = 13),
                            tickfont = list(
                                family = "Courier",
                                size = 13)
                        ),
                        showlegend = FALSE
                    )
                
                    output$last_ten_years <- renderPlotly(last_years_plot) 
            })
             
            # Rendering top 10 years plot.
            observe(
            {  
                if (top_years_data %>% is_empty())
                    return()
                
                top_years_plot <- plot_ly(
                    data = top_years_data, 
                    x = ~year, 
                    y = ~year_share,
                    type = "scatter",
                    mode = "markers",
                    text = ~paste(score, "observations", "<br><b>Year</b>:", year,
                                  "<br><b>Share</b>:", year_share, "%"),
                    hoverinfo = "text",
                    showlegend = FALSE, 
                    color = I(colorOption()),
                    marker = list(
                        size = ~log(score, 1.5),
                        opacity = 0.6,
                        line = list(width = 0))
                )
                
                top_years_plot <- top_years_plot %>% 
                    config(., scrollZoom = TRUE) %>%
                    layout(.,
                           xaxis = list(
                               showgrid = FALSE,
                               zeroline = FALSE,
                               title = list(
                                   text = "<b>Top ten years, by events</b>",
                                   family = "Courier",
                                   size = 13),
                               tickfont = list(
                                   family = "Courier",
                                   size = 13)
                           ),
                           yaxis = list(
                               showgrid = FALSE,
                               zeroline = FALSE,
                               title = list(
                                   text = "Percent",
                                   family = "Courier",
                                   size = 13),
                               tickfont = list(
                                   family = "Courier",
                                   size = 13)
                           ),
                           showlegend = FALSE
                    )
                
                output$top_years <- renderPlotly(top_years_plot)
            })
    })
}