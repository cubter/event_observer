library(shiny)
library(leaflet)
library(rlang)
library(RColorBrewer)
library(purrr)
library(magrittr)
library(plotly)
library(tibble)

# Error codes: 
# 1: response from Redis contains empty value.

r <- redux::hiredis()

keys <- r$HKEYS("vernacular_name:scientific_name")
vals <- r$HVALS("vernacular_name:scientific_name")
species <- c(keys, vals) %>% unlist()

# It seems redux just doesn't know yet that ZREVRANGE is deprecated.
redis_top_years <- r$ZREVRANGE("year_count", 0, 9, "WITHSCORES")
year_indexes <- map2_lgl(redis_top_years, seq_along(redis_top_years),
                         ~ (.y %% 2 == 1))
top_years <- tibble(year = redis_top_years[year_indexes] %>% unlist() %>% as.integer(),
                    score = redis_top_years[!year_indexes] %>% unlist() %>% as.integer())
top_years$year_share = (top_years$score/sum(top_years$score) * 100) %>% 
    round(., 1)

# Define server logic required to draw a histogram
server <- function(input, output, session) 
{
    # Server-side selectize is a performance improvement, since it uses R for the search,
    # and we have pretty many species out there. 
    # See: https://shiny.rstudio.com/articles/selectize.html
    updateSelectizeInput(
        session, 'species', 
        choices = species, 
        server = TRUE,
        options = list(onInitialize = I('function() { this.setValue(""); }')))
    
    # output$date_reactive <- renderText({
    #     format(as.POSIXct(formatted_date()),"%d %B %Y")
    # })
    
    output$total_species <- renderText({
        paste0(prettyNum(length(vals), big.mark=","), " species")
    })
    
    output$total_records_count_reactive <- renderText({
        num_events <- r$HGET("num_events", "value")
        paste0(prettyNum(num_events, big.mark=","), " events")
    })
    
    filteredData <- reactive(
    {
        species = input$species
        
        if (species == "")
            return(list("", ""))
        
        coordinates <- r$LRANGE(paste(species, "coord", sep = ":"), 0, -1)
        datetime <- r$LRANGE(paste(species, "datetime", sep = ":"), 0, -1)
        
        # If empty list is returned, most probably vernacular name has been provided,
        # hence, we need to get the scientific name first.
        if (coordinates %>% is_empty())
        {
            sci_name <- r$HGET(
                "vernacular_name:scientific_name", species)
            # If no appropriate scientific name has been found.
            if (sci_name %>% is.null())
                return(list("", ""))
            
            coordinates <-
                r$LRANGE(paste(sci_name, "coord", sep = ":"), 0, -1)
            datetime <-
                r$LRANGE(paste(sci_name, "datetime", sep = ":"), 0, -1)
        }
        
        if (coordinates[[1]] == "" && (length(coordinates) == 1))
            return(list("", ""))
        
        coordinates %<>%
            stringr::str_split(",", simplify = T)
        lat <- coordinates[, 1] %>% as.double()
        long <- coordinates[, 2] %>% as.double()
        
        datetime %<>%
            stringr::str_split(",", simplify = T)
        datetime %<>% tibble::as_tibble()
        
        colnames(datetime) <- c("date", "time")
    
        return(list(tibble::tibble(lat, long), datetime))
    })
    
    output$top_years <- renderPlotly(
    {
        year_plot <- plot_ly(
            data = top_years, 
            x = ~year, 
            y = ~year_share,
            type = "scatter",
            mode = "markers",
            text = ~paste(score, "observations", "<br><b>Year</b>:", year,
                          "<br><b>Share</b>:", year_share, "%"),
            hoverinfo = "text",
            showlegend = FALSE, 
            marker = list(
                size = ~log(score, 1.5),
                # color = brewer.pal(9, input$colors)[9],
                opacity = 0.6,
                line = list(width = 0))
            )
            
        year_plot %<>% config(., scrollZoom = TRUE) %>%
            layout(
                xaxis = list(
                    showgrid = FALSE, 
                    zeroline = FALSE,
                    title = list(
                        text = "Top ten years, by observations",
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
        
        year_plot 
    })
    
    timelineData <- reactive(
    {
        # tryCatch(
        # {
        #     filteredData()[[1]][1, 1]
        # },
        # error = function(e)
        # {
        #     return(tibble(NULL))
        # })
        if (filteredData()[[1]][[1]][[1]] == "")
            return(tibble(NULL))
        
        f_data <- filteredData()
        space_data <- f_data[[1]]
        dt_data <- f_data[[2]]
        
        out <- tibble(
            date = dt_data$date,
            time = dt_data$time,
            lat = space_data$lat,
            long = space_data$long,
            coord = paste(space_data$lat, space_data$long, sep = ", "),
            # 0.25 here is an arbitrary number, which is close to log(1.2, 2).
            # It helps to scale down coordinate differences.
            weight = 0.5
        )
        
        return(out)
    }
    )
    
    # Returns the set of points which are within the map bounds right now.
    observationsInBoundsData <- reactive({
        timeline_data <- timelineData()
        
        if (is.null(input$map_bounds) || timeline_data %>% is_empty())
            return(timeline_data[FALSE, ])
        
        bounds <- input$map_bounds
        lat_rng <- range(bounds$north, bounds$south)
        long_rng <- range(bounds$east, bounds$west)
        
        return(subset(
            timeline_data, 
            lat >= lat_rng[1] & lat <= lat_rng[2] &
                long >= long_rng[1] & long <= long_rng[2]))
    })

    # observeEvent({
    # timeline_data <- observationsInBoundsData()
        
        # if (timeline_data %>% is_empty() %>% not()){
        
        # validate(need(timeline_data %>% is_empty() %>% not(), "None"))
        output$timeline <- renderPlotly(
        {
            timeline_data <- observationsInBoundsData()
            
            if (timeline_data %>% is_empty())
                return()
    
            # Assuming the number of observations for each species is not huge (<= 500),
            # I'm linearly finding each row corresponding to the date provided
            # and assign it a weight (a more optimal solution would include e.g. named lists,
            # but I don't see the need for them here.) Then, cumulative sum for each date
            # is calculated This will let different events to have different y coordinates
            # in the plot (i.e. to e drawn separately).
            calc_weight <- function(date, timeline_data)
            {
                indexes <- (timeline_data$date == date) %>% which()
                timeline_data[indexes, "weight"] <<- cumsum(timeline_data[indexes, "weight"])
            }
            unique_dates <- timeline_data$date %>% unique()
            walk(unique_dates, ~ calc_weight(.x, timeline_data))

            timeline_data$time[map_lgl(timeline_data$time, ~ .x == "")] <- "None"

            # Drawing a plot.
            time_plot <- plot_ly(
                data = timeline_data, x = ~date, y = ~weight,
                type = "scatter",
                # popup text
                hovertemplate = ~paste(
                    "<b>Place</b>:", coord, "<br><b>Time</b>:",
                    time, "<extra></extra>")
                )
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
                        tickfont = list(
                            family = "Courier",
                            size = 13)
                        )
                    )

            time_plot
        }
        )
    # }})
    
    output$map <- renderLeaflet(
    {
        leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 25, zoom = 3)
    })
    
    # observe(
    # {
    #     map_data <- timelineData()
    # 
    #     lat_long_popups <- paste("<strong>Place:</strong>",
    #                              map_data[[1]]$lat,
    #                              map_data[[1]]$long)
    #     datetimes_popups <- paste("<br><strong>Time:</strong>",
    #                               map_data[[2]]$date,
    #                               map_data[[2]]$time)
    # 
    #     map_data[[1]] %<>%
    #         dplyr::mutate(., popup = paste(lat_long_popups, datetimes_popups))
    # 
    #     # colorData <- 1
    #     # pal <- colorFactor("viridis", colorData)
    #     pal <- colorpal()
    # 
    #     leafletProxy("map", data = map_data[[1]]) %>%
    #         clearShapes() %>%
    #         addCircleMarkers(radius = 5,
    #                    stroke = FALSE,
    #                    fillColor = ~pal(2), fillOpacity = 0.4,
    #                    popup = map_data[[1]]$popup)
    # })

}
