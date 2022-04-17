library(shiny)
library(leaflet)
library(rlang)
library(RColorBrewer)
library(purrr)
library(magrittr)
library(plotly)
library(tibble)
library(shinyWidgets)

r <- redux::hiredis()

keys <- r$HKEYS("vernacular_name:scientific_name")
vals <- r$HVALS("vernacular_name:scientific_name")
species <- c(keys, vals) %>% unlist()

# It seems redux just doesn't know yet that ZREVRANGE is deprecated.
top_years_count <- r$ZREVRANGE("year_count", 0, 9, "WITHSCORES")
year_indexes <- map2_lgl(top_years_count, seq_along(top_years_count),
                         ~ (.y %% 2 == 1))
top_years <- tibble(year = top_years_count[year_indexes] %>% as.integer(),
                    score = top_years_count[!year_indexes] %>% as.integer())
top_years$year_share = (top_years$score/sum(top_years$score) * 100) %>% 
    round(., 1)

all_years_count <- r$ZREVRANGE("year_count", 0, -1, "WITHSCORES")
year_indexes <- map2_lgl(all_years_count, seq_along(all_years_count),
                         ~ (.y %% 2 == 1))
all_years <- tibble(year = all_years_count[year_indexes] %>% as.integer(),
                    score = all_years_count[!year_indexes] %>% as.integer())
last_ten_years %<>% filter(., year %in% (year %>% sort(decreasing = T))[1:10])

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
    
    output$date_reactive <- renderText({
        format(Sys.Date(),"%d %B %Y")
    })
    
    output$total_species <- renderText({
        paste0(prettyNum(length(vals), big.mark=","), "</b> species")
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
                opacity = 0.6,
                line = list(width = 0))
            )
            
        year_plot %<>% config(., scrollZoom = TRUE) %>%
            layout(
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
        
        year_plot 
    })
    
    output$last_ten_years <- renderPlotly(
    {
        year_plot <- plot_ly(
            data = last_ten_years, 
            color = I(brewer.pal(7, "Blues")[6]),
            type = "bar",
            showlegend = FALSE
        )
        year_plot %<>% add_bars(
            x = ~year, 
            y = ~log(score, 2),
            width = 0.3,
            hovertemplate = ~paste(
                "<b>Year</b>:", year, "<br><b>Count</b>:",
                score, "<extra></extra>"),
            marker = list(opacity = 0.7)
        )
        
        year_plot %<>% config(., scrollZoom = TRUE) %>%
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
        
    
    
    # validate(need(timeline_data %>% is_empty() %>% not(), "None"))
    output$timeline <- renderPlotly(
    {
        timeline_data <- observationsInBoundsData()
        
        # TODO: proper handling
        if (timeline_data %>% is_empty())
            return()
        
        # Converts vector ("date_vec") values to dates and compares each value with 
        # "date" returning a logical vector. 
        compare_range <- function(date_vec, op, date) 
        { 
            date_vec %<>% as.Date()
            date %<>% as.Date()
            out <- is.na(date_vec) | op(date_vec, dt)
            return(out) 
        }
        
        user_dates <- input$dates 
        events_dates <- as.Date(timeline_data$date) 
        start <- as.Date(user_dates[1])
        end <- as.Date(user_dates[2])
        
        timeline_data %<>% 
            filter(., (events_dates <= end | is.na(events_dates)) &
                       (events_dates >= start | is.na(events_dates)))
        
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
    
    num_events <- renderText(
    {
        if (!is.null(timelineData()))
            nrow(timelineData())
    }
    )
    
    # Reactive expression for color.
    colorScheme <- reactive(
    {
        # 7 is just dark enough to be well seen on the map.
        if (input$colors != "")
            return(brewer.pal(7, input$colors)[7])
        else 
            return("#FFFFFF")
    }
    )
    
    output$map <- renderLeaflet(
    {
        # Static maps without markers, hence leaflet() instead of leafletProxy().
        leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 25, zoom = 3)
    })
    
    clustering <- reactive(
    {
        # If clustering is enabled, perform leaflet's clusering.
        if (!input$clustering)
            return(NULL)
        else
            return(markerClusterOptions())
    }
    )
    
    observe(
    {
        map_data <- timelineData()
        
        if (map_data %>% is_empty())
            return(FALSE)

        lat_long_popups <- paste("<b>Place</b>:", map_data$coord)
        datetimes_popups <- paste("<br><strong>Time:</strong>",
                                  map_data$date,
                                  map_data$time)

        map_data %<>%
            dplyr::mutate(., popup = paste(lat_long_popups, datetimes_popups))

        pal <- colorFactor(palette = input$colors, domain = input$species,
                           levels = 1)
        
        leafletProxy("map", data = map_data) %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            addCircleMarkers(
                radius = ~1 / log(length(lat), 1000000), # radius is reversibly proportional to the number of events 
                clusterOptions = clustering(),
                stroke = FALSE,
                fillColor = colorScheme(), 
                fillOpacity = 0.6,
                popup = map_data$popup)
    })

}
