library(shiny)
library(leaflet)
library(rlang)
library(RColorBrewer)
library(purrr)
library(magrittr)
library(plotly)
library(tibble)
library(shinyWidgets)
library(dplyr)
library(redux)
library(stringr)
library(Rcpp)
library(bslib)
library(lubridate)

# Initialising connection to Redis.
r <- redux::hiredis()

# Obtaining vernacular & scientific names from the hash table.
vernac_names <- r$HKEYS("vernacular_name:scientific_name")
sci_names <- r$HVALS("vernacular_name:scientific_name")
species <- c(vernac_names, sci_names) %>% unlist()

# Counting the top years, by events.
# It seems Redux just doesn't know yet that ZREVRANGE is deprecated.
year_count <- r$ZREVRANGE("year_count", 0, -1, "WITHSCORES")

get_top_years <- function(year_count)
{
    if (year_count %>% is_empty())
        return(tibble(NULL))
    
    if ((year_count %>% length()) < 20)
        top_year_count <- year_count
    else
        top_year_count <- year_count[1:20]
    
    top_year_indexes <- map2_lgl(
        top_year_count, seq_along(top_year_count), ~ (.y %% 2 == 1))
    
    top_years <- tibble(year = top_year_count[top_year_indexes] %>% unlist() %>% 
                            as.integer(),
                        score = top_year_count[!top_year_indexes] %>% unlist() %>% 
                            as.integer())
    
    top_years$year_share = (top_years$score/sum(top_years$score) * 100) %>% 
        round(., 1)
    
    return(top_years)
}
top_years <- get_top_years(year_count)

get_last_years <- function(year_count)
{
    if (year_count %>% is_empty())
        return(tibble(NULL))
    
    # Counting the last 10 years, by events.
    all_year_indexes <- map2_lgl(year_count, seq_along(year_count), ~ (.y %% 2 == 1))
    all_years <- tibble(year = year_count[all_year_indexes] %>% unlist() %>% 
                            as.integer(),
                        score = year_count[!all_year_indexes] %>% unlist() %>% 
                            as.integer())
    # Default method (radix sort) is OK, as the num of years is not big
    last_ten_years <- filter(all_years, year %in% (year %>% sort(decreasing = T))[1:10])
}
last_ten_years <- get_last_years(year_count)

num_events <- r$HGET("num_events", "value")         # total num of events
last_update_date <- r$HGET("update_date", "value")  # last update's date

# Sourcing Rcpp file
sourceCpp("weights_assigner.cpp")

server <- function(input, output, session) 
{
    updateProgressBar(session = session, id = "pb0", value = 50)
    
    # Server-side selectize is a performance improvement, since it uses R for the search,
    # and we have pretty many species out there. 
    # See: https://shiny.rstudio.com/articles/selectize.html
    updateSelectizeInput(
        session, 
        'species', 
        choices = species, 
        server = TRUE,
        selected = "Parus minor"
    )

    # Returns color acc. to the user's selection.
    colorScheme <- reactive(
    {
        # 7 is just dark enough to be well seen on the map.
        if (input$colors != "")
            return(brewer.pal(7, input$colors)[7])
        else 
            return("#FFFFFF")
    }
    )    
    
    test <- reactive({
        out = input$plot_selector
        out})
    
    statisticsServer(
        "statistics", top_years, last_ten_years, length(sci_names), num_events, 
        last_update_date, reactive(colorScheme()))
    
    # Processes user's input for species.
    filteredData <- reactive(
    {
        species = input$species
        
        if (species == "")
            return(list())
        
        coordinates <- r$LRANGE(paste(species, "coord", sep = ":"), 0, -1)
        datetime <- r$LRANGE(paste(species, "datetime", sep = ":"), 0, -1)
        
        # If empty list is returned, most probably vernacular name has been provided,
        # hence, we need to get the scientific name first.
        if (coordinates %>% is_empty())
        {
            sci_name <- r$HGET("vernacular_name:scientific_name", species)
            
            # If no appropriate scientific name has been found.
            if (sci_name %>% is.null())
                return(list())
            
            coordinates <-
                r$LRANGE(paste(sci_name, "coord", sep = ":"), 0, -1)
            datetime <-
                r$LRANGE(paste(sci_name, "datetime", sep = ":"), 0, -1)
        }
        
        if (coordinates[[1]] == "" && (length(coordinates) == 1))
            return(list())
        
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
    
    observe(
    {
        if (filteredData() %>% is_empty() %>% not())
        {
            if (nrow(filteredData()[[1]]) > 30000)
            {
                # Save the ID for removal later
                showModal(modalDialog(
                    title = "Large dataset",
                    "Wow! You've chosen species with many observations. It will take time for me to depict them.",
                    easyClose = TRUE
                ))
            }
        }
    })
    
    # Constructs a tibble with events data, assigns weights to different events,
    # which are used later for the timeline. Returns empty tibble if no data
    # have been found in the DB or if an empty species input is provided.
    eventsData <- reactive(
    {
        if (filteredData() %>% is_empty())
            return(tibble(NULL))
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Calculating data", value = 0)
        
        f_data <- filteredData()
        space_data <- f_data[[1]]
        dt_data <- f_data[[2]]

        out <- tibble(
            date = dt_data$date,
            time = dt_data$time,
            lat = space_data$lat,
            long = space_data$long,
            coord = paste(space_data$lat, space_data$long, sep = ", "),
            weight = 0.1  # arbitrary number
        )

        # Filtering the dates acc. to user's input
        user_dates <- input$dates
        events_dates <- out$date %>% ymd()
        start <- ymd(user_dates[1])
        end <- ymd(user_dates[2])
        
        # Default as.Date() has been replaced with lubridate's ymd(), which is
        # on avg. 8 times faster.
        out %<>% subset(.,
            (events_dates <= end | is.na(events_dates)) &
                (events_dates >= start | is.na(events_dates)))
        
        progress$set(50)
        
        row_weights <- assign_weights(out$date, seq_along(out$date))
        out$weight[row_weights$rows] <- row_weights$weights

        # Setting empty time to "None"
        out$time[map_lgl(out$time, ~ .x == "")] <- "None"
        
        return(out)
    }
    )
    
    updateProgressBar(session = session, id = "pb0", value = 65)
    
    # Returns leaflet::markerClusterOptions(), if clustering is enabled, which are 
    # to be used for clustering events on the map.
    clustering <- reactive(
    {
        if (!input$clustering)
            return(NULL)
        else
            return(markerClusterOptions())
    }
    )
    
    mapServer(
        "map", 
        reactive(eventsData()), 
        reactive(colorScheme()), 
        reactive(clustering())
        )
    
    updateProgressBar(session = session, id = "pb0", value = 80)

    # Returns the set of points which are within the user's map bounds.
    observationsInBoundsData <- reactive(
    {
        timeline_data <- eventsData()
        # Making this work appeared to be very non-trivial for me.
        bounds <- input[[NS("map", "map_bounds")]]
        
        if (is.null(bounds) || timeline_data %>% is_empty())
            return(timeline_data[FALSE, ])

        lat_rng <- range(bounds$north, bounds$south)
        long_rng <- range(bounds$east, bounds$west)
        
        out <- subset(
            timeline_data, lat >= lat_rng[1] & lat <= lat_rng[2] & 
                long >= long_rng[1] & long <= long_rng[2])
        
        return(out)
    })
    
    # Responsible for drawing the timeline dynamically based on the map's bounds
    timelineServer(
        "events_timeline", reactive(observationsInBoundsData()), reactive(colorScheme()))
    
    updateProgressBar(session = session, id = "pb0", value = 100)
    
    # Downloader.
    output$download <- downloadHandler(
        filename = function()
        {
            return("events_data.csv")
        },
        content = function(file)
        {
            if (eventsData() %>% is_empty())
            {
                data <- tibble(lat = "", long = "", date = "", time = "")
            }
            else
            {
                data <- eventsData()[, c("lat", "long", "date", "time")]
                data %<>% dplyr::mutate(., input$species)
            }

            write.csv(data, file, row.names = FALSE)
        }
    )

}
