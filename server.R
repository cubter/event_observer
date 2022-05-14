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
library(memoise)
library(cachem)

# Initialising connection to Redis.
r <- redux::hiredis()

SHINY_DATA_PATH <- Sys.getenv("SHINY_DATA_PATH")

# Obtaining the list of species. This list is static in the sense that it's updated
# only when the Redis' instance data are updated as well.
species <- read.delim(paste0(SHINY_DATA_PATH, "/species", collapse = ""), 
                      header = F)
species %<>% extract2("V1")

# Reading the files with years stats.
read_years_files <- function(file_path)
{
    if (!file.exists(file_path))
        return(tibble(NULL))
    else
        return(read.csv(file_path))
}
top_years <- read_years_files(paste0(SHINY_DATA_PATH, "/top_years_count.csv", 
                                     collapse = ""))
last_ten_years <- read_years_files(paste0(SHINY_DATA_PATH, "/last_ten_years_count.csv", 
                                         collapse = "")) 
# Total num of events.
num_events_file_path <- paste0(SHINY_DATA_PATH, "/num_events.txt", collapse = "")
num_events <- ifelse(file.exists(num_events_file_path), 
                     read.delim(num_events_file_path, header = F) %>% 
                         extract2("V1"),
                     0)
last_update_date_file_path <- paste0(SHINY_DATA_PATH, "/last_update_date.txt", collapse = "")
# Date of the last DB update.
last_update_date <- ifelse(file.exists(last_update_date_file_path), 
                           read.delim(last_update_date_file_path, header = F) %>% 
                               extract2("V1"),
                           "Unknown")

# Sourcing Rcpp file
sourceCpp("weights_assigner.cpp")

# Caching part. I use disk cache, which is obviously slower, instead of memory one 
# to spare memory in case the app is used by multiple users simultaneously. If this 
# is not the case, and there's plenty of memory, it can always be switched.
# 
# Yes, I know that it doesn't make a lot of sense to cache Redis results, 
# considering that Redis instance is on the same server as the app. However this 
# solution is based on the assumption that they're separated & reading from disk is 
# faster.
# 
# Setting cache directory for memoise.
disk_cache <- cachem::cache_disk(".rcache")

# Functions to cache.
mem_LRANGE <- memoise(r$LRANGE, cache = disk_cache)
mem_HGET <- memoise(r$HGET, cache = disk_cache)
mem_str_split <- memoise(str_split, cache = disk_cache)
mem_as_double <- memoise(as.double, cache = disk_cache)
mem_paste <- memoise(paste, cache = disk_cache)
mem_assign_weights <- memoise(assign_weights, cache = disk_cache)

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
    
    statisticsServer(
        "statistics", top_years, last_ten_years, length(species), num_events, 
        last_update_date, reactive(colorScheme()))
    
    # Processes user's input for species.
    filteredData <- reactive(
    {
        species = input$species
        
        if (species == "")
            return(list())
        
        coordinates <- mem_LRANGE(paste(species, "coord", sep = ":"), 0, -1)
        datetime <- mem_LRANGE(paste(species, "datetime", sep = ":"), 0, -1)
        
        # If empty list is returned, most probably vernacular name has been provided,
        # hence, we need to get the scientific name first.
        if (coordinates %>% is_empty())
        {
            sci_name <- mem_HGET("vernacular_name:scientific_name", species)
            
            # If no appropriate scientific name has been found.
            if (sci_name %>% is.null())
                return(list())
            
            coordinates <- mem_LRANGE(paste(sci_name, "coord", sep = ":"), 0, -1)
            datetime <- mem_LRANGE(paste(sci_name, "datetime", sep = ":"), 0, -1)
        }
        
        if (coordinates[[1]] == "" && (length(coordinates) == 1))
            return(list())
        
        coordinates %<>% mem_str_split(",", simplify = T)
        lat <- coordinates[, 1] %>% mem_as_double()
        long <- coordinates[, 2] %>% mem_as_double()
        
        datetime %<>% mem_str_split(",", simplify = T)
        datetime %<>% as_tibble()
        
        colnames(datetime) <- c("date", "time")
        
        # Setting empty time to "no time".
        datetime$time[(datetime$time == "") %>% which()] <- "no time"
        
        popups <- mem_paste(
            "<b>Place</b>: ", lat, ", ", long, 
            "<br><b>Time</b>: ", datetime$date, ", ", datetime$time,
            sep = "")
        
        return(list(tibble(lat, long), datetime, popups))
    })

    # Shows a notification if the user has selected species with more than 30K observations.    
    observe(
    {
        if (filteredData() %>% is_empty() %>% not())
        {
            if (nrow(filteredData()[[1]]) > 30000)
            {
                showModal(modalDialog(
                    title = "Large dataset",
                    "Wow! You've chosen species with many observations. It will take time for me to depict them.",
                    easyClose = TRUE
                ))
            }
        }
    })
    
    # Constructs a tibble with events data, assigns weights to different events,
    # which are used for the timeline. Returns empty tibble if no data
    # have been found or if input with no species is provided.
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
            popup = f_data[[3]],
            weight = 0.1  # arbitrary number
        )

        # Filtering the dates acc. to user's input
        user_dates <- input$dates
        events_dates <- ymd(out$date)
        start <- ymd(user_dates[1])
        end <- ymd(user_dates[2])
        
        # Default as.Date() has been replaced with lubridate's ymd(), which is
        # on avg. 8 times faster.
        out %<>% subset(.,
            (events_dates <= end | is.na(events_dates)) &
                (events_dates >= start | is.na(events_dates)))
        
        progress$set(50)
        
	    # assign_weights is from the C++ file.
        row_weights <- mem_assign_weights(out$date, seq_along(out$date))
        out$weight[row_weights$rows] <- row_weights$weights
        
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
    
    # Draws teh map with all the points.
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
    
    # Responsible for drawing the timeline dynamically based on the map's bounds.
    timelineServer(
        "events_timeline", reactive(observationsInBoundsData()), reactive(colorScheme()))
    
    updateProgressBar(session = session, id = "pb0", value = 100)
    
    # Downloads the .csv file with all the observations for the selected species.
    output$download <- downloadHandler(
        filename = function()
        {
            return(paste0("events_data_", input$species, ".csv"))
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
