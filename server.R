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
last_ten_years <- filter(all_years, year %in% (year %>% sort(decreasing = T))[1:10])

num_events <- r$HGET("num_events", "value")
last_update_date <- r$LRANGE("update_date", 0, -1)

server <- function(input, output, session) 
{
    # Server-side selectize is a performance improvement, since it uses R for the search,
    # and we have pretty many species out there. 
    # See: https://shiny.rstudio.com/articles/selectize.html
    updateSelectizeInput(
        session, 'species', 
        choices = species, 
        server = TRUE
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
        "statistics", top_years, last_ten_years, length(vals), num_events, 
        last_update_date)
    
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
    
    # Constructs a tibble with events data, assigns weights to different events,
    # which are used later for the timeline. Returns empty tibble if no data
    # have been found in the DB or if an empty species input is provided.
    eventsData <- reactive(
    {
        if (filteredData() %>% is_empty())
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
            weight = 0.25
        )

        # Filtering the dates acc. to user's input
        user_dates <- input$dates
        events_dates <- as.Date(out$date)
        start <- as.Date(user_dates[1])
        end <- as.Date(user_dates[2])

        out %<>% filter(
            (events_dates <= end | is.na(events_dates)) &
                (events_dates >= start | is.na(events_dates)))

        # Assuming the number of observations for species is not huge  (<= 500) on avg.,
        # I'm linearly finding (a more optimal solution would include e.g. named lists,
        # but I don't see the need for them here.) each row corresponding to the date
        # provided and assign it a weight  Then, cumulative sum for each date
        # is calculated. This will let different events to have different y coordinates
        # in the plot (i.e. to e drawn separately).
        calc_weight <- function(date)
        {
            indexes <- (out$date == date) %>% which()
            out[indexes, "weight"] <<- cumsum(out[indexes, "weight"])
        }
        unique_dates <- out$date %>% unique()
        walk(unique_dates, calc_weight)

        # Setting empty time to "None"
        out$time[map_lgl(out$time, ~ .x == "")] <- "None"

        return(out)
    }
    )
    
    # Returns leaflet::markerClusterOptions(), if clustering is enabled to be used for
    # clustering events on the map.
    clustering <- reactive(
    {
        if (!input$clustering)
            return(NULL)
        else
            return(markerClusterOptions())
    }
    )
    
    ns <- shiny::NS("events")
    mapServer(
        "events", 
        reactive(eventsData()), 
        reactive(colorScheme()), 
        reactive(clustering())
        )
    
    # Returns the set of points which are within the user's map bounds
    observationsInBoundsData <- reactive(
    {
        timeline_data <- eventsData()
        bounds <- input[[ns("map_bounds")]]
        
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
