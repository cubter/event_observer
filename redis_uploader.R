library(purrr)
library(magrittr)
library(redux)

args = commandArgs(trailingOnly=TRUE)

if (length(args) != 1) 
{
    stop("File containing occurrences must be supplied as the first arg.\n", call. = FALSE)
} 

cat("Reading file: ", args[1], "\n")

# The csv file must contain the column names.
occurr = read.csv2(
    args[1], 
    sep = ",", 
    stringsAsFactors = F, 
    check.names = T,
    nrows = 20000)

# I'm not conducting file structure checks here because I assume, the file provided
# is correct. But, of course, for a production-ready app tests would be needed. 
occurr = occurr[, c("scientificName", "vernacularName", "latitudeDecimal", "longitudeDecimal", "eventDate", "eventTime")]

cat("\nConstructing indexes. This may take a while.\n")
# Constructs an index, containing key-value pairs, where key is a species' 
# scientific name, and value is a list containing vernacular name, latitude, longitude, 
# date & time of the event. Drawback: usage of global var (index_env).
# Note: perhaps I should have parallelized the loop (using `future` pkg e.g.), but I
# suppose I have a data dependency here.
construct_index <- function(
    scientific_name, vernacular_name, lat, long, event_date, event_time, row_num) 
{
    if (scientific_name == "") 
        return()
    
    if (rlang::env_has(index_env, scientific_name)) 
    {
        index_env[[scientific_name]][[2]] %<>% append(row_num) 
        index_env[[scientific_name]][[3]] %<>% append(lat)      
        index_env[[scientific_name]][[4]] %<>% append(long)
        index_env[[scientific_name]][[5]] %<>% append(event_date)
        index_env[[scientific_name]][[6]] %<>% append(event_time)
        
        return()
    }
    
    index_env[[scientific_name]] <- list(vernacular_name, row_num, lat, long, event_date, event_time)
    names(index_env[[scientific_name]]) <- c(
        "vernacular_name", "row_num", "lat", "long", "event_date", "event_time")
}

index_env <- rlang::env()

purrr::pwalk(list(occurr$scientificName, 
                  occurr$vernacularName, 
                  occurr$latitudeDecimal, 
                  occurr$longitudeDecimal,
                  occurr$eventDate, 
                  occurr$eventTime, 
                  seq_along(occurr$scientificName)
                  ),
             ~ construct_index(..1, ..2, ..3, ..4, ..5, ..6, ..7))

# Functions that return appropriate Redis' functions.

# "Coordinates" is a list, contaning pairs of latitude & longitude.
set_redis_coordinates <- function(env, env_key, lat, long)
{
    if (!is.null(env[[env_key]]$lat) && !is.null(env[[env_key]]$long))
    {
        coord <- paste(env[[env_key]][[lat]], env[[env_key]][[long]], sep = ",")
        
        env[[env_key]][[lat]] <- NULL
        env[[env_key]][[long]] <- NULL
        
        return(redis$LPUSH(paste(env_key, "coord", sep = ":"), coord))
    }
}

# "Datetime" is a list, contaning pairs of date & time.
set_redis_date_time <- function(env, env_key, event_date, event_time)
{
    if (!is.null(env[[env_key]]$event_date) && !is.null(env[[env_key]]$event_time))
    {
        datetime <- paste(env[[env_key]][[event_date]], env[[env_key]][[event_time]], sep = ",")
        
        env[[env_key]][[event_date]] <- NULL
        env[[env_key]][[event_time]] <- NULL
        
        return(redis$LPUSH(paste(env_key, "datetime", sep = ":"), datetime))
    }
}

# "Names" is a hash, contaning pairs of vernac. names (key) & scient. names (value).
set_redis_name_pairs <- function(scientific_name, vernacular_name,
                                 scheme_name)
{
    return(redis$HSET(scheme_name, vernacular_name, scientific_name))
}

increment_redis_num_events <- function(hash_name, val)
{
    if (!r$EXISTS(hash_name))
    {
        r$HSET(hash_name, "value", 0)
    }
    return(r$HINCRBY(hash_name, "value", val))
}

set_years_distribution <- function(sorted_set_name, score, member)
{
    return(r$ZINCRBY(sorted_set_name, score, member))
}

# Setting connection to Redis.
r <- redux::hiredis()
redis <- redux::redis

cat("\nUploading data to Redis. This may take a while.\n")

# Pipelining data to Redis.
# Pipelining events' coordinates.
cmds <- map(occurr$scientificName, 
            ~ set_redis_coordinates(index_env, .x, "lat", "long"))
r$pipeline(.commands = cmds)

# Pipelining events' dates & times.
cmds <- map(occurr$scientificName,
            ~ set_redis_date_time(index_env, .x, "event_date", "event_time"))
r$pipeline(.commands = cmds)
 
# Pipelining species' name pairs.
cmds <- map2(occurr$scientificName, occurr$vernacularName,
             ~ set_redis_name_pairs(.x, .y, "vernacular_name:scientific_name"))
r$pipeline(.commands = cmds)

increment_redis_num_events("num_events", nrow(occurr))

years_distribution <- format(as.Date(occurr$eventDate),"%Y") %>% 
    table()

walk2(years_distribution, names(years_distribution),
      ~ r$ZINCRBY("year_count", .x, .y))

# Setting today as the date of the last DB update.
r$HSET("update_date", "value", Sys.Date() %>% as.character())

cat("Uploading finished\n")

