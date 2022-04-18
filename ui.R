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


# Define UI for application that draws a histogram
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    # Handles all the map-related stuff being a single point of access. 
    mapUI("events"),
    
    absolutePanel(
        id = "species_observer", 
        class = "panel panel-default", 
        fixed = TRUE, 
        draggable = TRUE, 
        top = 60, 
        left = "auto", 
        right = 20, 
        bottom = "auto",
        width = 330, 
        height = "auto",
        style = "opacity: 0.75; padding: 0 20px 20px 20px;transition: opacity 500ms 1s;",
        
        h2("Species observer"),
        
        selectizeInput(
            "species", 
             label = "Search species.", 
             choices = NULL, 
             options = list(
                 placeholder = 'Select species', 
                 onInitialize = I('function() { this.setValue(""); }')
                 )
            ),
        
        dateRangeInput(
            "dates", 
            label = h5("Choose dates (optional)"),
            start = "1000-01-01",
            min = "1000-01-01",
            max = Sys.Date(),
            end = Sys.Date(),
            format = "yyyy-MM-dd",
            startview = "year"),
        
        selectizeInput(
            "colors", 
            "Color Scheme",
            rownames(subset(brewer.pal.info, category %in% c("seq")))),
        
        h5(HTML("<b>Cluster nearby events on the map?</b>")),      
          
        switchInput(
            inputId = "clustering",
            size = "mini"
        ),  
        
        downloadButton("download", "Download selected species' events")
    ),
    
    conditionalPanel(
        condition = 'input.species == ""',
        absolutePanel(
            id = "statistics",
            class = "panel panel-default",
            fixed = TRUE,
            draggable = TRUE,
            top = 60,
            left = 20,
            right = "auto",
            bottom = "auto",
            width = 350,
            height = "auto",
            style = "opacity: 0.75; padding: 0 10px 20px 20px",

            h3("Statistics", align = "right"),
            statisticsUI("statistics")
        )
    ),
    
    conditionalPanel(
        condition = 'input.species != ""',
        # Handles all the timeline-related stuff being a single point of access.
        timelineUI("events_timeline")
    )
)
