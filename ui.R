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

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    theme = bs_theme(version = 4, bootswatch = "flatly"),
    
    # Depicts progress bar while the app is loading
    absolutePanel(
        id = "progress_panel",
        fixed = TRUE,
        draggable = FALSE,
        top = 0,
        height = 0,
        bottom = "auto",
        left = 0,
        right = 0,
        style = "opacity: 0.9;transition: opacity 500ms 1s; color: #FFFFFF",
        progressBar(
            id = "pb0",
            value = 30,
            size = "xs",
            display_pct = TRUE,
            striped = TRUE)
    ),
    
    # Handles all the map-related stuff being a single point of access.
    mapUI("map"),
    
    absolutePanel(
        id = "species_observer", 
        class = "panel panel-default", 
        fixed = TRUE, 
        draggable = TRUE, 
        top = 0, 
        left = "auto", 
        right = 20, 
        bottom = "auto",
        width = 330, 
        height = "auto",
        style = "opacity: 0.75; padding: 0 20px 20px 20px;transition: opacity 500ms 1s;",
        
        h3("Species observer"),
        
        selectizeInput(
            "species", 
             label = "Search species.", 
             choices = NULL, 
             options = list(
                 placeholder = 'Select species')
            ),
        
        dateRangeInput(
            "dates", 
            label = h5("Choose dates"),
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
        
        h6(HTML("<b>Cluster nearby events on the map?</b><br><br>Warning: for large number of events switching it off may make the map render for a very long time.")),
          
        switchInput(
            inputId = "clustering",
            size = "mini",
            # By def., clustering is on, since there are species with hundreds
            # of thousands of events.
            value = TRUE
        ),  
        
        downloadButton("download", "Download selected species' events")
    ),
    
    
        absolutePanel(
            id = "stat_panel",
            class = "panel panel-default",
            fixed = TRUE,
            draggable = TRUE,
            top = 0,
            left = 20,
            right = "auto",
            height = 200,
            width = 430,
            style = "opacity: 0.75; padding: 0px 80px 20px 20px",
            
            conditionalPanel(
                condition = 'input.species == ""',
                h3("Statistics", align = "right"),
                statisticsUI("statistics")
            )
        ),
    
    # Button allowing to hide the timeline's panel.
    conditionalPanel(
        condition = 'input.species != ""',
        absolutePanel(
            id = "hide_timeline_panel",
            class = "panel panel-default",
            fixed = TRUE,
            draggable = FALSE,
            top = "auto",
            left = 40,
            right = 40,
            bottom = 200,
            width = "auto",
            height = 50,
            style = "opacity: 0.75; padding: 0 10px 5px 5px",
            prettyToggle(
                inputId = "hide_timeline",
                label_on = HTML("<b>Show timeline!</b>"), 
                icon_on = icon("check"),
                fill = FALSE,
                shape = "curve",
                status_on = "info",
                status_off = "danger", 
                label_off = HTML("<b>Hide timeline!</b>"),
                icon_off = icon("times"),
                animation = "pulse")
        )
    ),
    
    conditionalPanel(
        condition = 'input.species != "" && input.hide_timeline != true',
        # Handles all the timeline-related stuff being a single point of access.
        timelineUI("events_timeline")
    )
)
