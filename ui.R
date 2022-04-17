library(shiny)
library(leaflet)
library(RColorBrewer)
library(timevis)
library(magrittr)
library(plotly)
library(tibble)


# Define UI for application that draws a histogram
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    leafletOutput("map", width="100%", height="100%"),
    
    absolutePanel(
        id = "controls", 
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
             options = list(placeholder = 'Select species')),
        
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
        
        h5(HTML("<b>Cluster nearby positions on the map?</b>")),      
          
        switchInput(
            inputId = "clustering",
            size = "mini"
        )    
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
            h3(textOutput("total_species"), align = "right"),
            h4(textOutput("total_records_count_reactive"), align = "right"),
            h5(textOutput("date_reactive"), align = "right"),
            
            plotlyOutput("top_years", height = "320px", width = "auto"),
            h5("\n", align = "center"),
            h5("\n", align = "center"),
            plotlyOutput("last_ten_years", height = "220px", width = "auto"),
        )
    ),
    
    conditionalPanel(
        condition = 'input.species != ""',
        absolutePanel(id = "timeline", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE,
                      top = "auto",
                      left = 70,
                      right = 70,
                      bottom = 20,
                      width = "auto",
                      height = 180,
                      style = "opacity: 0.75; padding: 0 5px 5px 5px",

                      h4("Species timeline", align = "center", top = "10px"),
                      
                      # h6(textOutput("num_events"), align = "center"),
                      # TODO h3(tableOutput("db_error"), align = "center"),
                      plotlyOutput("timeline", height = "140px", width = "100%")
         )
    )
)
