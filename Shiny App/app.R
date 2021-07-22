# Imports
library(shiny)
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(leaflet)
library(widgetframe)
library(sp)
library(wbstats)
library(dplyr)
library(tidyr)
library(shinyjs)
library(shinythemes)


## Load Data
# load cached list of sovereign territories 
load(file="countrylist")
# Spatial Data
spdf <- ne_countries(scale = "medium", returnclass = "sp")

# set title of leaflet legend
title_string = "Legend"

# fix widths
# Define UI for application
ui <- 
    navbarPage(id = "navbarpage",title="Bridge",theme = shinytheme("cerulean"),
        tabPanel("Input", 
                 selectInput("birth_country_input", label = h3("Select Birth Country"), 
                             choices = country_df$name),
                 textInput("birth_year_input", label = h3("Birth Year"),value = "1995")),
        tabPanel("Child Mortality",useShinyjs()),
        #tabPanel("Primary School"),
        tabPanel("High School"),
        tabPanel("GDP"),
        tabPanel("About"),
        mainPanel("",
                  fluidPage(fluidRow(column(
                      12,
                      h4(textOutput("text")),
                      p(textOutput("abouttext")),
                      fluidRow(column(6,
                                      fillPage(
                                          h4(textOutput("text1")),
                                          leafletOutput("plots_earlier", height = "60vh")
                                      )),
                               column(6,
                                      fillPage(
                                          h4(textOutput("text2")),
                                          leafletOutput("plots_later", height = "60vh")
                                      )))
                  ))))
    )
        
    
    

# Define server logic required to draw leaflets
server <- function(input, output) {


    # Listens for navbar changes
    # May be a better way to do this, rather than have reduplicate 'hide'
    toListen <- reactive({
        
        if (input$navbarpage=="Input")
        {
            hide("plots_earlier")
            hide("plots_later")
            hide("text")
            hide("text1")
            hide("text2")
            hide("abouttext")
            return(NULL)
        }
        else if (input$navbarpage == "About")
        {
            hide("plots_earlier")
            hide("plots_later")

            hide("text1")
            hide("text2")
            output$text = renderText({"About"})
            
            about_text = "This was made as a quick project to learn Shiny. Data is from the World Bank
            via wbstats. Inspired by Hans Rosling's TED talk, and Gapminder. Note: GDP Per Capita is not 
            the same as income, but is an approximate for the relative standard of living. PPP would be 
            more appropriate, but unfortunately is not available using the API until 1990. "
            output$abouttext = renderText({about_text})
            
            show("text")
            show("abouttext")
            
            return(NULL)
        }
        else
        {
            hide("abouttext")
            hide("plots_earlier")
            hide("plots_later")
            hide("text")
            hide("text1")
            hide("text2")
            return(input$navbarpage)
        }
    })
    
    
    birth_iso3_df <- reactiveValues(birth_iso3 = "")
    
    observeEvent(input$birth_country_input, {
        birth_iso3_df$birth_iso3 = country_df[which(country_df$name==input$birth_country_input),"iso_a3"]
    })
    
    observeEvent(toListen(),{
        
        orig_birth_year = input$birth_year_input
        if (input$navbarpage == "Child Mortality")
        {
            birth_year = orig_birth_year
            variable_string = "SH.DYN.MORT"
        }
        else if (input$navbarpage == "Primary School")
        {
            birth_year = as.numeric(orig_birth_year) + 6
            variable_string = "SE.PRM.ENRR"
        }
        else if (input$navbarpage == "GDP")
        {
            birth_year = as.numeric(orig_birth_year) + 18
            variable_string = "NY.GDP.PCAP.KD"
        }
        else if (input$navbarpage == "High School")
        {
            birth_year = as.numeric(orig_birth_year) + 13
            variable_string = "SE.SEC.ENRR"
        }
        
        # Importing Data
        # Variable Data
        for (i in 1:3)
        {
            tryCatch({
                data = wb_data(indicator = variable_string, mrnev = 1)
                break
            }
            )
        }
        
        
        # controls how many years the data can fill in. e.g. if 1967 has no data, but 1966 does
        year_range = 2
        birth_year = as.numeric(birth_year)
        
        for (i in 1:3)
        {
            tryCatch({
                birth_data = wb_data(indicator = variable_string, country = birth_iso3_df$birth_iso3)
                break
            }, error=function(cond) {
                return()
            }
            )
        }
    
        

        birth_data = birth_data  %>% filter(date <= (birth_year + year_range)) %>% filter(date >= (birth_year - year_range))
        
        # checks if all are NA
        if (sum(!is.na(birth_data[variable_string]))==0)
        {
            birth_value = NA
        }
        else
        {
            birth_data = birth_data %>% drop_na(variable_string) %>% filter(abs(date-as.numeric(birth_year)) == min(abs(date-as.numeric(birth_year)))) %>% select(variable_string)
            birth_data = birth_data[1,]
            birth_value = round(birth_data[[variable_string]],2)
        }
        
        for (i in 1:3)
        {
            tryCatch({
                data_earlier = wb_data(indicator = variable_string, start_date = birth_year)
                break
            }, error=function(cond) {
                return()
            }
            )
        }


            

        
        # Update text:
        if (input$navbarpage == "Child Mortality")
        {
            text_string = paste0("If you were born in ", input$birth_country_input, " in ", input$birth_year_input,
                                 " the chance you would die before age five was: ", birth_value/10, "%"
            )
        }
        else if (input$navbarpage == "Primary School")
        {
            text_string = paste0("When you turned six in ",birth_year,
                                 ", the percent of your peers in elementary school was: ",birth_value, "%"
            )
        }
        else if (input$navbarpage == "GDP")
        {
            text_string = paste0("When you turned eighteen in ",birth_year,
                                 ", the average adult income was: $",
                                 substr(formatC(birth_value, format="f", big.mark=","),0,nchar(formatC(birth_value, format="f", big.mark=","))-2)
            )
        }
        else if (input$navbarpage == "High School")
        {
            text_string = paste0("When you turned thirteen in ",birth_year,
                                 ", the percent of your peers in high school was: ",birth_value, "%"
            )
        }



        # Operations on data for plotting
        if (is.na(birth_value))
        {
            data$is_higher = NA
            data_earlier$is_higher_earlier = NA
        }
        else
        {
            data$is_higher = as.numeric(data[[variable_string]] >= birth_value)
            data_earlier$is_higher_earlier = as.numeric(data_earlier[[variable_string]] >= birth_value)
        }

        
        # rename some columns
        current_variable_string = paste0(variable_string,"_current")
        earlier_variable_string = paste0(variable_string,"_earlier")
        names(data)[names(data) == variable_string] <- current_variable_string
        names(data_earlier)[names(data_earlier) == variable_string] <- earlier_variable_string
    
        
        # Joining data
        #may want to select columns here
        spdf@data = left_join(spdf@data,data, by = c("iso_a3"="iso3c"))
        spdf@data = left_join(spdf@data,data_earlier, by = c("iso_a3"="iso3c"))
        
        # may be duplicating here
        qpal <- colorBin(rev(viridis::viridis(2)),
                         spdf@data[["is_higher"]], bins=2)
        qpal <- colorBin(rev(viridis::viridis(2)),
                         spdf@data[["is_higher_earlier"]], bins=2)
        
        # work around
        labels <- c("Lower","Higher","NA")
        
        
        # fix tooltip, change legends to years
        # Graph
        l <- leaflet(spdf, options =
                         leafletOptions(attributionControl = FALSE, minzoom=1)) %>%
            addPolygons(
                label=~stringr::str_c(
                    name, ' ',
                    formatC(spdf@data[[current_variable_string]], big.mark = ',', format='f')),
                labelOptions= labelOptions(direction = 'auto'),
                weight=1,color='#333333', opacity=1,
                fillColor = ~qpal(spdf@data[["is_higher"]]), fillOpacity = .5,
                highlightOptions = highlightOptions(
                    color='#000000', weight = 2,
                    bringToFront = TRUE, sendToBack = TRUE)
            ) %>%
            addLegend(
                "topright", pal = qpal, values = ~spdf@data[["is_higher"]], labels = c(1:4),
                labFormat = function(type, cuts, p) {  # Here's the trick
                    paste0(labels)
                },
                title = htmltools::HTML(title_string),
                opacity = .5 )
        
        
        l2 <- leaflet(spdf, options =
                         leafletOptions(attributionControl = FALSE, minzoom=1)) %>%
            addPolygons(
                label=~stringr::str_c(
                    name, ' ',
                    formatC(spdf@data[[earlier_variable_string]], big.mark = ',', format='f')),
                labelOptions= labelOptions(direction = 'auto'),
                weight=1,color='#333333', opacity=1,
                fillColor = ~qpal(spdf@data[["is_higher_earlier"]]), fillOpacity = .5,
                highlightOptions = highlightOptions(
                    color='#000000', weight = 2,
                    bringToFront = TRUE, sendToBack = TRUE)
            ) %>%
            addLegend(
                "topright", pal = qpal, values = ~spdf@data[["is_higher_earlier"]], labels = c(1:4),
                labFormat = function(type, cuts, p) {  # Here's the trick
                    paste0(labels)
                },
                title = htmltools::HTML(title_string),
                opacity = .5 )
        
        output$plots_later <- renderLeaflet({
            return(l)


        })
        output$plots_earlier <- renderLeaflet({
            return(l2)
            
        })
        
        output$text = renderText({text_string})
        output$text1 = renderText({as.character(birth_year)})
        output$text2 = renderText({"Current Year"})
        show("plots_later")
        show("plots_earlier")
        show("text")
        show("text1")
        show("text2")

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
