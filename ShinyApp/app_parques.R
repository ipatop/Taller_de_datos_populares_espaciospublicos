#Script para tener la shiny app the parques
library(shiny)
library(viridis)
library(RColorBrewer)
require(sf)
require(tidyverse)
require(ggplot2)
require(patchwork)
require(ggmap)
require(osmdata)

# Load packages ----
library(shiny)
library(maps)
library(mapproj)

# Load data ----
load("data/BAmap.RData")
# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Parques CABA"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Mapea los espacios verdes de la Ciudad de Buenos Aires y permite analyzar variables"),
      
      selectInput("var", 
                  label = "Elegir una variable a mostrar",
                  
                  choices = c("Area", "Perimeter",
                              "ASP", "Paradas de colectivo a menos de 200mts",
                              "Caniles","Juegos Para Niñes","Teatro",
                              "Estación de ejercicio","Canchas",
                              "Baños","Distancia a bicisenda",
                              "Bebedores","Area mayor a media hectarea"),
                  
                  selected = "Area"),
      
      sliderInput("Limit", 
                  label = "Area de los parques interes",
                  min = 0, max = max(parques_full$area), value = c(0, 1000))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    
    data <- switch(input$var, "Area"= names(parques_full)[33], 
                   "Perimeter"=names(parques_full)[34],
                   "ASP"=names(parques_full)[35], 
                   "Paradas de colectivo a menos de 200mts"=names(parques_full)[36],
                   "Caniles"=names(parques_full)[37],
                   "Juegos Para Niñes"=names(parques_full)[38],
                   "Teatro"=names(parques_full)[39],
                   "Estación de ejercicio"=names(parques_full)[40],
                   "Canchas"=names(parques_full)[41],
                   "Baños"=names(parques_full)[42],
                   "Distancia a bicisenda"=names(parques_full)[43],
                   "Bebedores"=names(parques_full)[44],
                   "Area mayor a media hectarea"=names(parques_full)[45]
                   )
    
    
    legend <- switch(input$var, "Area"=names(parques_full)[33], 
                     "Perimeter"=names(parques_full)[34],
                     "ASP"=names(parques_full)[35], 
                     "Paradas de colectivo a menos de 200mts"=names(parques_full)[36],
                     "Caniles"=names(parques_full)[37],
                     "Juegos Para Niñes"=names(parques_full)[38],
                     "Teatro"=names(parques_full)[39],
                     "Estación de ejercicio"=names(parques_full)[40],
                     "Canchas"=names(parques_full)[41],
                     "Baños"=names(parques_full)[42],
                     "Distancia a bicisenda"=names(parques_full)[43],
                     "Bebedores"=names(parques_full)[44],
                     "Area mayor a media hectarea"=names(parques_full)[45]
    )
    
    
    if(is.numeric(data[,var])){
      
      ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
        geom_sf(var=ciudad,fill='white') +
        geom_sf(var=parques_full %>% filter(area>=min & area<=max), mapping=aes(fill= var)) +
        scale_fill_continuous(name= !!sym(var)) +
        ggtitle(paste0('Parques coloreados por ',var))
      
    }else{
      
      ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
        geom_sf(var=ciudad,fill='white') +
        geom_sf(var=parques_full %>% filter(area>=min & area<=max), mapping=aes(fill= !!sym(var))) +
        scale_fill_manual(values = c("darkgrey","darkorange")) +
        ggtitle(paste0('Parques coloreados por ',var))
      
    }
    
  }
  )

}

# Run app ----
shinyApp(ui, server)