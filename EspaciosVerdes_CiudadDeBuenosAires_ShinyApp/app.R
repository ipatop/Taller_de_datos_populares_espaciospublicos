#Script para tener la shiny app de parques
# Autores Ines Patop y Ariel Salgado

# Load packages ----
library(shiny)
library(viridis)
library(RColorBrewer)
require(sf)
require(tidyverse)
require(ggplot2)
require(patchwork)
require(ggmap)
require(osmdata)

# Load data ----
load("data/BAmap.RData")
# Source helper functions -----

# User interface ----
ui <- fluidPage(
  
  fluidRow( 
    column(4, "Seleccion de Variable",varSelectInput(inputId = "var",label =  "Variable:", parques_full[33:42]))
    ,
    column(4, "Seleccion de corte de área", sliderInput(inputId = "range", label = "Rango de área", min=0, max=max(parques_full$area), value = c(5000,10000)))),
 
  fluidRow( column(6, "Mapa de la ciudad",plotOutput("map",width =800,height = 800)),
            column(4,"Distribucuón de la variable",tableOutput("summary")),
            column(4,plotOutput("density")))
  
)

# Server logic ----

server <- function(input, output, session) {
  
  
  output$summary <- renderTable({
    summary(as.data.frame(parques_full) %>% dplyr::filter( area > input$range[1] & area < input$range[2]) %>% dplyr::select(!!input$var) )
  })
  
  output$density <- renderPlot({
    
    ggplot(parques_full  %>% dplyr::filter( area > input$range[1] & area < input$range[2]) ,aes(x=!!input$var))+geom_density()+labs(title = paste("Distribución de",input$var,"de los parques en la ciuidad",subtitle = paste("Area recortada entre",input$range[1],"y",input$range[2],"metros cuadrados")))+ theme_minimal()
   
  }, res = 96)
  
  
  output$map <- renderPlot({
    
    ggplot() + theme_minimal()+ theme(legend.position = c(.8,.1))+
      geom_sf(data=ciudad,fill='white') +
      geom_sf(data=parques_full %>% dplyr::filter( area > input$range[1] & area < input$range[2]),mapping=aes(fill=!!input$var)) +
      scale_fill_viridis_c(option = "plasma") +
      labs(title = paste('Parques de la ciudad coloreados por ',input$var),subtitle = paste("Area recortada entre",input$range[1],"y",input$range[2],"metros cuadrados"))
    
  }, res = 96)
}

# Run app ----
shinyApp(ui, server)