library(shiny)
library(bslib)
library(palmerpenguins)
library(ggplot2)
library(tidyverse)

dataset <- penguins |> na.omit()

ui <- page_sidebar( # try also page_navbar
  title = "My first app",
  sidebar = sidebar(
    selectInput(inputId = "species", label = "Species", choices = unique(dataset$species))
  ),
  # outputs
  card(plotOutput("myplot")) # try also textOutput and tableOutput
)

server <- function(input, output) {

  output$myplot <- renderPlot({
    dataset |> filter(species == input$species) |> 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
      geom_point() + theme_bw() 
      
  })
}

shinyApp(ui = ui, server = server)


