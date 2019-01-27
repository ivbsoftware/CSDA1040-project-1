#
# This is the user-interface definition of a Shiny web application.
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Jokes Recommender"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("rating",
                   "Rate this joke:",
                   min = -10,
                   max = 10,
                   value = 0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
    #  verbatimTextOutput("text", placeholder = T)
      h4(textOutput("text"))
    )
  )
))

