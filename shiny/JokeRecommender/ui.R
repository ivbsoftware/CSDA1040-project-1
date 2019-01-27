#
# This is the user-interface definition of a Shiny web application.
#

library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(fluidPage(
  theme=shinytheme("cosmo"),
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Jokes Recommender"),
  
  # Sidebar with a slider input for user rating
  sidebarLayout(
    sidebarPanel(
      actionButton("updateRandom", "Pick Random Joke"),  
      actionButton("recommend", "Recommend a Joke"),  
      hr(),              
      sliderInput("rating", "Rate this joke:", min = -10, max = 10, value = 0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
    #  verbatimTextOutput("text", placeholder = T)
      h4 (style = "color:green; font-family:'Comic Sans MS'; text-align: justify;", 
          textOutput("text")
      )
    )
  )
))

