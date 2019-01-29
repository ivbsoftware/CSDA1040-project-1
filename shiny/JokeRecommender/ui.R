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
#      helpText("You must rate the joke to see more!"),
#      hr(),
      actionButton("updateRandom", "Pick Random Joke",width = "100%"),  
      p(),              
      actionButton("recommend", "Recommend a Joke",width = "100%"),  
      hr(),              
      sliderInput("rating", "Rate this joke:", min = -10, max = 10, step = 0.01, value = NA)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("msg"),
      h4 (style = "color:green; font-family:'Comic Sans MS'; text-align: justify;", 
          textOutput("text")
      )
    )
  )
))

