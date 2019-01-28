#
# This is the server logic of a Shiny web application. You can run the 
#

library(shiny)
library(shinyjs)

library("recommenderlab")
library("ggplot2")
library("RDSTK")
library("Matrix")

## Reading in Data ---------------------------------------------------------------------

# Load pre-trained recommender object
recommender <- readRDS("jokeRecommender.Rds") 

# load pre-build jokes object
jokes <- readRDS("jokes.Rds") 
jokesNum <- nrow(jokes)

# function returns a random index of next joke not rated by this user
next_random_joke_id <- function(rts) {
  if (is.null(rts)) {
    return(sample.int(150,1))
  }
  nextJokeId <- -1
  randIndexes <- sample(1:150,150,replace = FALSE)
  for (i in randIndexes) {
    if (is.na(rts[i])) {
      nextJokeId <- i
      break
    }
  }
  return(nextJokeId)
}

# function recommends next joke id
predict_joke_id <- function(method, rts) {
  
  # if no ratings vector not yet initialized
  if (is.null(rts)) {
    return(sample.int(150,1))
  }
  
  # coerce ratings into a realRatingMAtrix
  m <- matrix(rts,
              nrow=1, ncol=(jokesNum), dimnames = list(
                user=paste('u', 1:1, sep=''),
                item=paste('i', 1:jokesNum, sep='')
              ))
  rm <- as(m, "realRatingMatrix")
  
  recNum <- predict(recommender, rm, n = 1)
  recNum <- as(recNum, "list")

  return(as.numeric(recNum[[1]]))
}

# function count number ratings by current user
ratedNum <- function(rts) {
  return (sum(!is.na(rts)))
}

MIN_NUMBER_OF_RATINGS <- 5

# funcion updates screen values
updateUI <- function(input, output, session, jid, rtNum) {
  # output joke text
  output$text <- renderText({
    return(jokes[[jid,"Joke"]])
  })
  
  # set slider to the average rating of this joke
  updateSliderInput(session, "rating", value = jokes[[jid,"AvgRating"]])  

  # enable "recommend" button after some ratings
  if (rtNum > MIN_NUMBER_OF_RATINGS) {
    shinyjs::show("recommend")
  } else {
    shinyjs::hide("recommend")
  }

  # to force user move the slider  
  shinyjs::disable("updateRandom")
  shinyjs::disable("recommend")
}

# server logic
shinyServer(function(input, output, session) {

  # init flags
  countFlag <- reactiveVal(0)
  disableButtonsFlag <- reactiveVal(TRUE)
  
  # init ratings vector session variable  
  ratings <- reactiveVal(rep(NA, jokesNum))

  # init currentJokeId session variable  
  id <- next_random_joke_id(NULL)
  currentJokeId <- reactiveVal(id)
  
  # pick first joke at start randomly
  updateUI(input, output, session, id, 0)
  
  # callback - 'updateRandom' button clicked 
  observeEvent(input$updateRandom, {
    id <- next_random_joke_id(ratings())
    currentJokeId(id)
    updateUI(input, output, session, id, ratedNum(ratings()))
    disableButtonsFlag(TRUE)
  })
  
  # callback - 'recommend' button clicked
  observeEvent(input$recommend, {
    rts <- ratings()
    id <- predict_joke_id("",rts)
    currentJokeId(id)
    updateUI(input, output, session, id, ratedNum(rts))
    disableButtonsFlag(TRUE)
  })
  
  # callback - slider changes, update the user rating vector
  observeEvent(input$rating, {
    jid <- currentJokeId()
    avg <- jokes[[jid,"AvgRating"]]
    #print (paste("slider=", input$rating, ", currentJokeId=", jid, ", average=", avg))

    # update this user ratings
    ratingsVec <- ratings()
    ratingsVec[jid] <- input$rating
    ratings(ratingsVec)
    #print(ratings())

    # now user can ask for a new joke
    count <- countFlag() + 1
    if (!disableButtonsFlag() && count > 2) {
      shinyjs::enable("updateRandom")
      shinyjs::enable("recommend")
    }
      
    disableButtonsFlag(FALSE)
    countFlag(count)
  })
  
})
