#
# This is the server logic of a Shiny web application. You can run the 
#

library(shiny)
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

# initialze this user ratings vector
ratings <- rep(NA, jokesNum) 
#ratings <- sample(c(NA,0:5),jokesNum, replace=TRUE, prob=c(.7,rep(.3/6,6)))


# function returns a random index of next joke not rated by this user
next_random_joke_id <- function() {
  nextJokeId <- -1
  randIndexes <- sample(1:150,150,replace = FALSE)
  for (i in randIndexes) {
    if (is.na(ratings[i])) {
      nextJokeId <- i
      break
    }
  }
  return(nextJokeId)
}

# predict next joke id
predict_joke_id <- function(method) {

  ## coerce ratings into a realRatingMAtrix
  m <- matrix(ratings,
              nrow=1, ncol=(jokesNum), dimnames = list(
                user=paste('u', 1:1, sep=''),
                item=paste('i', 1:jokesNum, sep='')
              ))
  rm <- as(m, "realRatingMatrix")
  
  recNum <- predict(recommender, rm, n = 1)
  recNum <-as(recNum, "list")

  return(as.numeric(recNum[[1]]))
}

# find next joke object
next_joke_id <- function(method) {
  if (method == "RANDOM") {
    nextJokeId <- next_random_joke_id()
  } else {
    nextJokeId <- predict_joke_id(method)
  }
}


# server logic
shinyServer(function(input, output) {
   
  output$text <- renderText({
    return(
      jokes[[next_joke_id("RANDOM"),"Joke"]]
    )
  })

})
