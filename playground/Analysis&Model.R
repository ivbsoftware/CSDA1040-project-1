

library(recommenderlab)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RDSTK)  

######DATA LOAD##########
setwd("C:/Users/mparravani/OneDrive - ArcelorMittal/Data Analytics/GitHub Storage/CSDA1040-project-1")

df<-read.csv("data/jesterfinal151cols.csv/jesterfinal151cols.csv",header = FALSE)
jokes<-read.delim("data/jester_items.tsv",header = FALSE, stringsAsFactors=FALSE)

#NOTES ABOUT DATA
  #Each row is a user (Row 1 = User #1)
  #Each column is a joke (Column 1 = Joke #1)
  #Ratings are given as real values from -10.00 to +10.00
  #99 corresponds to a null rating


#Check format
head(df)

#Rename columns as per their "Joke ID"
names(df)[1]<-"USERID"
for (i in 2:ncol(df))
{
  names(df)[i]<-i
}

#replace all 99 ratings with NA so we can filter them out
df[df==99]<-NA
ratingcount<-nrow(df)-rowSums(is.na(df))

#What's the distribution of rating count? Many jokes probably have 0 ratings.
ggplot() + aes(ratingcount)+ geom_histogram(colour="black", fill="white")+
  ylab("Number of Jokes")+
  xlab("Number of Ratings")

#how many users have given a reasonable amount of ratings?
ratingcount_u<-ncol(df)-colSums(is.na(df))

ggplot() + aes(ratingcount_u)+ geom_histogram(colour="black", fill="white")+
  ylab("Number of Users")+
  xlab("Number of Ratings Given")

summary(ratingcount_u)
summary(ratingcount)

#Get sentiment score for jokes to see if there's any relation to joke rating and sentiment
jokes[,3]<-1
jokes[,4]<-1
colnames(jokes)<-c("ID","Joke","Sentiment","AvgRating")
jokes$ID <- as.factor(jokes$ID)

for (i in 1:nrow(jokes))
{jokes[i,3]<-text2sentiment(jokes[i,2])}

for (i in 1:ncol(df))
{jokes[i,4]<- median(df[,i],na.rm=TRUE)}

ggplot(data=jokes, mapping = aes(x=jokes$Sentiment,y=jokes$AvgRating)) +
  geom_point()+
  ggtitle("Sentiment vs Median Rating")+
  ylab("Median Rating") + xlab("Sentiment Score")

#No discernable trend


######FEATURE ENGINEERING########

#take top half of jokes by rating count
df2<-df[,ratingcount>median(ratingcount)]


#Change to real rating matrix
R<-as.matrix(data)
class(R)
r <- as(R, "realRatingMatrix")
r


