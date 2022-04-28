#load libraries
library(lubridate)
library(ggplot2)
library(tidyverse)
library(MASS)
library(dplyr)
#install.packages("leaps")
library(leaps)
#load data, print out column names
hate_crimes<-read.csv("NYPD_Hate_Crimes.csv")
#replace blank values with "NA"; Arrest Id and Arrest Date each have 860 NA values
hate_crimes[hate_crimes == ""] <- NA 
#turn Arrest Date into Date class 
hate_crimes$Arrest.Date<-as.Date(hate_crimes$Arrest.Date,format="%m/%d/%Y")
#Add another column to signify if arrested or not (arrested=1, not arrested = 0)
hate_crimes$ArrestStatus <- ifelse(is.na(hate_crimes$Arrest.Date), 1, 0)

#factors columns
hate_crimes <- hate_crimes %>% mutate_if(is.character,as.factor)
is.factor(hate_crimes$Complaint.Precinct.Code)
hate_crimes$Record.Create.Date<-as.Date(hate_crimes$Record.Create.Date,format="%m/%d/%Y")
#Delete Arrest Date Col
hate_crimes2 <-  subset(hate_crimes, select = -c(Arrest.Date, Arrest.Id) )
# shuffle
shuffled_hate_crimes<-hate_crimes[sample(1:nrow(hate_crimes)), ]
#divides into training/test
train<-shuffled_hate_crimes[1:1000,]
test<-shuffled_hate_crimes[1001:1296,]

      
#running full model 
model1 <- glm(ArrestStatus ~ .,family=binomial(link='logit'),data=train, maxit=100)
summary(model1)
model2<-stepAIC(model1, direction="backward")
summary(model2)
str(train)
#rename levels of boros (Patrol Borough Name)
levels(hate_crimes$Patrol.Borough.Name)[1]<-"North Brooklyn"
levels(hate_crimes$Patrol.Borough.Name)[2]<-"South Brooklyn"
levels(hate_crimes$Patrol.Borough.Name)[3]<-"Bronx"
levels(hate_crimes$Patrol.Borough.Name)[4]<-"North Manhattan"
levels(hate_crimes$Patrol.Borough.Name)[5]<-"South Manhattan"
levels(hate_crimes$Patrol.Borough.Name)[6]<-"North Queens"
levels(hate_crimes$Patrol.Borough.Name)[7]<-"South Queens"
levels(hate_crimes$Patrol.Borough.Name)[8]<-"Staten Island"


#predict if they were arrested or not based on variables in the dataset 
