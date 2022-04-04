#Making a Model to illustrate how NON-GAME FACTORS influence MLB game attendance



#Cleaning Environment
rm(list = ls())

#Importing Libraries
library("partykit")
library("CHAID")
library("caret")
library("modeldata")
library("dplyr")
library("ggplot2")

#Importing the Dataset
mlb_data = read.csv("C:/Users/kurtl/Downloads/baseball_reference_2016_clean.csv", stringsAsFactors = TRUE)

#Removing Unwanted Columns
mlb_data2 <- mlb_data[c(2,9,16,17,20)]


#Converting Attendance & Temp to range (high, low)
mlb_data2$attendance <-ifelse(mlb_data2$attendance > 25000, "High", "Low")
mlb_data2$temperature <- ifelse(mlb_data2$temperature > 55, "High", "Low")

#converting to Variables Factor
x<-c("temperature", "sky", "game_type", "day_of_week", "attendance") 
mlb_data2[,x]<-lapply(mlb_data2[,x],factor)

#checking Variable is a Factor
is.factor(mlb_data2$temperature)
is.factor(mlb_data2$attendance)

#Decision Tree
control = chaid_control(minbucket = 2000, maxheight = 3)
model = chaid(attendance ~.,
              data = mlb_data2,
              control = control)

#plotting the Decision Tree
plot(model,
     main="MLB Attendance",
     gp=gpar(fontsize=8,
             color="black"))

# Illustrating Accuracy
varimp(model)
print(model)
