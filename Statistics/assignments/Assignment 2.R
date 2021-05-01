###Question 1###
France<-4
Croatia<-2
if(France>Croatia){
  print("France Wins Croatia" )
}

###Question 2###

mtcars$cyl<-as.factor(mtcars$cyl)
for(i in colnames(mtcars[,8:11])){
  mtcars[[i]]<-as.factor(mtcars[[i]])
}

###Question 3###

colSums(is.na(Dataset_2_1))
str(Dataset_2_1)
colSums(is.na(Dataset_2_1))/1000

###Question 4###
rowSums(is.na(Dataset_2_1))/42

###Question 5###
install.packages("dplyr")
install.packages("gapminder")
library(dplyr)
library(gapminder)
summary(select_if(Dataset_2_1,is.numeric))
png(file="boxplot.png")
boxplot(Dataset_2_1$THC)
boxplot(THC~Year,data=Dataset_2_1,frame=FALSE)
boxplot(Dataset_2_1$CO)
boxplot(CO~Year,data=Dataset_2_1,frame=FALSE)
boxplot(Dataset_2_1$CO2)
boxplot(CO2~Year,data=Dataset_2_1,frame=FALSE)

###Question 6###
library(ggplot2)
hist(Dataset_2_1$THC)
hist(Dataset_2_1$CO)
hist(Dataset_2_1$CO2)
ggplot(Dataset_2_1,aes(x=THC))+geom_histogram()

###Question 7###
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
Data_set_2_2$`First FD Date`<-as.Date(Data_set_2_2$`First FD Date`,format="%d/%m/%Y")
Data_set_2_2$`Last FD Date`<-as.Date(Data_set_2_2$`Last FD Date`,format="%d/%m/%Y")
Data_set_2_2$`FD termination date`<-as.Date(Data_set_2_2$`FD termination date`,format="%d/%m/%Y")

###Question 8###
Data_set_2_2$`Date of Birth`<-as.Date(Data_set_2_2$`Date of Birth`,format="%d/%m/%Y")

###Question 9###
#Don't understand the difference among question 7 to 9

###Question 10###
Age<-Data_set_2_2$`First FD Date`-Data_set_2_2$`Date of Birth`

