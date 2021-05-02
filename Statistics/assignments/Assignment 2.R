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
str(Dataset_2_1)
Summary_data<-function(y){
  print(summary(y))
  boxplot(y)
}
Summary_data(Dataset_2_1$THC)
Summary_data(Dataset_2_1$CO)
Summary_data(Dataset_2_1$CO2)
Summary_data(Dataset_2_1$NOx)
Summary_data(Dataset_2_1$CH4)
Summary_data(Dataset_2_1$N2O)

###Question 6###
library(ggplot2)
ggplot(Dataset_2_1,aes(THC))+geom_histogram()
ggplot(Dataset_2_1,aes(CO))+geom_histogram()
ggplot(Dataset_2_1,aes(CO2))+geom_histogram()
ggplot(Dataset_2_1,aes(NOx))+geom_histogram()
ggplot(Dataset_2_1,aes(CH4))+geom_histogram()
ggplot(Dataset_2_1,aes(N2O))+geom_histogram()

###Question 7###
install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(readr)
Dataset2<-read.csv("C:/Users/User/Documents/Data Science/Week2/Dataset2.csv",
                      na.strings=c(" ","NA"),
                      stringsAsFactors = FALSE)
head(Dataset2)
library(lubridate)
Dataset2$First.FD.Date<-mdy(Dataset2$First.FD.Date)
Dataset2$Last.FD.Date<-mdy(Dataset2$Last.FD.Date)
Dataset2$FD.termination.date<-mdy(Dataset2$FD.termination.date)
head(Dataset2)

###Question 8###
install.packages("dplyr")
library(dplyr)

tidy_date_1<-Dataset2 %>%
  separate('Date.of.Birth',c("Day","Month","Year")) %>%
  mutate(Yr = case_when(Year > 20~'19',
                      Year <= 20 ~'20'))%>%
  unite(Year,c("Yr","Year"),sep="")
head(tidy_date_1)
tidy_date_1$Month<-match(tidy_date_1$Month,month.abb)
tidy_date_2<-tidy_date_1%>%
  unite(Date_of_Birth,c("Month","Day","Year"),sep="/")
head(tidy_date_2)

###Question 9###
tidy_date_2$First.FD.Date<-as.Date(tidy_date_2$First.FD.Date,format="%d/%m/%Y")
tidy_date_2$Last.FD.Date<-as.Date(tidy_date_2$Last.FD.Date,format="%d/%m/%Y")
tidy_date_2$FD.termination.date<-as.Date(tidy_date_2$FD.termination.date,format="%d/%m/%Y")
tidy_date_2$Date_of_Birth<-as.Date(tidy_date_2$Date_of_Birth,format="%d/%m/%Y")

###Question 10###
tidy_date_3<-tidy_date_2%>%
  mutate(Age=(First.FD.Date - Date_of_Birth)/365)%>%
  separate(Age,(c("Age","Decimal")))%>%
  select(-("Decimal"))
head(tidy_date_3)

