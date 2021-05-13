setwd("~/Data Science/Week3")
getwd()
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("DT")
library(DT)
install.packages("data.table")
library(data.table)
install.packages("ggplot2")
library(ggplot2)
install.packages("moments")
library(moments)
### Step 1 and Step 2###
require(readr)
COVID19 <- read.csv("COVID19.csv", 
                    stringsAsFactors = FALSE,
                    na.strings=c("","NA"))
str(COVID19)
datatable(COVID19)
COVID19_1<-COVID19 %>%
slice(9:229)%>%
select(3:20)  
datatable(COVID19_1)
colSums(is.na(COVID19_1))        
Percentage_of_miss<-function(x){
  (sum(is.na(x))/length(x))*100
}
Missing_Percentage_Col<-apply(COVID19_1,2,Percentage_of_miss)
Missing_Percentage_Col
COVID19_2<-COVID19_1%>%
purrr::discard(~sum(is.na(.x))/length(.x)*100>=5)
datatable(COVID19_2)

###Step3:Rename###
COVID19_Rename<-COVID19_2 %>%
  rename(Country="Country.Other",
         Tot_Cases_1M_pop="Tot.Cases.1M.pop",
         Deaths_1M_pop="Deaths.1M.pop",
         Tests_1M_pop="Tests.1M.pop",
         X1_Caseevery_X_ppl="X1.Caseevery.X.ppl",
         X1_Deathevery_X_ppl="X1.Deathevery.X.ppl",
         X1_Testevery_X_ppl="X1.Testevery.X.ppl")
datatable(COVID19_Rename)
comma_remove<-function(x){
  gsub(",","",x)
}
str(COVID19_Rename)
COVID19_data_character<-as.data.frame(apply(COVID19_Rename,MARGIN=2,FUN=comma_remove))
str(COVID19_data_character)
COVID19_data_character[,2:10]<-lapply(COVID19_data_character[,2:10],as.numeric)
COVID19_data_character[,12:14]<-lapply(COVID19_data_character[,12:14],as.numeric)
COVID19_data_character$Country<-as.factor(COVID19_data_character$Country)
COVID19_data_character$Continent<-as.factor(COVID19_data_character$Continent)
str(COVID19_data_character)
COVID19_After_Cleaning<-COVID19_data_character
str(COVID19_After_Cleaning)
datatable(COVID19_After_Cleaning)

#1.	Create plots for total cases, total death, and total recovery. Explain with a figure for each#
ggplot(COVID19_After_Cleaning,aes(x=TotalCases))+
  geom_histogram(bin=500)
ggplot(COVID19_After_Cleaning,aes(x=TotalDeaths))+
  geom_histogram(bin=500)
ggplot(COVID19_After_Cleaning,aes(x=TotalRecovered))+
  geom_histogram(bin=500)

#2.	Create a plot to examine the correlation between total cases and total population. Explain if there is any correlation between total cases and total population.#
ggplot(COVID19_After_Cleaning,aes(x=TotalCases,y=Population))+
  geom_point()
skewness(COVID19_After_Cleaning$TotalCases)
skewness(COVID19_After_Cleaning$Population,na.rm=TRUE)
median(COVID19_After_Cleaning$TotalCases)
median(COVID19_After_Cleaning$Population,na.rm=TRUE)
IQR(COVID19_After_Cleaning$TotalCases)
IQR(COVID19_After_Cleaning$Population,na.rm = TRUE)
ggplot(COVID19_After_Cleaning,aes(x=TotalCases,y=Population))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point()
#Yes,there is correlation##
#3.	Create a plot to examine the correlation between Tot Cases/1M pop and total population. Explain if there is any correlation between them?#
ggplot(COVID19_After_Cleaning,aes(x=Tot_Cases_1M_pop,y=Population))+
  geom_point()
skewness(COVID19_After_Cleaning$Tot_Cases_1M_pop,na.rm=TRUE)
skewness(COVID19_After_Cleaning$Population,na.rm=TRUE)
median(COVID19_After_Cleaning$Tot_Cases_1M_pop,na.rm=TRUE)
median(COVID19_After_Cleaning$Population,na.rm=TRUE)
IQR(COVID19_After_Cleaning$Tot_Cases_1M_pop,na.rm=TRUE)
IQR(COVID19_After_Cleaning$Population,na.rm=TRUE)
ggplot(COVID19_After_Cleaning,aes(x=Tot_Cases_1M_pop,y=Population))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point()
#No correlation#
#4.	Which column do you feel is better for comparison purposes, total cases or TotCases/1M pop. Explain.#
#Total Cases/1M Population is better as it does not influence by size of the population of the country or continent.#
#5.	Create a plot to examine the correlation between total cases and total death. Explain the figure.#
ggplot(COVID19_After_Cleaning,aes(x=TotalCases,y=TotalDeaths))+
  geom_point()
#There is correlation#
#6.	Create a plot to examine the correlation between total cases and Deaths/1M pop. Explain the figure. Which column is more suitable to compare the result, total death or Death/1Mpop?#
ggplot(COVID19_After_Cleaning,aes(x=TotalCases,y=Deaths_1M_pop))+
  geom_point()
ggplot(COVID19_After_Cleaning,aes(x=TotalCases,y=Deaths_1M_pop))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point()

#There is correlation. Deaths per 1M population is better for comparison.#
#7.	Compare Tot Cases/1M pop by continent, and explain your result.#
COVID19_After_Cleaning%>%
  group_by(Continent)%>%
  na.omit(Continent)%>%
  ggplot(aes(x=Continent,y=Tot_Cases_1M_pop))+
  geom_boxplot()
COVID19_After_Cleaning%>%
  group_by(Continent)%>%
  na.omit(Continent)%>%
  summarize(median_TotalCases_1M=median(Tot_Cases_1M_pop,na.rm=TRUE))
#8.	Compare Deaths/1M pop by continent, and explain your result.$
COVID19_After_Cleaning%>%
  group_by(Continent)%>%
  na.omit(Continent)%>%
  ggplot(aes(x=Continent,y=Deaths_1M_pop))+
  geom_boxplot()
COVID19_After_Cleaning%>%
  group_by(Continent)%>%
  na.omit(Continent)%>%
  summarize(median_Deaths_1M=median(Deaths_1M_pop,na.rm=TRUE))
#9.	Which country is best among testing the COVID19 and which country is worst? There are two columns total test vs. test/M. Choose appropriate column.#
summary(COVID19_After_Cleaning$Tests_1M_pop)
COVID19_After_Cleaning%>%
  filter(Tests_1M_pop==575)
#Yemen is the best#
COVID19_After_Cleaning%>%
  filter(Tests_1M_pop==5540672)
#Gibraltar is the worst#
#Test/M is more appropriate#
#10.	Compare your COVID19 test results by continent? There are two columns total test vs test/M. Choose appropriate column.#
COVID19_After_Cleaning%>%
  group_by(Continent)%>%
  na.omit(Continent)%>%
  ggplot(aes(x=Continent,y=Tests_1M_pop))+
  geom_boxplot()
COVID19_After_Cleaning%>%
  group_by(Continent)%>%
  na.omit(Continent)%>%
  summarize(median_Tests_1M=median(Tests_1M_pop,na.rm=TRUE))
#Test/M is better for comparison#
#11.	Check if Tests/1M pop is skewed or normally distributed.#
skewness(COVID19_After_Cleaning$Tests_1M_pop,na.rm=TRUE)
#Yes, highly skewed#
