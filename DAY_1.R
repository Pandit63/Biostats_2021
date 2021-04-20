#Phiwokuhle Somdaka
#Chapter 2 Biostats
#Working with data classes
#19 April 2021


datasets::BOD

#finding out more on the data
str(BOD)

#More examples on the above
datasets::Nile
str(Nile)
View(Nile)
?Nile

datasets::airquality
str(airquality)

#To load file into environmental pane
pines <- Loblolly
view(pines)
str(pines)#structure of data
class(pines$height)#Looking at a class of a certain column


#Descriptive data

#load data
datasets::ChickWeight

#load library
library(tidyverse)

chicks <- as_tibble(ChickWeight)

nrow(chicks)#different from the true sample size, this is the number of rows not the no of chicks

# how many weights are available across all Diets and Times?
chicks %>% 
  summarise(length = n())

#sample size
unique(chicks$Chick)

#Exercise: calculate the mean of the weights in time 20, within their respective diets
library(tidyverse)
datasets::ChickWeight

chicks%>% 
  filter(Time == 20) %>% 
group_by(Diet) %>% 
  summarise(mean_weight = mean(weight))#the word "mean_weight" is any name you choose for yourself
#you can choose to call it "Somdaka" for all we know.
#OR
chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(mean_weight = sum(weight) / n())

#calculate the kurtosis in day 20 for the different diets
#library
library(e1071)


kurtosis(chicks$weight)#leptokurtic

#Calculating quartiles (by diet)
chicks %>% 
  group_by(Diet) %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))

#Calculating range (by diet)
chicks %>% 
  group_by(Diet) %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

datasets::austres
view(austres)
     