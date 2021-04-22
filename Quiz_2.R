# 3831469
# Quiz 2
# 22 April 2021

# Question 1
# load libraries
library(plotly)
library(tidyverse)
library(ggpubr)

# load data
Orange <- datasets::Orange
ToothGrowth <- datasets::ToothGrowth
warpbreaks <- datasets::warpbreaks

# ToothGrowth data
# check normality
Orange_1 <- Orange_2%>% 
  group_by(Tree) %>% 
  summarise(norm_cir = as.numeric(shapiro.test(circumference)[2]))# p value> 0.05 thus normal.

# testing toothgrowth 
? ToothGrowth

#Hypothesis: the Toothlength of the guinea pigs is longer with dose 0f 1mg?day
# than with 0.5 mg/day

# select and slice data
V <- ToothGrowth %>% 
  slice(1:20) %>% 
  select("len", "dose")

# Figure before normality
ggplot(data = V, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose)) +
  labs(x = "Dose (mg/day)", y = "ToothLength")


# normality
ToothGrowth%>% 
  group_by(supp) %>% 
  summarise(norm_supp = as.numeric(shapiro.test(len)[2]))

# Normality
V%>% 
  group_by(dose) %>% 
  summarise(norm= as.numeric(shapiro.test(len)[2])) #thus normal since p > 0.05

# Homoscedas..
V%>% 
  group_by(dose) %>% 
  summarise(sample_len = var(len)) #Homosce.., since the diff isnt more than 4

# perform test
t.test(dose ~ len, data = V, var.equal = TRUE)

# Warpbreaks data
?warpbreaks

# Hypothesis: Wool A has a greater number of breaks when the
# level of tension is high

# select data
Warp <- warpbreaks %>% 
  filter(wool == "A")

# figure

# test normality
Warp%>% 
  group_by(tension) %>% 
  summarise(norm_break = as.numeric(shapiro.test(breaks)[2])) # normal

# Homosced...
Warp%>% 
  group_by(tension) %>% 
  summarise(sample_break = var(breaks)) # pass

# perform t test
t.test(breaks ~ tension, data = Warp, var.equal = TRUE)

# Orange data

#Hypothesis: Tree circumference in tree 4 increase faster with age than in in Tree 5

# Fix data
Oranges <- Orange %>% 
  slice(22:35)

# test normality
Oranges%>% 
  group_by(Tree) %>% 
  summarise(norm_cir = as.numeric(shapiro.test(circumference)[2])) #normal

# test homosced...
Oranges %>% 
  group_by(Tree) %>% 
  summarise(sample_circ = var(circumference)) # fine

# test




# Question 2
# load data
SACTN_data <- read_csv("SACTN_data.csv")

# separate
SACTN <- SACTN_data %>% 
  separate(col = "index", into = c("site", "src"), sep = "/")


