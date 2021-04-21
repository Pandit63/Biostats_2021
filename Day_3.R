# Day 3
# linear regression and correlation
# 21 April 2021
# Phiwokuhle Somdaka

# linear regression

# library
library(tidyverse)

#load data
head(faithful)


eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

slope <- round(eruption.lm$coef[2], 3)
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

# graph of linear regression
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)")
       y = "Eruption duration (minutes)")



# Correlation
# Assumption
# Pairwise data???? research this

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot) #new package! this plots correlation.

# Load data from the folder Biostatics 2021
ecklonia <- read_csv("ecklonia.csv")

# selecting the columns you want to exclude (this is done by adding the minus sign before each name)
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID) # All set for Pearson

# Pearson correlation (used when variables are continuous)
# correlation to check the dependance of stipe length to frond length
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")
# correlation close to 1, so it is strong (since it is more than 0.5)

# comparing all the variables in ecklonia sub. See why we deleted the columns with words?
ecklonia_pearson <- cor(ecklonia_sub)

# correlation for ordinal data is done using spearman!

# Kendall rank correlation (both continous and ordinal data)
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))

# check correlation
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# calculting Pearson r
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))
# the r print part has no meaning, just the round part is important.
# this above says that pearson r must be rounded off to 2.

# creating a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) + # creates the line on the graph
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) + # specifying where the r should be placed in the graph.
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# multiple panel visual (method can be square, ellipse, number, shade, pie)
corrplot(ecklonia_pearson, method = "square")
corrplot(ecklonia_pearson, method = "circle")
corrplot(ecklonia_pearson, method = "pie")
corrplot(ecklonia_pearson, method = "shade")
corrplot(ecklonia_pearson, method = "ellipse")
corrplot(ecklonia_pearson, method = "number")


# Exercise: Produce a heatmap using ggplot

# load libraries
library(ggplot2)
library(dplyr)
library(ggplot2)
library(reshape)
library(ggpubr)
library(corrplot)
library(reshape2)

# load data
ecklonia <- read_csv("ecklonia.csv")

# remove columns with select
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

#calculate Pearson r
data <- round(cor(ecklonia_sub), 2)

# melt the data
melted_data <- melt(data)


# heatmap
ggplot(data = melted_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90))


        