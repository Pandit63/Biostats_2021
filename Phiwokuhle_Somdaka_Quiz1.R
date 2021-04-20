# 3831469
# Quiz 1
# 20 April 2021

# Question 1
# Numerical data: This is quantitative data that represents things than can be
# counted, calculated and measure. There different kinds of classes within the
# class. 
# 1.Discrete data: this is integer data. Meaning that it can only have whole
# numbers, no decimals should appear in the data. For exapmple, age can only be
#1, 2, 3.. not 1,5 years.
# 2.Continuous data: represents data from measured things. It is not limited to
# integers, it can contain decimals, for example, a height can be 1,5 metres.
# 3. Dates: a special kind of continuous data.

# Qualitative data: different from numeric data, this comprises of categories that
# are described, including rankings and classes. This class also has subset classes.
# 1. Categorical data: has different categories where the number of individuals or
# measrement etc are written under the different categories. For example, there 
# can be categoriesfor males and females with 25 and 35 individual, respectively.
# 2. Ordinal data: In this kind of data, classes are ranked from the lowest to the
# highest, and the magnitude between the given classes cannot be measured. You 
# typically get this data on an ordinal scale. For example, the ranking for good
# can be 4 and bad 2.
# Binary data: This is an either/or kind of data for example, a dataset can have
# data for students that either "passed" or "failed"

# functions to view data.
# 1. view()
# 2. summary()
# 3. head()
# 4. tail()
# 5. colnames()

# Skewness: this measres the symmetry of a dataset. It checks what the location 
# of the median relative to the mean of the data. If skewness is negative, the
# data is left-skewed and the mean is less than the median. If it is positive,
# then the mean is greater than the median and the data is right skewed.

# Kurtosis: this shows the tail-shape of the distribution of the data.Data that
# is normally distributed has zero kurtosis and the tail-shape is mesokurtic.
# Data that has a negative kurtosis has a thin tail and is called platykurtic.
# Data that has a positive kurtosis has a fat tail and is called leptokurtic.

# Question 2
Orange <- datasets::Orange

library(tidyverse)
library(e1071)

# The Orange data is categorical data because it comprises of different categories
# (age and circumference) and the measured and individual values.\

# first 6 rows and last 6 rows
Orange_1 <- Orange %>% 
  slice(1:6, 30:35)

#column names
colnames(Orange)

# summary statistic
summary(Orange)

# Mean, Median and Standard deviation
Age <- Orange %>% 
  group_by(Tree) %>% 
  summarise(mean_age = mean(age),
            med_age = median(age),
            sd_age = sd(age))

Circum <- Orange %>% 
  group_by(Tree) %>% 
  summarise(mean_circ = mean(circumference),
            med_circ = median(circumference),
            sd_circ = sd(circumference))

# kurtosis
kurtosis(Orange$age)
kurtosis(Orange$circumference)

# skewness
skewness(Orange$age)
skewness(Orange$circumference)

# graphs
# graph 1:
Circum_2 <- Orange %>% 
  group_by(Tree) %>% 
  summarise(mean_circ = mean(circumference))

ggplot(data = Circum_2, aes(x = Tree, y = mean_circ)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  labs(x = "Tree", y = "Mean circumference") +
  ggtitle("Comparison of the mean circumference of four trees")

# graph 2
Data <- Orange %>% 
  slice(1:7)

Data2 <- Data %>% 
  select(col = age, circumference)





# Question 3
# mutate(): to create new variables or columns using the data in an existing column.
# select(): to remove other columns and be left with the columns you need to work with.
# group_by(): used when you want to calculate mean,sd, max, min etc for individual
# samples in a dataset.
# filter(): used to remove rows and remain with the rows you require.
# separate(): to separate data (untidy) in a column that has different kinds of
#observations.

