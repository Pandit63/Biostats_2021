# Phiwokuhle Somdaka
# Assignment 1
# R recap
# 19 April 2021

# Section 1
datasets::BOD
view(BOD)
# C

# Section 2
# load libraries
library(dplyr)
library(dslabs)

# load data
murders1 <- murders

# exploring data
glimpse(murders1)
?murders
head(murders1)

# describing murder dataset
# The data shows the US gun murders by state in 2010. It shows the number of 
# people that were shot by gun in each state, this can be compared to the overall
# population size (as it is given in the data) and a conclusion can be formulated
# as to whether gun killings are a major problem in a state. The data specifies
# the region that each stae belongs to, this information can be used to formulate
# the overall mean for each region. Thus comparisons can be done to determine
# region has the highest rate of gun killings.


# showing only states and population sizes
slct_mur <- murders %>% 
  select("state", "population")

# removing Florida
murders1[murders1$state != "Florida",]

# removing South region states
no_south <- murders1[murders1$region != "South",]
# There are 34 states if the South is removed

# Population size of South and West regionally
# South:
murders1 %>% 
  filter(region == "South")%>% 
  summarise(sum_pop = sum(population))

# West
murders1 %>% 
  filter(region == "West") %>% 
  summarise(sum_pop = sum(population))

NorthEast <- murders1 %>% 
  filter(region == "Northeast")

# bar graph comparing the total mean no of deaths of the different regions

#library(ggplot2)

means_mur <- murders1 %>% 
group_by(region) %>% 
  summarise(mean_deaths = mean(total))

ggplot(data = means_mur, aes(x = region, y = mean_deaths)) +
  geom_bar(stat = "identity", color = "black", fill = "blue") +
  labs(x = "Region", y = "mean no of deaths") +
  ggtitle("Number of deaths by gunfire in the different regions (2010)")
# The above graph illustrates the difference in the number of gunfire deaths
# between the different regions. It is clearly evident that the South Region
# is mostly affected by death through gunfires, whereas the rest have more or less
# a similar level of being affected by gunfire deaths.

# barplot showing number of deaths by gun in the states of the Northeast region

NorthEast <- murders1 %>% 
  filter(region == "Northeast")

ggplot(data = NorthEast, aes(x = state, y = total)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  labs(x = "State", y = "No of deaths") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Number of deaths by gunfire in the States of the North East region (2010)")

# South vs West population size
# The South region has a grater population size (115 674 434) compared to the
# West region (71 945 553).

# 20< x < 100
New <- murders1 %>% 
  filter(total > 20) %>% 
  filter(total < 100)

# rows 10-24 and 26
obj <- murders1 %>% 
  slice(10:24,26)


murders_tibble <- 
  as_tibble(murders1)

region_tibble <- 
  as_tibble(murders1) %>% 
  group_by(region)

# Section 3

#load data
Heights <- heights

# average, std, min, median, max

# females
Heights %>% 
  filter(sex == "Female") %>% 
  summarise(min_hgt = min(height),
            med_hgt = median(height),
            max_hgt = max(height),
            avg_hgt = mean(height),
            sd_hgt = sd(height))

# males
Heights %>% 
  filter(sex == "Male") %>% 
  summarise(min_hgt = min(height),
            med_hgt = median(height),
            max_hgt = max(height),
            avg_hgt = mean(height),
            sd_hgt = sd(height))

# Section 4
x <-  c( 1, 6, 21, 19 , NA, 73, NA)
y <-  c(NA, NA, 3, NA, 13, 24, NA)

#no of NA
# x:
x <- sum(is.na(x))

#y
y <- sum(is.na(y))
  

# Section 5
library(lubridate)

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))
# hypothesis
# there was a steady decrease in the average Winter temperatures (F) over a
# period of 4 years.

ggplot() + 
  geom_line(data = Seasonal_data, aes(x = year, y = winter), colour = "red") + 
  labs(x = "Year", y = "Temperature (F)") +
  ggtitle("Average Winter temperatures over a period of 4 years (2015 to 2018)")


ggplot(data = Seasonal_data, aes(x = year, y = winter)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
labs(x = "Year", y = "Temperature (F)") +
  ggtitle("Average Winter temperatures over a period of 4 years (2015 to 2018)")

# The average winter temperatures decreased from 2015 to 2016 and increased in
# 2017, only to decrease again in 2018. There was no set pattern in the winter
# temperatures over the years. Thus, we reject the hypothesis.


cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))

# separating
Sep_cats <- cats_data %>%
  separate(col = "position", into = c("first_place", "second_place", "third_place"), sep = "-")

#uniting
unite_cats <- Sep_cats %>% 
  unite(minutes, seconds, col = "total_time", sep = ":")

# Section 6
Air1 <- airquality

# unite(Making a single column from two columns, uniting day and month to become
#only date in the format DD/MM)
Unite_Air <- Air1 %>% 
  unite(Day, Month, col = "Date", sep = "/")

# separate (To seperate data, seperation the day from the month, creating a column
#for each)
Sep_Air <- Unite_Air %>% 
  separate(col = Date, into =c("Day", "Month"))

# mutate (to create new columns, creating a date column using "Month" data)
Mut_Air <- Air1 %>% 
  mutate(Date = Month)

# filter(to filter observations, filtering or showing only observations in day 1)
Fil_Air <- Air1 %>% 
  filter(Day == 1)

# arrange (arranging rows in ascending order, in this case days)
Arr_Air <- Air1 %>% 
  arrange(Day)

#group_by (grouping by months and then calculating the mean of each month)
Grp_Air <- Air1 %>% 
  group_by(Month) %>% 
  summarise(mean = mean(Temp))
