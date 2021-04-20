# P Somdaka
# Day 2
# t-tests

library(tidyverse)
library(plotly)
library(ggpubr)

# testing normality of data 
#random normal data , randomly created dataset a and b
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# history, its good to plot before checking normality cause graph gives you shape of the datasets
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h

# normality test #Shapiro world test
shapiro.test(r_dat$dat)

# different samples normality test, we have to check for individual samples not the entire set
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

# testing homoscedasticity
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

# Two for one (Shapiro and Normality)
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result) #function activated, now use it as below
}
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])

# Two sample t-test
# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# using the comparing of means test
compare_means(dat ~ sample, data = r_two, method = "t.test", var.equal = TRUE)


# new data
Ecklonia <- read_csv("ecklonia_1.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# visualising the data
ggplot(data = Ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# filter the data
ecklonia_sub <- Ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# choosing a test

#Check normality and homos..
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1], # checking the variances of the individual samples
            stipe_mass_norm = two_assum(value)[2]) # 

# t test
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")


# ANOVA
# 1st remember t tests
chicks <- datasets::ChickWeight

# subset
#Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# perform t test
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test") # no significance because p is great5er than 0.05

#ANOVA real stuff!
#diffs btwn 4 diets on last day (single factor)
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

# Multiple factor: do on your own


