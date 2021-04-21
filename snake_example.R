# Phiwokuhle Somdaka
# Snake example
# 20 April 2021

# read data and convert day column to a factor (because ANOVA works with variables)
# that are factor independent and 'day' is a continuous variable)

library(tidyverse)
library(readr)

snakes <- read_csv("snakes.csv")
snakes$day = as.factor(snakes$day)

# create summaries of the data (sd and mean)
snakes.summary <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% #Results off???
  ungroup()

# ignoring grouping by both
snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()

# load new library
library(Rmisc)
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

# visualise data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# fitting ANOVA model
snakes.aov <- aov(openings ~ day + snake, data = snakes)

# testing assumptions
snakes.res <- residuals(snakes.aov)

# histogram of snake residue
hist(snakes.res, col = "red")

# other plots
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")
