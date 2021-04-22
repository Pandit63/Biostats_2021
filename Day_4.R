# Day 4
# Confidence intervals and others
# 22 April 2021
# Phiwokuhle Somdaka

# confidence interval -range onf means if experiment was to be repeated 100x maybe
# examples
# create data
Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

# read the data
data <- read.table(textConnection(Input),header = TRUE)
summary(data)

# data summary
summary <- summary(data)

#load library
library(rcompanion)

# groupwise means function: step~ 1 (overall mean), conf (CI), 
X <- groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# oneway data (for the different genders)
Sex <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# use bootstrapping
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000, # how many times out of 10000 can the result be the same
              boot = TRUE, # switch on this
              traditional = FALSE, # switching off the traditional way
              normal = FALSE, # switch off normal way of calculating CI
              basic = FALSE, # same as above
              percentile = FALSE, # same as above
              bca = TRUE)

# plotting CI
# load library
library(ggplot2)
library(gplots)

ggplot(data = Sex, aes(x = Sex, y = Mean, colour = Sex)) +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper), width = 1) +
  geom_line() +
  geom_point()

ggplot(data = Sex) +
  geom_col(aes(x = Sex, y = Mean), fill = "red", col = "black") +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper, Sex),
                col = "black", 
                width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")

# Another example (Incl Teacher and Gender)

data2 <- read.table(textConnection(Input),header = TRUE)

S_T<- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)


ggplot(data = S_T) +
  geom_col(aes(x = Sex, y = Mean), fill = "blue", col = "black") +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper, Sex),
                col = "black", 
                width = 0.2) +
  facet_wrap(~Teacher, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")

# Analysis of variance (among sex and teacher)
AV <- aov(Steps ~ Sex*Teacher, data = data)


# CI of compared means
anova_Tukey <- TukeyHSD(AV)

# plot
plot(anova_Tukey)

