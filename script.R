# If not installed, install these packages.
install.packages("readxl")
install.packages("ggplot2")


# load packages
library(readxl)
library(ggplot2)


# read data
depdata <- read_xlsx("depression.xlsx")


# view data
head(depdata)


# Summary statistics
data.frame(n = length(depdata$score),
           mean = mean(depdata$score),
           sd = sd(depdata$score))


# check outliers
check_outliers(depdata$score)


# Shapiro-Wilk normality test
shapiro.test(depdata$score)


ggplot(depdata, aes(sample = score)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic()


ggplot(depdata) +
  geom_histogram(aes(x = score)) +
  theme_classic()


ggplot(depdata) +
  geom_boxplot(aes(y = score), outlier.color = "red") +
  theme_classic()


t.test(depdata$score, mu = 4)


cohen_d(depdata$score, mu = 4)



