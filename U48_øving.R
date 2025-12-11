library(tidyverse)
library(ggtext)
library(cowplot)
library(ggdag)

norm.samp <- rnorm(10, 0, 1)

rbinom(10, 10, 0.5)

data.frame(x = seq(from = 1, to = 10, by = 1)) |> 
  mutate(y = rbinorm)

rbinom(10, 1, prob = 0.2)


rpois(10, 20)

data.frame(x = seq(from = 1, to = 40, by = 1)) |> 
  mutate(y = dpois(x, 20)) |> 
  ggplot(aes(x,y)) + geom_bar(stat = "identity")

set.seed(1)

pill <- rbinom(100, 1, 0.5)

fluid <- rnorm(100, pill * (-0.5), 1)

bp <- rnorm(100, fluid, 0.2)

m <- lm(bp ~ pill + fluid)
summary(m)

ggplot(data = data.frame(pill, fluid, bp),
aes(as.factor(pill), bp)) +
  geom_boxplot()

#Confounder

set.seed(1)

sleep <- rbinom(100, 1, 0.5)

training <- rnorm(100, sleep * (1), 1)

strength <- rnorm(100, sleep * (0.8), 1)

m2 <- lm(strength ~ training + sleep)

summary(m2)

#Collider

