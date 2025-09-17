library(tidyverse)
library(exscidata)

set.seed(100)

d <- data.frame(Xdata = rnorm(10, mean = 0, sd = 1),
                 Ydata = rnorm(10, mean =10, sd = 2))

ggplot(data = d, aes(x = Xdata, y = Ydata)) + geom_point()

d$z <- c(rep("A", 5), rep("B", 5))

head(d)

ggplot(data = d, aes(x = Xdata, y = Ydata, color = z)) + geom_point()

ggplot(data = d, aes(x = Xdata, y = Ydata, fill = z)) + geom_point(shape = 23)

ggplot(data = d, aes(x = Xdata, y = Ydata, fill = z, shape = z)) +
  geom_point(size = 3) +
  scale_fill_manual(values = c("red", "green")) +
  scale_shape_manual(values = c(21, 23))
