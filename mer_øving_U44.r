library(exscidata)
library(tidyverse)

d <- exscidata::cyclingstudy %>%
  filter(timepoint == "pre") %>%
  select(subject, group, VO2.max, height.T1) %>%
  ggplot(aes(height.T1, VO2.max)) +
  geom_point(size = 3,
             fill = "lightblue",
             shape = 21) +
  labs(x = "Height (cm)",
       y = expression("VO"["2max"] ~ (ml^-1 ~ min^-1))) +
  theme_classic()
d
