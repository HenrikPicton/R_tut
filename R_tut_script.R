library(tidyverse)
library(exscidata)


strengthvolume %>%
  group_by(time, sets, sex, exercise) %>%
  summarise(
    mean_load = mean(load, na.rm = TRUE),
    sd_load   = sd(load, na.rm = TRUE),
    .groups = "drop"
  )
