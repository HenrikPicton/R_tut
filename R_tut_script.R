library(tidyverse)
library(exscidata)

Oppgave10 <- strengthvolume %>%
  select(exercise,time, sets, load, sex) %>% 
  filter(exercise %in% c("legext", "legpress")) %>% 
  group_by(exercise, time, sets, sex) %>% 
  summarise(Avgload = mean(load, na.rm = TRUE),
            Sdload = sd(load, na.rm = TRUE))




  
