library(tidyverse)
library(exscidata)

Oppgave14 <- ggplot(data = strengthvolume %>% 
                      filter(time %in% c("session1", "post"), exercise == "legpress"),
                    mapping = aes(x = time, y = load)) + 
  geom_point(size = 2)+
  facet_wrap(~ sets)

Oppgave14



Oppgave15 <- strengthvolume %>%
  filter(exercise %in% c("isom", "isok.60", "isok.120", "isok.240"),
         !is.na(load)) %>%
  mutate(velocity = case_when(
    exercise == "isom" ~ 0,
    exercise == "isok.60" ~ 60,
    exercise == "isok.120" ~ 120,
    exercise == "isok.240" ~ 240
  )) %>%
  ggplot(aes(x = velocity, y = load, color = sex)) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ time)

Oppgave15



Oppgave16 <- millward %>% 
  ggplot(aes(x = RNA, y = protein_synthesis))+
  geom_smooth(method = "lm", se = TRUE)

Oppgave16
