library(tidyverse)
library(exscidata)

Oppgave14 <- ggplot(data = strengthvolume %>% 
                      filter(time %in% c("session1", "post"), exercise == "legpress"),
                    mapping = aes(x = time, y = load)) + 
  geom_point(size = 2)+
  facet_wrap(~ sets)

Oppgave14
