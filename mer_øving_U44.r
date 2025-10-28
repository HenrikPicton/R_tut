library(exscidata)
library(tidyverse)

cyc_select <-  cyclingstudy %>%
        filter(timepoint == "pre") %>%
        select(subject, group, VO2.max, height.T1)

m1 <- lm(VO2.max ~ height.T1, data = cyc_select)

linearplot <- exscidata::cyclingstudy %>%
  filter(timepoint == "pre") %>%
  select(subject, group, VO2.max, height.T1) %>%
  ggplot(aes(height.T1, VO2.max)) +
  geom_point(size = 3,
             fill = "blue",
             shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Height (cm)",
       y = expression("VO"["2max"] ~ (ml^-1 ~ min^-1))) +
  theme_classic()

linearplot

varianceplot <- cyclingstudy |> 
  filter(timepoint == "pre") |> 
  select(subject, group, VO2.max, height.T1) |> 
  mutate(yhat = fitted(m1),
  resid = resid(m1)) |> 
  ggplot(aes(height.T1, resid)) + geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0, lty = 2, color = "red") + 
  labs(y = "Residuals", x = "Height (cm)")

varianceplot

residualplot <- cyclingstudy |> 
  filter(timepoint == "pre") |> 
  select(subject, group, VO2.max, height.T1) |> 
  mutate(yhat = fitted(m1)) |> 

  ggplot(aes(height.T1, VO2.max, group = subject)) +
  
  geom_segment(aes(y = yhat, yend = VO2.max, x = height.T1, xend = height.T1),
color = "blue") +

  geom_point(size = 3,
             fill = "blue",
             shape = 21) +
  
  geom_point(aes(height.T1, yhat),
             size = 3, fill = "purple", shape = 21) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
         labs(x = "Height (cm)",
              y = expression("VO"["2max"] ~ (ml^-1 ~ min^-1))) +
         theme_classic()
residualplot
