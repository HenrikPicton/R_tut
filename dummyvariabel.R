library(exscidata)
library(tidyverse)

dat <- exscidata::boston %>% 
  filter(year==2012) %>% 
  mutate(speed = 42 / ((seconds/60) / 60)) %>% 
  print()

m <- lm(speed ~ gender, data = dat)

summary(m)


dat %>% 
  ggplot(aes(age, speed)) + 
  geom_point() +
  geom_smooth(method = "lm")

dat %>% 
  ggplot(aes(age, fill = gender)) +
  geom_histogram(position = position_dodge())

dat_age <- dat %>% 
  filter(age == 30) %>% 
  print()

m1 <- lm(speed ~ gender, data = dat_age)

summary(m1)


m2 <- lm(speed ~ gender + age, data = dat)
coef(summary(m2))


dat %>% 
  ggplot(aes(age, speed, color = gender)) +
  geom_point() +
  geom_abline(intercept = coef(summary(m2))[1,1],
              slope = coef(summary(m2))[3,1],
              color = "red") +
  
  geom_abline(intercept = coef(summary(m2))[1,1] +
                coef(summary(m2))[2,1],
              slope = coef(summary(m2))[3,1],
              color = "blue") +
                scale_color_manual(values = c("red","blue"))

dat2 <- dat %>% 
  mutate(age2 = age^2) %>% 
  print()

m3 <- lm(speed ~ gender + age + age2,
         data = dat2)

summary(m3)

coef(m3)

predictions <- data.frame(age = rep(seq(from = 18, to = 82, by = 1), 2),
gender = rep(c("M", "F"), each = 130/2)) %>% 
  mutate(age2 = age^2) %>% 
  head()
