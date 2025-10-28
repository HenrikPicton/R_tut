library(exscidata)
library(tidyverse)

dat <- exscidata::boston %>%
  filter(year == 2012) %>%
  mutate(speed = 42 / ((seconds / 60) / 60))

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
  geom_abline(intercept = coef(summary(m2))[1, 1],
              slope = coef(summary(m2))[3, 1],
              color = "red") +

  geom_abline(intercept = coef(summary(m2))[1, 1] +
                coef(summary(m2))[2, 1],
              slope = coef(summary(m2))[3, 1],
              color = "blue") +
  scale_color_manual(values = c("red", "blue"))

dat <- dat %>%
  mutate(age2 = age^2) %>%
  print()

m3 <- lm(speed ~ gender + age + age2 + gender:age + gender:age2,
         data = dat)

m4 <- lm(speed ~ gender * (age + age2), data = dat)

summary(m3)

coef(m3)

predictions <- data.frame(age = rep(seq(from = 18, to = 82, by = 1), 2),
                          gender = rep(c("M", "F"), each = 130 / 2)) %>%
  mutate(age2 = age^2) %>%
  mutate(
    prediction = if_else(
      gender == "M",
      coef(m3)[1] + coef(m3)[2] + age * coef(m3)[3] + age2 * coef(m3)[4],
      coef(m3)[1] + age * coef(m3)[3] + age2 * coef(m3)[4]
    )
  )
dat %>%
  ggplot(aes(age, speed, color = gender)) +
  geom_point() +
  geom_line(data = predictions, aes(age, prediction, color = gender)) +
  scale_color_manual(values = c("red", "blue"))

round(coef(m3), 3)

df <- data.frame(age = c(20, 30, 70, 20, 30, 70),
                 age2 = c(400, 900, 4900, 400, 900, 4900),
                 gender = c("M", "M", "M", "F", "F", "F"))

df$pred <- predict(m4, newdata = df)

df %>%
  pivot_wider(names_from = gender,
              values_from = pred) %>%
  mutate(diff = M - FALSE)

dat %>%
  ggplot(aes(age, speed, color = gender)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = FALSE)
coef(m4)

dat2 <- dat %>%
  mutate(age = age - mean(age),
         age2 = age^2)

m5 <- lm(speed ~ gender * (age + age2),
         data = dat2)

coef(m4)
coef(m5)
