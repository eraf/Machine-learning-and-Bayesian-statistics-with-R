data1 <- cars
head(data1)

library(dplyr)
install.packages("ggplot2")
library(ggplot2)

data1 %>% 
  ggplot(aes(x = speed, y = dist)) + 
  geom_point(colour = "red") + 
  geom_smooth(se = F, colour = "grey") +
  theme_classic() +
  labs(x = "Speed", y = "Distance", 
       title = "Speed vs Distance")

install.packages("tidyr")
library(tidyr)

data1 %>% 
  gather() %>% 
  ggplot(aes(key, value)) +
  geom_boxplot(
    outlier.color = "red",
    size = .5,
    width = .25
  ) +
  facet_wrap(~key, scales = "free") +
  theme_classic()

data1 %>% 
  gather() %>% 
  ggplot(aes(value, fill = key)) +
  geom_density(alpha = .5) +
  facet_wrap(~key, scale = "free") +
  theme_classic()

print(cor(data1$speed, data1$dist))
lm_model = lm(dist ~ speed, data = data1)
print(lm_model)
model_summary = summary(lm_model)
print(model_summary)
print(names(model_summary))
print(model_summary$coefficients)
model_coeff <- model_summary$coefficients
print(class(model_coeff))

t_value <- model_coeff['speed', 'Estimate'] / 
  model_coeff['speed', 'Std. Error']
t_value
f_value = model_summary$fstatistic[1]
f_value
summary(model_summary$fstatistic)
model_summary$adj.r.squared
AIC(lm_model)
BIC(lm_model)

names(data1)
data1$id = 1:nrow(data1)
train <-  data1 %>% 
  sample_frac(size = .8, 
              replace = F)
test <- data1 %>% 
  anti_join(train, by = 'id')

help("sample_frac")
help("anti_join")

train_model = lm(dist ~ speed, data = train)
summary(train_model)
help("predict")
preds <- predict(train_model, test)
