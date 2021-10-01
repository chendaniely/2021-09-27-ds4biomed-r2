library(medicaldata)
library(tidyverse)

blood <- blood_storage

#blood$Recurrence <- as.factor(blood$Recurrence)

blood <- blood %>%
  mutate(Recurrence = as.factor(Recurrence),
         RBC.Age.Group = as.factor(RBC.Age.Group)
         )

mod <- glm(Recurrence ~ RBC.Age.Group, data = blood, family="binomial")
summary(mod)

library(broom)

mod %>%
  tidy() %>%
  mutate(or = exp(estimate))


mod <- glm(Recurrence ~ RBC.Age.Group + Age + PVol + as.factor(TVol), data = blood, family="binomial")
summary(mod)

mod %>%
  tidy() %>%
  mutate(or = exp(estimate))

library(tidymodels)

model <- logistic_reg()
model

translate(model)

model %>%
  fit(Recurrence ~ RBC.Age.Group + Age + PVol + as.factor(TVol), data = blood) %>%
  tidy() %>%
  mutate(or = exp(estimate))

model_fit <- model %>%
  fit(Recurrence ~ RBC.Age.Group + Age + PVol + as.factor(TVol), data = blood)

blood_sample <- blood %>%
  sample_n(10)
blood_sample

predict(model_fit, blood_sample)

blood$Recurrence %>% table()

set.seed(42)
blood_split <- initial_split(blood, prop = 0.8, strata = Recurrence)
blood_train <- training(blood_split)
blood_test <- testing(blood_split)

model <- logistic_reg()
model_fit <- model %>%
  fit(Recurrence ~ RBC.Age.Group + Age + PVol + as.factor(TVol),
      data = blood_train)

model_fit %>%
  tidy() %>%
  mutate(or = exp(estimate))

predict(model_fit, blood_test) %>%
  table()

model_predict <- predict(model_fit, blood_test)

model_predict_class <- predict(model_fit, blood_test)
model_predict_prob <- predict(model_fit, blood_test, type = "prob")

blood_results <- bind_cols(model_predict_class, model_predict_prob, select(blood_test, Recurrence))

blood_results %>%
  count(.pred_class, Recurrence)

conf_mat(blood_results, truth = Recurrence, estimate = .pred_class)

accuracy(blood_results, truth = Recurrence, estimate = .pred_class)
sens(blood_results, truth = Recurrence, estimate = .pred_class)
specificity(blood_results, truth = Recurrence, estimate = .pred_class)
precision(blood_results, truth = Recurrence, estimate = .pred_class)
recall(blood_results, truth = Recurrence, estimate = .pred_class)

my_metrics <- metric_set(sensitivity, specificity)
my_metrics(blood_results, truth = Recurrence, estimate = .pred_class)


roc_auc(blood_results, truth = Recurrence, estimate = .pred_0)

roc_curve(blood_results, truth = Recurrence, estimate = .pred_0) %>%
  autoplot()


blood <- blood_storage
blood$Recurrence <- as.factor(blood$Recurrence)
blood_split <- initial_split(blood, prop = 0.80, strata = Recurrence)
blood_train <- training(blood_split)
blood_test  <- testing(blood_split)

simple_blood <-
  recipe(Recurrence ~ Age + PVol + PreopPSA + TVol + RBC.Age.Group,
         data = blood_train) %>%
  step_mutate(TVol = as.factor(TVol),
              RBC.Age.Group = as.factor(RBC.Age.Group),
              role = "predictor"
  ) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  themis::step_downsample(Recurrence)
simple_blood

logistic_model <- logistic_reg() %>%
  set_engine("glm")

logistic_wflow2 <-
  workflow() %>%
  add_model(logistic_model) %>%
  add_recipe(simple_blood)

logistic_fit <- fit(logistic_wflow2, blood_train)

logistic_fit %>% extract_recipe(estimated = TRUE)

logistic_fit %>% tidy() %>% mutate(or = exp(estimate))

predict(logistic_fit, blood_test)
