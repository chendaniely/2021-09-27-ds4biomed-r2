library(tidyverse)

cancer <- read_csv("data/survival/veterans_lung_cancer.csv")

library(survival)

Surv(cancer$survival_in_days, cancer$status)

sf1 <- survfit(Surv(cancer$survival_in_days, cancer$status) ~ 1, data = cancer)

names(sf1)

plot(sf1)

library(survminer)

ggsurvplot(sf1)

summary(sf1, times=365.25)

ggsurvplot(sf1, risk.table = TRUE)


sf2 <- survfit(Surv(cancer$survival_in_days, cancer$status) ~ treatment, data = cancer)

ggsurvplot(sf2, risk.table = TRUE)


sf3 <- survfit(Surv(cancer$survival_in_days, cancer$status) ~ treatment + prior_therapy, data = cancer)

ggsurvplot(sf3, risk.table = TRUE)

cox <- coxph(Surv(survival_in_days, status) ~ treatment + prior_therapy, data = cancer)
cox

library(broom)

cox %>%
  tidy() %>%
  mutate(hr = exp(estimate))
