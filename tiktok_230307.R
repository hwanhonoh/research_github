# tiktok path modeling
library(haven)
library(tidyverse)
library(lavaan)
library(semTools)
library(psych)

data = read_spss("tiktok_230211_3.sav")

model1 = '
a_ep ~ b_ep + b_cni_2
a_cni_2 ~ b_cni_2 + b_ep

a_ep ~~ a_cni_2

a_pp ~ a_ep + a_cni_2

a_s ~ a_pp
a_c ~ a_pp
'

fit1 = cfa(model1, data = data, missing = "fiml")
summary(fit1, fit.measures = T, standardized = T, rsquare = T)

fit1_mod <- modindices(fit1)
fit1_mod %>%
  arrange(desc(mi)) %>%
  filter(mi >= 3)
