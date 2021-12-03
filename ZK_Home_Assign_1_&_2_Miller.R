#home assignment part 1
home_sample_1 = read.csv("https://tinyurl.com/yxm5rd89")
library(tidyverse)
library(ggplot2)
library(psych)
library(lmtest)
library(car)
library(lm.beta)
#model diagnostics

home_sample_1 %>% summary()
ggplot(home_sample_1,aes(x = pain,)) + geom_histogram()
ggplot(home_sample_1, aes(x = age)) + geom_histogram()
ggplot(home_sample_1, aes(x = IQ)) + geom_histogram()
ggplot(home_sample_1, aes(x = household_income)) + geom_histogram()
ggplot(home_sample_1, aes(x = STAI_trait)) + geom_histogram()

#taking a closer look at errors

home_sample_1 %>% plot(which = 4)
home_sample_1 %>% ggplot() + aes(x = pain, y = age) + geom_point() + geom_smooth(method = "lm")
home_sample_1 %>% ggplot() + aes(x = STAI_trait, y = age) + geom_point() + geom_smooth(method = "lm")
home_sample_1 %>% slice(c(88, 34))

#dealing with errors

hs1_revised <- home_sample_1 %>% filter(!STAI_trait == 4.2, !pain == 55)
view(hs1_revised)

#model 1

mod1 = lm(pain ~ age + sex, data = hs1_revised)

#model 2

mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = hs1_revised)

#drafted model comparison in report

summary(mod1)
lm.beta(mod1)
summary.lm.beta(mod1)
confint(mod1)
AIC(mod1)
anova(mod1)
#checking LR assumptions: normality, linearity, homoscedasticity, multicollinearity

describe(residuals(mod2))
describe(hs1_revised)

#checking normality with QQ plot

mod2 %>% plot(which = 2)

#checking linearity 

mod2 %>% residualPlots()

#checking homoscedasticity

mod2 %>% plot(which = 3)
bptest(mod2)
ncvTest(mod2)

#checking multicollinearity

mod2 %>% vif()

mod2b = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = hs1_revised)

AIC(mod2a)
AIC(mod2b)
Anova(mod2a,mod2b)

#mod2a (coritsol_saliva) has lower AIC; re-running assumptions 

mod2a %>% plot(which = 2)

mod2a %>% residualPlots()

mod2a %>% plot(which = 3)
bptest(mod2a)
ncvTest(mod2a)

mod2a %>% vif()

#summary

summary(hs1_revised)
summary(mod2a)

#part 2 

hs1_revised = read.csv("https://tinyurl.com/yxm5rd89")

#non-theory-based model

back_mod = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = hs1_revised)

#model diagnostics to address new variables

hs1_revised %>% summary()
ggplot(home_sample_1, aes(x = weight)) + geom_histogram()
ggplot(home_sample_1, aes(x = IQ)) + geom_histogram()
ggplot(home_sample_1, aes(x = IQ, y = pain)) + geom_point()
ggplot(home_sample_1, aes(x = household_income)) + geom_histogram()
ggplot(home_sample_1, aes(x = household_income, y = pain)) + geom_point()
plot(cooks.distance(lm(pain ~ household_income + IQ, data=hs1_revised)))

#normality

back_mod %>% plot(which = 2)

#checking linearity 

back_mod %>% residualPlots()

#checking homoscedasticity

back_mod %>% plot(which = 3)
bptest(back_mod)
ncvTest(back_mod)

#checking multicollinearity

back_mod %>% vif()

#attempting backward regression 

back_mod = step(back_mod, direction = "backward")

#running new regression on retained predictors

backward_model = back_mod = lm(pain ~ pain_cat + mindfulness + cortisol_serum, data = hs1_revised)
backward_model

#running full regression model from Pt. 1

theory_based_model = mod2a = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = hs1_revised)
theory_based_model

#comparing models AIC & Anova

AIC(backward_model)
AIC(theory_based_model)
Anova(backward_model)
Anova(theory_based_model)

#testing the models on new data

data_file_2 = read.csv("https://tinyurl.com/87v6emky")

str(data_file_2)
data_file_2 %>% summary()
ggplot(home_sample_1,aes(x = IQ,)) + geom_histogram()
ggplot(home_sample_1, aes(x = household_income )) + geom_histogram()

predict(backward_model)
back_mod_test<-predict(backward_model, data_file_2)
back_mod_test

predict(theory_based_model)
theory_mod_test<-predict(theory_based_model, data_file_2)
theory_mod_test
  
RSS_backward=sum((data_file_2[,"pain"]-back_mod_test)^2) 
RSS_backward
RSS_thoery=sum((data_file_2[,"pain"]-theory_mod_test)^2)
RSS_thoery    

