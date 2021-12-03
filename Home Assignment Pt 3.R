#Part 3: Increase Generalizability

data_file_3 = read.csv("https://tinyurl.com/b385chpu")
data_file_4 = read.csv("https://tinyurl.com/4f8thztv")

library(tidyverse)
library(ggplot2)
library(psych)
library(lme4)
library(lmerTest)
library(MuMIn)
library(tidyRSS)
library(plyr)
library(cAIC4)

#visualizing the data

data_file_3 %>% summary()
data_file_4 %>% summary()

ggplot(data_file_3, aes(x = household_income)) + geom_histogram()

data_file_3 %>% plot(which = 4)

data_file_3 %>% slice(c(2))

describe(data_file_3)

#dealing with error (neg income value)

df3_revised <- data_file_3 %>% filter(!household_income == -7884)
view(df3_revised)

#attempting to build a linear mixed model

random_int_mod = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1 | hospital), data = data_file_3)
random_int_mod

#coefficient and confidence intervals
summary(random_int_mod)
confint(random_int_mod)
random_int_mod2

#coefficient and confidence intervals
summary(random_int_mod2)
confint(random_int_mod2)

#computing R2 and residual variance
r.squaredGLMM(random_int_mod)
sum(residuals(random_int_mod)^2)
r.squaredGLMM(random_int_mod2)

#predicting pain in data_file_4

describe(data_file_4)
predictpain <- predict(random_int_mod, data_file_4, allow.new.levels = TRUE)

#computing variance
RSS = sum((data_file_4$pain - predictpain)^2)
RSS
((data_file_4$pain - predictpain)^2)
mod_mean <- lm(pain ~ 1, data = data_file_4)
TSS=sum((data_file_4$pain -predict(mod_mean))^2)

#compare marginal and conditional R2 values
1-(RSS/TSS)
r.squaredGLMM()

#linear mixed effects model: predicting pain on dataset 3
random_int_mod = lmer(pain ~ coritsol_serum + (cortisol_serum|hospital),data = data_file_3)

predictpain_3 = data_file_3 %>% mutate(pred_int = predict(random_int_mod), pred_slope = predict(random_int_mod2))

#reorder
neworder <- c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")

#XXX failed XXX attempts#

predictpain_3b %>% ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) + geom_point(aes(color=hospital), size = 1)+ geom_line((color='red', aes(y=pred_slope, x=cortisol_serum)+ facet_wrap()

#CAIC
cAIC(random_int_mod)$caic
cAIC(random_int_mod2)$caic

#ANOVA and R^2

anova(random_int_mod,predictpain_3)
r2beta(random_int_mod, method = "nsj", data = data_file_3)
r2beta(pred_slope)

#attempt to fix 

data_file_3 %>% ggplot() + aes(y = pain, x = cortisol_serum = hospital)
+ geom_point(aes(color = hospital), size = 4) + geom_line(color = "red", aes(y=predictpain_3, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
