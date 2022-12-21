#Mixed Effects Generalised Linear Models
library(lme4)
library(lmerTest)

model <- glmer(Y ~ X + (1|PID), data=d, family='binomial'(link='logit'))
summary(model)

qqnorm(c(unlist(ranef(model)$PID))); qqline(c(unlist(ranef(model)$PID)))