d <- read.csv("C:/Users/Admin/Desktop/Advanced Statistics/BPData.csv")
d
#Disregarding the fact that samples from the dataset is not independent(before and after score of each patients), we try unpaired t-test:
t.test(BP ~ Group, data=d, var.equal=T)
#Which  is equivalent to fitting a simple linear regression
model <- lm(BP ~ Group, data= d)
summary(model)
#We got the result that the treatment works
#However, the samples are not independent, we add the participants ID
library(lme4)
library(lmerTest)

model <- lmer(BP ~ Group + (1|PID), data=d)
summary(model)
