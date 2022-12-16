d <- read.csv("C:/Users/Admin/Desktop/Advanced Statistics/LLGA.csv")
head(d)

#Exploratory Analysis
table(d$Gender)
#Omit 1 missing values and 42 unknowns to include gender into our analysis
cleanD<- d[d$Gender %in% c('F', 'M'), ]
head(cleanD)

#Bivariate analysis:
library(ggplot2)
ggplot(data= cleanD, aes(x= Age))+ 
  geom_density(aes(group= Gender, col= Gender, fill= Gender), alpha= 0.5)

ggplot(data= cleanD, aes(x= Age, y= Freq))+
  geom_point(aes(group= Gender, col= Gender))
#Not much has been shown except a lot of variation

#The Poisson model does not make assumptions about normality or homoscedasticity of the residuals
#Fitting the Poisson GLM Yi~Pois(ui)
model <- glm(Freq~ Age*Gender, data= cleanD, family= 'poisson'(link= 'log'))
summary(model)
#As can be seen from the result, both gender and age are statistically significant
#For women, each additional year of age  is associated with an average (exp(0.026-1)*100% which aproximatelly equals to 2.6% increase in the visit frequency)
#For men, the correlation between those 2 variables is 0.1% smaller

#Next, we test whether the relationship differs by gender by comparing the below model (not have the gender variable) and the aforementioned one
model_0_gender <- glm(Freq~ Age, data= cleanD, family= 'poisson'(link= 'log'))
anova(model, model_0_gender, test= 'Chisq')

#Conclude: Gender has a statistically significant effect of age on the frequency of attendance of sports facilities.

#Plot the fit
Predict <- expand.grid(Age= 15:95, Gender= c('F', 'M'))
Predict$Freq <- predict(model, newdata= Predict, type= 'response')
ggplot(data= Predict, aes(x= Age, y= Freq))+
  geom_line(aes(group= Gender, col= Gender), size= 2)+
  ylab('Estimated Mean Frequency of Attendance')

#Illustrate the uncertainty of the prediction by adding 95% envelops
#Obtaining std error
Predict$SE <- predict(model, newdata= Predict, type='response',se.fit=T)$se.fit

#Using CLT to evaluate upper and lower bounds
Predict$High <- Predict$Freq+ 1.96*Predict$SE
Predict$Low <- Predict$Freq- 1.96*Predict$SE

ggplot(data= Predict, aes(x= Age, y= Freq))+
  geom_ribbon(aes(ymin= Low, ymax= High, group= Gender, fill= Gender), alpha=0.3)+
  geom_line(aes(group= Gender, col= Gender), size= 1)+
  ylab('Estimated Mean Frequency of Attendance')

#The envelops are narrowand not overlap, which meansthe relationships between Age and mean frequency of attendance are very different for men and for women.
#However, in our example, we expect all the participants to have the same average attendance, once we adjust for gender and age
#The number of recorded visits should be proportional to the duration of participation
#Yi~ Pois(udi, Ndi) where udi s the daily frequency of visits and Ndi is the total number of days available for visits
#log(ui)= log(udi)+ log(Ndi)

cleanD$Duration <- as.numeric(as.Date('1/04/2016', '%d/%m/%Y')-
                            as.Date(cleanD$StartDate, '%d/%m/%Y'))
cleanD$Duration <- pmin(cleanD$Duration, 365)

cleanD<- cleanD[cleanD$Duration>0,]

model_adj<- glm(Freq ~ Age*Gender + offset(log(Duration)), 
          data= cleanD, family='poisson'(link='log'))

summary(model_adj)

Pred <- expand.grid(Age= 15:95, Gender= c('F', 'M'), Duration=365)
Pred$Freq <- predict(model_adj, newdata= Pred, type= 'response')

Pred$SE <- predict(model_adj, newdata= Pred, type='response',se.fit=T)$se.fit

Pred$High <- Pred$Freq+ 1.96*Pred$SE
Pred$Low <- Pred$Freq- 1.96*Pred$SE

ggplot(data= Pred, aes(x= Age, y= Freq))+
  geom_ribbon(aes(ymin= Low, ymax= High, group= Gender, fill= Gender), alpha=0.3)+
  geom_line(aes(group= Gender, col= Gender), size= 1)+
  ylab('Estimated Mean Annual Frequency of Attendance')
AIC(model)
AIC(model_adj)
d.pred <- expand.grid(Age=15:95, Gender=c('F','M'), Duration=365)

d.pred$Freq <- predict(model_adj, newdata=d.pred, type='response')


#However
table(cleanD$Freq==1)
#32.8% of them never came back. So may-be we should not model the Freq variable as a response. Let's model return visits instead. And let's offset it by the duration of time remaining in the study
cleanD$Return <- cleanD$Freq - 1

cleanD$Duration <- as.numeric(as.Date('1/04/2016', '%d/%m/%Y')-
                            as.Date(cleanD$StartDate, '%d/%m/%Y'))
cleanD$Duration <- pmin(cleanD$Duration, 365)

cleanD<- cleanD[cleanD$Duration > 0,]
cleanD
table(cleanD$Duration==0)
model_adj2<- glm(Return ~ Age*Gender+offset(log(Duration)), data=cleanD,
          family='poisson'(link='log') )
summary(model_adj2)

Predict <- expand.grid(Age= 15:95, Gender= c('F', 'M'), Duration=365)
Predict$Freq <- predict(model_adj2, newdata= Predict, type= 'response')

Predict$SE <- predict(model_adj2, newdata= Predict, type='response',se.fit=T)$se.fit
Predict$High <- Predict$Freq+ 1.96*Predict$SE
Predict$Low <- Predict$Freq- 1.96*Predict$SE
ggplot(data= Predict, aes(x= Age, y= Freq))+
  geom_ribbon(aes(ymin= Low, ymax= High, group= Gender, fill= Gender), alpha=0.3)+
  geom_line(aes(group= Gender, col= Gender), size= 1)+
  ylab('Estimated Mean Annual Frequency of Return Visits')



