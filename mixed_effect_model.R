#Repeated measure ANOVA
d <- read.csv("C:/Users/Admin/Desktop/Advanced Statistics/eucalyptus.csv")
head(d)

#One of the questions we may ask is whether the tree height depends on the planting density and species
#Let's fit the random effects model to our data, and then compare it to the model which does not have stocking in it
library(lme4)
library(lmerTest)

unique(d[['stocking']])
#Treat type of plot (planting density) as a factor because there are only 4 different values
d$stocking <- factor(d$stocking)

#Plot as an random effect
model<- lmer(hgt~ spp*stocking+ (1|plot), data= d)
model_0_stocking <- lmer(hgt~ spp+ (1|plot), data= d)
anova(model, model_0_stocking)
#Conclude: stocking(planting density) does have a statistically significant effect on average tree heights ($p < 0.0001)
summary(model)$varcor
#These are the estimated standard deviations for the plot-specific random effect and for the residuals respectively
#The variance between plots is reasonably high compared to the residual, around 30% of the total unexplained variance. Therefore, it matters

#Jump to diagnostics phase
plot(model)
#Check the normality

par(mfrow=c(1,2))
qqnorm(resid(model)); qqline(resid(model), col= 'red')
qqnorm(c(unlist(ranef(model)$plot))); qqline(c(unlist(ranef(model)$plot)), col= 'red')
#The distribution of the random effects looks normal. The distribution of the residuals looks symmetric although not normal

#For comparison, let's repeat the analysis without taking into account plot-specific effects
lm_model <- lm(hgt ~ spp*stocking , data=d)
lm_model_0_stocking <- lm(hgt ~ spp, data=d)
anova(lm_model, lm_model_0_stocking)
#In this case, the result is similar. But as we have seen in earlier sections, not taking the structure of your experiment into account may result in some very different results
#----------------------------------------------------------------------------------
#Random Intercepts and Random Slopes
#For simplicity, we will only consider single species, coded as 'dun'
dun <- d[d$spp == 'dun',]
model <- lmer(Stiffness ~ dbh+(1|plot), data= dun)

summary(model)
#Note that y= b0+ b1xi+ random_effects_ei+ err_i can be written as
          #y= (b0+ random_effects_ei)+ b1xi+ err_i
#In other words, the intercept (b0+ random_effects_ei) is different for each plot
pred <- expand.grid(dbh=seq(8,25,.1), plot= factor(unique(dun$plot)))
pred$Stiffness <- predict(model, newdata= pred)

library(ggplot2)

ggplot(data= pred, aes(x=dbh, y=Stiffness, group=plot))+
  geom_line(aes(col=plot))+
  xlab('dbh')+
  ylab('Stiffness')

#Random Slope
model_slope <- lmer(Stiffness ~ dbh+(dbh|plot), data= dun)
summary(model_slope)

#add a population average line to the visualisation
ranef(model_slope)
pred <- expand.grid(dbh=seq(8,25,.1), plot= factor(unique(dun$plot)))
pred$Stiffness <- predict(model_slope, newdata= pred)
pred.pop <- expand.grid(dbh= seq(8,25,.1), plot=factor(unique(dun$plot)))
pred.pop$Stiffness <- predict(model_slope, newdata= pred.pop, re.form= ~0)

ggplot(data= pred, aes(x= dbh, y= Stiffness, group= plot))+
  geom_line(aes(col=plot))+
  geom_line(data= pred.pop, aes(x= dbh, y= Stiffness), size= 2)+
  xlab('dbh')+
  ylab('Stiffness')

AIC(model)
AIC(model_slope)
#The AIC for random intercept-only model is smaller. Thus no evidence for random slopes

anova(model,model_slope)
#Getting the same result: no evidence for random slopes

#---------------------------------
#Fixed effects refer to coefficients which are the same for the entire population.
#Random effects vary from person to person or plot to plot.
#If you have repeatedly measured individual participant, you know that you need to include person-specific random effects in the model. If your trees are grouped into plots, you know that you need to include plot-specific random effects in the model.
#It seems intuitively clear that random effects can only apply to grouping variableswhether they are plot ID, or person ID. So if we see a continuous variable, we are pretty sure there cannot be any random effects connected to it.
'''But what about species in the tree data?
  Why don't we have species-specific random effects? What about stocking density?
  How do we know when a categorical variable should be treated as a fixed effect
and when should it be treated as a random effect?
  Firstly, if the groups were deliberately chosen because you want to compare them or
to adjust for them, then they are fixed effects. Because in the tree example,
we wanted to see the effect of species and stocking on various tree metrics,
these categorical variables are definitely fixed effects.
The plots however, just happened. We are not interested in comparison between these
specific plots. We just needed to select some plots, and these are what we've got.
In the hypertension example, we wanted to compare patients' blood pressure BEFORE
and AFTER the treatment. So before/after categorical variable is definitely a fixed effect
while the patients IDs are treated as random effects,
because we are not interested in selecting these specific patients. They just happened.
So far so good. But imagine a situation, where we were not interested in comparing
between tree species. We were just aware that there ARE different species, and so we recorded
this information. Does that make them random effects? Arguably it does. With one exception.
If the number of categories for your random effect is fairly low,
many people would still advise you to keep it as a fixed effect.
So, if we only had 3 species, we would treat them as fixed whatever our experimental design.
But if we had, say, 20 randomly occurring species, we would treat them as a random effect.
And that is arguably also why demographic variables such as sex and education
are always kept as a fixed effect. Whether they are part of the design or not,
the number of categories in them is usually too low for a random effect setting.