#繪製回歸線
plot(m2$fitted.values)
ggplot(mydata,aes(x,y))+
  geom_point()+
  stat_smooth(method=lm,
              se=TRUE,
              level=0.99,)+
  labs(x="schooling",
       y="life_expectancy",) #FLASE為不繪製誤差帶
#繪製散佈圖
plot(m2)
ggplot(m2, aes(schooling, life_expectancy)) + 
  geom_point()


plot(schooling, life_expectancy)

Longevity <- dataset
m2 <- lm(GDP ~ Longevity)
summary(m2)
plot(Longevity, GDP)

Alcohol <- dataset$Alcohol
m3 <- lm(GDP ~ Alcohol)
summary(m3)
plot(Alcohol, GDP)

Schooling <- dataset$Schooling
m4 <- lm(GDP ~ Schooling)
summary(m4)
plot(Schooling, GDP)

Schooling <- dataset$Schooling
m4 <- lm(GDP ~ Schooling)
summary(m4)
plot(Schooling, GDP)

plot(dataset$percentage.expenditure, dataset$GDP)
m5 <- lm(GDP ~ dataset$percentage.expenditure)
summary(m5)


predictor2 <- dataset$Alcohol
m2 <- lm(response ~ predictor + predictor2)

anova(m2)
coefficients(m2)
fitted(m1)

confint(m2)
confint(m1, level=0.99) #迴歸係數的信賴區間

resid(m1)
install.packages("broom")
library(broom)
augmented_m1 <- augment(m1)
library(ggplot2)
ggplot(augmented_m1, aes(x=.fitted, y=.resid))+
  geom_point()

par(mfrow=c(1,2))
pairs(model$model)

ggplot(data=dataset)+
  geom_point(mapping=aes(x=BMI, y=GDP, color=Status))

qqnorm(predictor)
plot(residuals(model))
outlier.test(model)

# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics
