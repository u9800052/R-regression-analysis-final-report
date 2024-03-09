install.packages("openxlsx")
library(openxlsx)
library(ggplot2)
library(gplots)
#import data-----
dataset <- read.xlsx("C:/Users/user/Desktop/迴歸分析/●Final Project/dataset整理-2014 - 刪除NA(105筆).xlsx")
y <- dataset$Life.expectancy
x1 <- dataset$Population
x2<- dataset$Schooling 
x3 <- dataset$GDP
x4 <- dataset$percentage.expenditure
x5 <- dataset$Total.expenditure
x6 <- dataset$Income.composition.of.resources
x7 <- dataset$Alcohol
x8 <- dataset$BMI
x9 <- dataset$`HIV/AIDS`
x10 <- dataset$Hepatitis.B
x11 <- dataset$Measles
x12 <- dataset$Diphtheria 
x13 <- dataset$Polio
x14 <- dataset$`thinness.1-19.years`
x15 <- dataset$`thinness.5-9.years`
x16 <- dataset$`under-five.deaths`
x17 <- dataset$Adult.Mortality
x18 <- dataset$infant.deaths
x19 <- dataset$Status
#-----
#繪製scatterplot matrix
pairs(cbind(x5,x6,x9), pch=20,lower.panel = NULL)
#不含類別變數全放-----
m <- lm(log(y) ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19)
summary(m)
#x5+x6+x9+x17顯著
m1 <- lm(y ~ x5+x6+x9+x17)
summary(m1)
#Partial F-Test
m2 <- lm(y ~ x6+x9+x17)
m3 <- lm(y ~ x5+x9+x17)
m4 <- lm(y ~ x5+x6+x17)
m5 <- lm(y ~ x5+x6+x9)
summary(m5)
anova(m1,m2)
#加入交互作用
m6 <- lm(y ~ x5+x6+x9+poly(x9, 2, raw = TRUE))
summary(m6)


m7 <- lm(y ~ x5+x6+x9+x17+x19+x9*x17+poly(x9, 2, raw = TRUE))
summary(m7)
#子集
m5 <- lm(y[-1] ~ x5[-1]+x6[-1]+x9[-1]+x17[-1]+x9[-1]*x17[-1], data=dataset, subset=(x19==0))
summary(m5)

m <- lm(y[-1] ~ x5[-1]+x6[-1]+poly(x9[-1], 2, raw = TRUE)+x17[-1]+x9[-1]*x17[-1])
summary(m)
#相關係數
install.packages("psych")
library("psych")
pairs.panels(dataset[,c(7,9,10,19)])
#參考資料-----
#多項式迴歸
m123 <- lm(y ~ poly(x2, 2, raw = TRUE)) 
#以資料子集建模
m1ing <- lm(y ~ x5+x6+x9+x17+x19, data=dataset, subset=(x19==0))
m1ed <- lm(y ~ x5+x6+x9+x17+x19, data=dataset, subset=(x19==1)) 
m1ing <- lm(y ~ x5+x6+x9+x17, data=dataset)
m1ed <- lm(y ~ x5+x6+x9+x17+x19, data=dataset)
anova(m1ing, m1ed)
  