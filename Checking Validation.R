test <- read.xlsx("C:/Users/user/Desktop/迴歸分析/●Final Project/dataset整理-2014 - 刪除NA(26筆).xlsx")
library(modelr)
rsquare(m1)

#
library(caret)
td <- data.frame(y=test$Life.expectancy, x5=test$Total.expenditure, x6=test$Income.composition.of.resources, x9=test$`HIV/AIDS`, x17=test$Adult.Mortality, x19=test$Status)
result <- predict(m6, td)
cor(result, td$y)^2

plot(result, td$y)
abline(m6)

#Assessing model quality-----
install.packages("qpcR")
library(qpcR)
PRESS <- PRESS(c(m5), verbose = TRUE)
PRESS$stat

PRESS <- c(PRESS(c(m2), verbose = TRUE)$stat,PRESS(c(m4), verbose = TRUE)$stat, PRESS(c(m5), verbose = TRUE)$stat)

R_Sq_validation <- c(cor(predict(m2, td), td$y)^2,cor(predict(m4, td), td$y)^2,cor(predict(m5, td), td$y)^2)

data.frame(AIC(m2,m4,m5), BIC(m2,m4,m5),PRESS,R_Sq_validation)
