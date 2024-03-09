library(ggplot2)
library(broom)
#繪製散佈圖(二維)-----
plot(schooling, life_expectancy)
ggplot(dataset)+ 
  geom_point(mapping=aes(x=x2, y=y, color=x19))
#區分發展中及已開發國家
ggplot(dataset1)+
  geom_point(mapping=aes(x=schooling, y=life_expectancy, color=Status)) 
#繪製迴歸線(二維)-----
install.packages("broom")
library(broom)
ggplot(m2,aes(x=x2,y=y))+
  geom_point()+
  stat_smooth(method=lm,
              se=TRUE,
              level=0.95,)+
  labs(x="schooling",
       y="life_expectancy",) #FLASE為不繪製誤差帶
#模型診斷
#模型資訊-----
confint(m1, level=0.95) #迴歸係數的信賴區間

plot(density(m1$residuals)) #for visually checking whether or not the residuals are normally distributed. If the plot is roughly bell-shaped, then the residuals likely follow a normal distribution

install.packages("sandwich")
library(sandwich)
vcovHC(m1) #var-cov matrix

library(car) 
avPlot(m1 ,x17) #Added-Variable Plots

residualPlot(m2, fitted=TRUE, type="rstudent", tests=TRUE)
infIndexPlot(m1, vars="cook")
infIndexPlot(m2, vars="studentized")
infIndexPlot(m2, vars="hat")

anova(m, m1)
anova(m1, m2)
deviance(m3) #SSE
preds <- data.frame(schooling=c(6,9,12,16,18))
predict(m1, newdata = preds, interval = "prediction")
#繪製診斷圖-----
#四合一診斷圖
install.packages("ggfortify")
library(ggfortify)
autoplot(m6)
plot(y)
#殘差圖
install.packages("broom")
library(broom)
ggplot(augment(m5), aes(x=.fitted, y=.resid))+
  geom_point()
#QQ-plot
qqnorm(x17) #個別變數
stem(x17)

plot(y, which=2) 
ggplot(m2, aes(sample=m2$residuals)) +  
  stat_qq() + 
  stat_qq_line()+
  labs(x="x2",
       y="Residuals",)
#繪製資料分佈矩陣
install.packages("GGally")
library(GGally)
ggpairs(y)
#檢定資訊-----
# 常態性檢定
shapiro.test(m6$residual)
# 殘差獨立性檢定
install.packages("car")
require(car)
durbinWatsonTest(m6)
#殘差變異數同質性檢定
require(car)
ncvTest(m6)
#VIF檢定
install.packages("car")
library(car)
vif(m6, type="terms")
vif(m7, type="predictor") #有交互作用項時使用
install.packages("car")
library(car)
#識別異常資料點
outlierTest(m2)
#識別有(過度)影響力觀察值
influence.measures(m3) 
#cook'2 dist.
library(olsrr)
par(mfrow=c(2,2))

ols_plot_cooksd_chart(m5)
