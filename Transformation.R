library(MASS)
library(broom)
#自變數取log
plot(log(data$GDP), data$Life.expectancy)
ml <- lm(data$Life.expectancy ~ log(data$GDP))
summary(ml)
#Box-Cox轉換(對y做)
bc <- boxcox(m1)
which.max(bc$y)
lambda <- bc$x[which.max(bc$y)] #求對應的lambda位置
lambda
z <- y^lambda
mbc <- lm(z ~ x5+x6+x9+x17)
summary(mbc)
ggplot(augment(mbc), aes(x=.fitted, y=.resid))+
  geom_point()
#診斷-----
#繪製轉換前後殘差圖比較
plot(m4, which=1)
plot(mbc, which=1)
#比較轉換前後R^2
summary(m2)
summary(mbc)
shapiro.test(mbc)
durbinWatsonTest(mbc)
ncvTest(mbc)
