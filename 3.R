# 1
data("mtcars")
fit = lm(mpg~factor(cyl)+wt, mtcars)
summary(fit)

# 2
fit11 = lm(mpg~factor(cyl), mtcars)
summary(fit11)

# 3
fit2 = lm(mpg~factor(cyl)*wt, mtcars)
summary(fit2)
library(lmtest)
lrtest(fit,fit2)

# 4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit3 = lm(y~x)
hatvalues(fit3)

# 6
dfbeta(fit3)[,2]
