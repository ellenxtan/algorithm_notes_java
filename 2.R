# 1 2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y~x)
summary(fit)

# 3
data(mtcars)
x = mtcars$wt
y = mtcars$mpg
fit = lm(y~x)
summary(fit)
pre = predict(fit, newdata = data.frame(x=mean(mtcars$wt)),interval = "confidence")
pre

# 5
pre2 = predict(fit, newdata = data.frame(x=3), interval=("prediction"))
pre2

# 6
fit2 = lm(y~I(x/2))
coef2 <- coef(summary(fit2))
(coef2[2,1] + c(-1, 1) * qt(.975, df = fit2$df) * coef2[2, 2])

# 9 Y=mean(Y), model only have intercept
sum(resid(fit)^2) / sum((y - mean(y)) ^ 2)
