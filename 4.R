# 1
library(MASS)
fit <- glm(use~wind,family="binomial",data=shuttle)
exp(fit$coefficients)

# 2
fit2 <- glm(use~wind+magn,family="binomial",data=shuttle)
exp(fit2$coefficients)

# 3
shuttle$use.bin <- as.integer(shuttle$use) - 1
fit3 <- glm(I(1-use.bin)~wind,family="binomial",data=shuttle)
summary(fit3)
summary(fit)

# 4
head(InsectSprays)
fit4 <- glm(count~factor(spray)-1,family="poisson",data=InsectSprays)
exp(fit4$coefficients)
exp(fit4$coefficients)[1] / exp(fit4$coefficients)[2]

# 5
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots <- 0
splineTerms <- sapply(knots,function(knot)(x>knot)*(x-knot))
xMat <- cbind(x,splineTerms)
yhat <- predict(lm(y~xMat))
plot(x,y,frame=FALSE,pch=21,bg="lightblue")
lines(x,yhat,col="red",lwd=2)
summary(lm(y~xMat))
