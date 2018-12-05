library(MASS)
attach(UScrime)

pairs(UScrime[,])

y.lm=lm(y~.,data=UScrime) #linear regression for response y using all other variables as predictors
summary(y.lm)
par(mfrow = c(2,2))
plot(y.lm)

step(y.lm)

# Most significant variables are M, Ed, Ineq, and Prob
y2.lm=lm(y~M+Ed+Ineq+Prob,data=UScrime)
summary(y2.lm)
par(mfrow = c(2,2))
plot(y2.lm)

# k-fold validation
library(boot)
set.seed(1)
#train=sample(47,25)
y.fit=glm(y~.,data=UScrime)
y2.fit=glm(y~M+Ed+Ineq+Prob,data=UScrime)

cv.errors=cv.glm(UScrime,y.fit,K=10)$delta
cv.err=cv.errors[1]
cv.err

cv.errors2=cv.glm(UScrime,y2.fit,K=10)$delta
cv.err2=cv.errors2[1]
cv.err2