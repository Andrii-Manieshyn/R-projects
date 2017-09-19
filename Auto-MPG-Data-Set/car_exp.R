auto_mpg <- read.table("C:\\Users\\Andrii\\Documents\\r-project\\Auto-MPG-Data-Set\\data\\auto-mpg-with-header.data", header= TRUE)
plot(mpg~horsepower,auto_mpg)
## Simple linear regressinon
names(auto_mpg)
fit1 = lm(mpg~horsepower, data=auto_mpg)
fit1
summary(fit1)
abline(fit1, col="red")
names(fit1)
confint(fit1)
predict(fit1, data.frame(horsepower=c(111, 222, 333)))
### Multiple linear regression
fit2 = lm(mpg~horsepower+weight, data=auto_mpg)
summary(fit2)
fit3 = lm(mpg~.-car_name, data=auto_mpg)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

### Nonlinear terms and Interactions
fit5=lm(mpg~acceleration*cylinders, data=auto_mpg)
summary(fit5)
fit6=lm(mpg~acceleration*cylinders*horsepower, data=auto_mpg)
summary(fit6)
fit7=lm(mpg~acceleration*cylinders*horsepower, data=auto_mpg)
summary(fit7)


fit8=lm(mpg~acceleration + I(acceleration^2), data=auto_mpg); summary(fit8)
par(mfrow=c(2,2))
plot(fit8)
summary(fit8)

fit10=lm(mpg~horsepower + acceleration, data=auto_mpg); summary(fit10)
par(mfrow=c(2,2))
plot(fit10)


fit_res=lm(mpg~.-car_name-model_year-origin-acceleration-cylinders+ I(horsepower^2) + horsepower:displacement, data=auto_mpg)
par(mfrow=c(2,2))
plot(fit_res)
summary(fit_res)


# mpg changes responds on changes in acclereration parameters. 
# Other experements shows that other parameters have bad final results with residials