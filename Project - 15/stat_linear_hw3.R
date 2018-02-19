install.packages("faraway")
library(faraway)
data(dvisits)

#structure and summary of the dataset
str(dvisits)
summary(dvisits)

#looking at the data distribution
library(rcompanion)
plotNormalHistogram(dvisits)

#taking square root transformation
dvisit_sqrt = sqrt(dvisits)
plotNormalHistogram(dvisit_sqrt)

#taking cube root transformation
dvisits_cub = sign(dvisits) * abs(dvisits)^(1/3)
plotNormalHistogram(dvisits_cub)

#checking for missing values
sum(is.na(dvisits_cub))

#checking for multicollinearity
summary(dvisits_cub)
vif(dvisits_cub)
?dvisits
#removing the highly collinear features

dvisit_newdata <- dvisits_cub[, -3]
dvisit_newdata <- dvisit_newdata[, -16]

summary(dvisit_newdata)

vif(dvisit_newdata)


#Perform the variable selection method to sustain the significant variables in the model
#nullmodel
nullmodel <- lm(hospdays ~ 1, data = dvisit_newdata)
summary(nullmodel)

#fullmodel
fullmodel <- lm(hospdays~., data = dvisit_newdata)
summary(fullmodel)

#variable selection
forward <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction="forward")
backward <- step(fullmodel, data=dvisit_newdata, direction="backward")
stepwise <-  step(nullmodel, scope = list(upper=fullmodel), data=dvisit_newdata, direction="both")


#final model with lowest AIC value
try_model  <- lm(hospdays ~   freerepa + actdays + hospadmi+ chcond2 +age+ nondocco , data = dvisit_newdata , subset = )
summary(try_model)

# regression diagonistics
par(mfrow = c(2, 2))
plot(try_model)

plot(fitted(try_model), residuals(try_model), xlab = "Fitted", ylab = "Residuals")
abline(h = 0)

qqnorm(residuals(try_model), ylab = "Residuals", main = "")
qqline(residuals(try_model))

hist(residuals(try_model), xlab = "Residuals", main = "")


library("lmtest")
bptest(try_model)
