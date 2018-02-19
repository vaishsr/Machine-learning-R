pollution <- read.csv(file.choose(), header = TRUE, sep = ',')
str(pollution)
summary(pollution)
plot(pollution, pch = 4)

mortality_rate <- lm(y ~ ., data = pollution)
summary(mortality_rate)

mortality_rate_1 <- lm(y ~ ï..x1+x2+x3+x9+x6, data = pollution)
summary(mortality_rate_1)
# display the predictions
predicted_mort_rate <- predict(mortality_rate_1, pollution)

#function for RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}
error_morta_rate <- mortality_rate_1$residuals 
PredictionRMSE_linearreg <- rmse(error_morta_rate) 
PredictionRMSE_linearreg

library(e1071)
mortality_rate_2 <- svm(y ~ . , pollution)
predictedSVMY <- predict(mortality_rate_2, pollution)
#Caculating error 
error_svm_mort_rate <- pollution$y - predictedSVMY
#calling RMSE function
svmPredictionRMSE_mr <- rmse(error_svm_mort_rate)
svmPredictionRMSE_mr

#tuning the SVM model
svm_tune_mort_rate <- tune(svm, y~ ., data = pollution, ranges=list(epsilon=seq(0,1,0.1), cost=seq(1,10,1)))

best_mod_mort_rate <- svm_tune_mort_rate$best.model
best_pred_mort_rate <- predict(best_mod_mort_rate, data=pollution)
error_best_mort_rate <- pollution$y - best_pred_mort_rate
best_RMSE_mort_rate <- sqrt(mean(error_best_mort_rate^2))
best_RMSE_mort_rate 

