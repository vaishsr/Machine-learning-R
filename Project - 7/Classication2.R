overDrawn = read.csv(file.choose(), header=T, sep=",")
overDrawn <- overDrawn[,-1]
View(overDrawn)
str(overDrawn)
summary(overDrawn)
overDrawn$Overdrawn <- as.factor(overDrawn$Overdrawn)

library(VIM)
overDrawn_imp <- kNN(overDrawn,k=3)
overDrawn_imp <- overDrawn_imp[, 1:4]


summary(overDrawn_imp)

#Logistic Regression
#Auto split the data for training and testing into 80 percent
set.seed(1111)
train_over = sample(1:nrow(overDrawn_imp), 7.5*nrow(overDrawn_imp)/10)
test_over = (-train_over)
overDrawn.train = overDrawn_imp[train_over,]
overDrawn.test = overDrawn_imp[test_over,]
str(overDrawn.train)
str(overDrawn.test)

model.over = glm(Overdrawn ~ ., family=binomial(link="logit"),data=overDrawn.train)
summary(model.over)

results_pred = predict(model.over,newdata=overDrawn.test,type="response")
results_pred = ifelse(results_pred>0.5, 1, 0)
results_pred

misClassErr = mean(results_pred != overDrawn.test$Overdrawn)
misClassErr
accuracy = 1-misClassErr
accuracy


#*************************************************************************************************

#kNN classification

# Randomly split the data in training (2/3rd) and testing (1/3rd)
set.seed(1234)
overKNN <- sample(2, nrow(overDrawn_imp), replace=TRUE, prob=c(0.70, 0.30))
overKNN
overKNN_train <- overDrawn_imp[overKNN==1, 1:3]
overKNN_test <- overDrawn_imp[overKNN==2, 1:3]
overKNN_trainLabels <- overDrawn_imp[overKNN==1,4]
overKNN_testLabels <- overDrawn_imp[overKNN==2,4]

# Run the kNN classification
library(class)
overKNN_pred <- knn(train = overKNN_train, test = overKNN_test, cl = overKNN_trainLabels, k=3)


# Compute the corss tabulation (confusion matrix)
library(gmodels)
CrossTable(x=overKNN_pred, y=overKNN_testLabels, prop.chisq = FALSE)


#**************************************************************************************************


#Random forest

# Take a random sample of training(75%) and testing (25%)

over_rf <- overDrawn_imp
over_rf$Sex <- as.factor(over_rf$Sex)
str(over_rf)

over_rf$DaysDrink

if(over_rf$DaysDrink[over_rf$DaysDrink] > 7 ) print("hi")

set.seed(2345)
over_split <- sample(nrow(over_rf), 0.75 * nrow(over_rf))
overtrain_rf = over_rf[over_split,]
overtest_rf = over_rf[-over_split,]

# Train a model using Random Forest
library(randomForest)
model_rf <- randomForest(Overdrawn ~ . , data = overtrain_rf)
model_rf

# Make and test predictions
overRF_pred <- predict(model_rf, newdata = overtest_rf)
table(overRF_pred, overtest_rf$Overdrawn)
(103+0)/nrow(overtest_rf)

#*****************************************************************************************

#Naive Bayes


str(overDrawn_imp)

overDrawn_imp$DaysDrink[which(overDrawn_imp$DaysDrink < 7)] <- 0
overDrawn_imp$DaysDrink[which(overDrawn_imp$DaysDrink >= 7 & overDrawn_imp$DaysDrink < 14)] <- 1
overDrawn_imp$DaysDrink[which(overDrawn_imp$DaysDrink >= 14)] <- 2
overDrawn_imp$DaysDrink <- as.factor(overDrawn_imp$DaysDrink)

overDrawn_imp$Age[which(overDrawn_imp$Age <= 20)] <- 0
overDrawn_imp$Age[which(overDrawn_imp$Age == 21 | overDrawn_imp$Age == 22)] <- 1
overDrawn_imp$Age[which(overDrawn_imp$Age > 22)] <- 2

overDrawn_imp$Age <- as.factor(overDrawn_imp$Age)
overDrawn_imp$Sex <- as.factor(overDrawn_imp$Sex)

library(e1071)
library(caret)
# Prepare training and testing data
set.seed(1234)
over_split_nb <- sample(nrow(overDrawn_imp), 0.75 * nrow(overDrawn_imp))
overtrain_nb = overDrawn_imp[over_split_nb,]
overtest_nb = overDrawn_imp[-over_split_nb,]

# Build the NB model
nb_model <- naiveBayes(Overdrawn~., data = overtrain_nb)
print(nb_model)

# Predict
prediction_nb <- predict(nb_model, newdata = overtest_nb)
print(prediction_nb)

# Evaluate
confusionMatrix(prediction_nb, overtest_nb$Overdrawn)

