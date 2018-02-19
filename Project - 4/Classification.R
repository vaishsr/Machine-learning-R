library("ggvis")
cancer <- read.csv(file.choose(), header = TRUE, sep = ',')
cancer <- cancer[,-1]
View(cancer)
str(cancer)

#scaling the data
scale_cancer <- as.data.frame(scale(cancer[,-1]))
scale_cancer$Diagnosis <- cancer$Diagnosis

#splitting the training and testing data
set.seed(1234)
ind <- sample(2, nrow(scale_cancer), replace=TRUE, prob=c(0.75, 0.25))
names(scale_cancer)
cancer.training <- scale_cancer[ind==1, 1:10]
cancer.test <- scale_cancer[ind==2, 1:10]
cancer.trainLabels <- scale_cancer[ind==1,11]
cancer.testLabels <- scale_cancer[ind==2,11]


# Run the kNN classification
library(class)
cancer_pred <- knn(train = cancer.training, test = cancer.test, cl = cancer.trainLabels, k=3)

# Compute the corss tabulation (confusion matrix)
library(gmodels)
CrossTable(x=cancer_pred, y=cancer.testLabels, prop.chisq = FALSE)
#accuracy computation
accuracy = (84+55)/nrow(cancer.test)
accuracy

Error = 1- accuracy
Error
#*********************************************************************************************************************

abalone <- read.csv(file.choose(), header = TRUE, sep = ',')
View(abalone)  
str(abalone)
summary(abalone)  
barplot(table(abalone$Rings))

# Take a random sample of training(75%) and testing (25%)
set.seed(1234)
abalone_split <- sample(nrow(abalone), 0.75 * nrow(abalone))
train_abalone = abalone[abalone_split,]
test_abalone = abalone[-abalone_split,]

# Train a model using Random Forest
library(randomForest)
model_abalone <- randomForest(Rings ~ . , data = train_abalone)
model_abalone

# Make and test predictions
prediction_abalone <- predict(model_abalone, newdata = test_abalone)

RMSE <- sqrt(sum((prediction_abalone - test_abalone$Rings)^2)/length(prediction_abalone))

RMSE

#*********************************************************************************************************************

rest_Tip <- read.csv(file.choose(), header = TRUE, sep = ',')
View(rest_Tip)
str(rest_Tip)
summary(rest_Tip)

library(rpart)
library(tree)
attach(rest_Tip)

names(rest_Tip)

fitModel_tip <- rpart(Tip ~ Card+Ad+Joke+None, data = rest_Tip, method = "class",control = rpart.control(minsplit=2, minbucket=1))
summary(fitModel_tip)

printcp(fitModel_tip)

plotcp(fitModel_tip)

plot(fitModel_tip, uniform=TRUE, main="Decision Tree for Tip")
text(fitModel_tip, use.n=TRUE, all=TRUE, cex=.8)


tree_fit <- tree(Tip ~ Card+Ad+Joke+None, data = rest_Tip)
plot(tree_fit)
text(tree_fit,pretty = 0)
summary(tree_fit)


