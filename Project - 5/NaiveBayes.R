#loading all the training datasets
uTube1 <- read.csv(file.choose(), header=T, sep=",")
uTube2 <- read.csv(file.choose(), header=T, sep=",")
uTube3 <- read.csv(file.choose(), header=T, sep=",")
uTube4 <- read.csv(file.choose(), header=T, sep=",")

#Merging them into one dataset
uTube_train <- rbind(uTube1,uTube2,uTube3,uTube4)

#Loading the testing dataset
uTube_test <- read.csv(file.choose(), header = T, sep = ",")
uTube_test$CONTENT <- as.character(uTube_test$CONTENT)

#changing the class variables into factors 
uTube_train$CLASS <- as.factor(uTube_train$CLASS)
uTube_test$CLASS <- as.factor(uTube_test$CLASS)

#Data Preprocessing - cleaning
uTube_train$CONTENT <- gsub(pattern = "\\d", replacement = " " , uTube_train$CONTENT)
uTube_train$CONTENT <- gsub(pattern = "\\b[A-z]\\b{1}", replacement = " " , uTube_train$CONTENT)
uTube_train$CONTENT <- removeWords(uTube_train$CONTENT, stopwords())
uTube_train$CONTENT <- removeWords(uTube_train$CONTENT,stopwords())
uTube_train$CONTENT <- tolower(uTube_train$CONTENT)
uTube_train$CONTENT <- stemDocument(uTube_train$CONTENT)
uTube_train$CONTENT <- removePunctuation(uTube_train$CONTENT)
uTube_train$CONTENT <- stripWhitespace(uTube_train$CONTENT)

head(uTube_train$CONTENT)

#adding new features to the dataset on word and character counts
uTube_train$CHAR_COUNT <- nchar(uTube_train$CONTENT)
uTube_train$WORD_COUNT <- sapply(gregexpr("\\W+", uTube_train$CONTENT), length) + 1
uTube_test$CHAR_COUNT <- nchar(uTube_test$CONTENT)
uTube_test$WORD_COUNT <- sapply(gregexpr("\\W+", uTube_test$CONTENT), length) + 1

head(uTube_test)
View(uTube_train)
names(uTube_train)

# Train the training dataset with the Naive Bayes classifier
library(e1071)
spamFilter <- naiveBayes(CLASS~ CHAR_COUNT+WORD_COUNT+AUTHOR+CONTENT, data = uTube_train)
print(spamFilter)

# Predicting the testing data with the trained model
pred_spam <- predict(spamFilter, newdata = uTube_test)
print(pred_spam)

# Evaluate the accuracy of the model
library(caret)
confusionMatrix(pred_spam, uTube_test$CLASS)

