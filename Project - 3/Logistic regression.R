quality = read.csv(file.choose(), header=T, sep=",")

quality <- quality[,-1]
View(quality)
str(quality)

#recode Good as 1 and Bad as 0
quality$label<-ifelse(quality$label == "G", 1, 0)

#Auto split the data for training and testing into 80 percent
train_data = sample(1:nrow(quality), 8.0*nrow(quality)/10)
train_data

test_data = (-train_data)
test_data
quality.test = quality[test_data,]
quality.train = quality[train_data,]

str(quality.train)
str(quality.test)

model.fit = glm(label ~ . -num_characters -num_sentences -num_punctuations, family=binomial(link="logit"),data=quality.train)
summary(model.fit)

results = predict(model.fit,newdata=quality.test,type="response")

quality$label
  names(quality.test)
results = ifelse(results>0.5, 1, 0)
results

misClassError = mean(results != quality.test$label)
misClassError
accuracy = 1-misClassError
accuracy

cor(quality)
