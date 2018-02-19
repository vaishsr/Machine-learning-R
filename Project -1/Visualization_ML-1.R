
#Loading the input file in to R studio
Rest_ratings <- read.table(file.choose(), header = TRUE, sep = '\t')

#visualize the data
View(Rest_ratings)

#statistical information on the data
str(Rest_ratings)

#Multiple linear regression model
over <- lm(rating ~ food+ambience+service , data = Rest_ratings )

#Gives summary of the model fit
summary(over)

#library for data visualization
library(ggplot2)

#Using ggplot with the method "lm" to indicate that it is a linear model
ggplot(Rest_ratings, aes(x= food+ambience+service, y=rating))+geom_point()+stat_smooth(method= "lm")+
  labs(x = "Aspects of Restuarant", y = "OverAll Ratings")



ggplot(Rest_ratings)+geom_jitter(aes(restaurant,rating), colour ="blue" ) + geom_smooth(aes(restaurant, rating), method =lm, se = FALSE)+
             geom_jitter(aes(food,rating), colour ="green" ) + geom_smooth(aes(food, rating), method =lm, se = FALSE)+
  geom_jitter(aes(ambience,rating), colour ="Red" ) + geom_smooth(aes(ambience, rating), method =lm, se = FALSE)+  
  geom_jitter(aes(service,rating), colour ="black" ) + geom_smooth(aes(service, rating), method =lm, se = FALSE)+
  labs(x = "Aspects of Restuarant", y = "OverAll Ratings")
  
  
  
  
require(reshape2)
custRating = melt(ratings, id.vars='rating')
ggplot(custRating) +
  geom_jitter(aes(value,rating, colour=variable),) + geom_smooth(aes(value,rating, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Restuarant's Aspect", y = "Customer Ratings")


