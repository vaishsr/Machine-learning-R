zilla<- read.csv(file.choose(), header= TRUE, sep = ",")
str(zilla)

#converting the factor variables to appropriate data types
zilla$additional_info <- as.character(zilla$additional_info)
zilla$amenities <- as.character(zilla$amenities)
zilla$city <- as.character(zilla$city)
zilla$description <- as.character(zilla$description)
zilla$highlight_value <- as.character(zilla$highlight_value)
zilla$hotel_star_rating <- as.factor(zilla$hotel_star_rating)
zilla$landmark <- as.character(zilla$landmark)
zilla$occupancy <- as.character(zilla$occupancy)
zilla$property_address <- as.character(zilla$property_address)
zilla$room_types <- as.character(zilla$room_types)
zilla$service_value <- as.character(zilla$service_value)
zilla$similar_hotel <- as.character(zilla$similar_hotel)
zilla$things_to_do <- as.character(zilla$things_to_do)
zilla$things_to_note <- as.character(zilla$things_to_note)
zilla$check_in_date1 <- as.Date(zilla$check_in_date, format = '%d-%m-%Y')
zilla$check_out_date1 <- as.Date(zilla$check_out_date, format = '%d-%m-%Y')

zilla$check_in_date1 <- as.POSIXlt(zilla$check_in_date1,format = '%d-%m-%Y')
zilla$check_out_date1 <- as.POSIXlt(zilla$check_out_date1,format = '%d-%m-%Y')

#creating new column to calculate the number of days customer stayed in the property
zilla$StayDays <- difftime(as.POSIXct(zilla$check_out_date1), as.POSIXct(zilla$check_in_date1, tz="UTC"), units="days")
zilla$StayDays <- as.numeric(zilla$StayDays)

# Exracting the price value from the character price column
zilla$room_price <- as.numeric(gsub("\\D", "", zilla$room_price))

# function to extract the number from the actual value and add the numbers to get the total number of occupants per booking
col_occupants = function(text) {
  if(nchar(text) > 15) {
    as.numeric(substr(text, 1, 2)) + as.numeric(substr(text, 10, 11))
  } else {
    as.numeric(substr(text, 1, 2)) + as.numeric(substr(text, 9, 10))
  }
}


#creating a column for the total number of occupants per booking in the property
zilla$No.ofoccupants <- sapply(zilla[,17],col_occupants)
Stayzilla <- zilla[, -c(3,4,6,7,12,14,17,18,20,21,23,24,27,30,33,34,35)]
names(Stayzilla)
str(Stayzilla)


#splitting the data into training and testing
# Take a random sample of training(75%) and testing (25%)
Splitting <- sample(nrow(Stayzilla), 0.80 * nrow(Stayzilla))
train_stayzilla = Stayzilla[Splitting,]
test_stayzilla = Stayzilla[-Splitting,]


#rearranging the data as need for the model creation
names(train_stayzilla)
poi_stayzilla <- train_stayzilla[, c(7,9,10,12,13,14,15,19,20)]
sum(is.na(omitted_data))
names(poi_stayzilla)
View(poi_stayzilla)

#omitted_data<- na.omit(poi_stayzilla)
#poi_stayzilla$StayDays[poi_stayzilla$StayDays == " "] <- NA

#missing value imputation
library(VIM)
imp_poi_stayzilla <-kNN(train_stayzilla, k=2) 
summary(imp_poi_stayzilla)
imp_poi_stayzilla_train <- imp_poi_stayzilla[,1:9]
sum(is.na(imp_poi_stayzilla))

#poisson regression to create the model to predict the number of occupants
model_occupants <- glm(No.ofoccupants ~ room_price+image_count+latitude+longitude+property_type, family = poisson, data = train_stayzilla)
model_occupants

#poisson regression to create the model to predict the number of staydays
model_stayDays <- glm(StayDays ~ No.ofoccupants+room_price+image_count+service_value+property_type, family = poisson, data = train_stayzilla)
model_stayDays

#Prediction on the poisson model for no of occupants
prediction_poi_1 <- predict(model_occupants, data = test_stayzilla)
prediction_poi_1

#Prediction on the poisson model for no of occupants
prediction_poi_2 <- predict(model_stayDays, data = test_stayzilla)
prediction_poi_2

#function for RMSE
rmse<- function(error)
{
  sqrt(mean(error^2))
}
#rmse value for the prediction of number of occupants
error_rate_no.of_occupants <- model_occupants$residuals 
rmse_model_occupants <- rmse(error_rate_no.of_occupants) 
rmse_model_occupants

#rmse value for the prediction of Staydays
error_rate_Staydays <- model_stayDays$residuals 
rmse_model_staydays <- rmse(error_rate_Staydays) 
rmse_model_staydays


#random forest to create the model to predict the number of occupants
library(randomForest)
occupant_rf <- randomForest(No.ofoccupants ~ room_price+image_count+property_type+StayDays, 
                            data = imp_poi_stayzilla)
occupant_rf
# Make and test predictions
prediction_rf <- predict(occupant_rf, newdata = test_stayzilla)
table(prediction_rf, test_stayzilla$No.ofoccupants)

library(InformationValue)
classificationError = misClassError(train_stayzilla$No.ofoccupants,test_stayzilla$No.ofoccupants)
classificationError
accuracy_rf = 1- classificationError
accuracy_rf


#Explanatory data analysis
# plot to see the room price in the dataset
hist(zilla$room_price) 
#Room prices are heavily right-skewed to a handful of extravegantly prized luxury listings

#plot to see the property type offered
barplot(table(zilla$property_type))
#Surprisingly, the vast majority of the properties on the site are not homes but (self-described) hotels. 

# plot to see the service values
barplot(table(zilla$service_value)) 
#The vast majority of properties on the StayZilla platform are unverified.

library(ggmap)
library(ggplot2)
library(sp)
Indiamap <- get_map(geocode("India"), scale = 2,zoom = 5)
ggmap(Indiamap)

# To support the model created - visualization on the Booking density based with the StayDays
circle_staydays <- ggmap(Indiamap) + geom_point(aes(x = longitude, y = latitude, size = zilla$StayDays),
data = zilla,col = "blue", alpha = .5, shape = 1, stroke =2)

circle_staydays +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = zilla) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map of Booking Density in India with Staydays")

# To support the model created - visualization on the Booking density based with the No. of Occupants
circle_occupants <- ggmap(Indiamap) + geom_point(aes(x = longitude, y = latitude, size = zilla$No.ofoccupants),
                                                data = zilla,col = "black", alpha = .5, shape = 5, stroke =2)

circle_occupants +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = zilla) +
  scale_fill_gradient(low = "black", high = "green")+
  ggtitle("Map of booking Density in India with number of occupants")

#Most properties are located in northern India, which appears to be where the platform is most popular.