air <- read.csv(file.choose(), header = TRUE, sep = ',')
summary(air)
View(air)
air_data <- air[, 1:15]

str(air_data)
summary(air_data)
View(air)


air_data$CO.GT.[air_data$CO.GT. == -200] <- NA
air_data$PT08.S1.CO.[air_data$PT08.S1.CO. == -200] <- NA
air_data$NMHC.GT.[air_data$NMHC.GT.== -200]<- NA
air_data$C6H6.GT.[air_data$C6H6.GT.== -200]<- NA
air_data$PT08.S2.NMHC.[air_data$PT08.S2.NMHC. == -200]<- NA
air_data$NOx.GT.[air_data$NOx.GT. == -200]<- NA
air_data$PT08.S3.NOx.[air_data$PT08.S3.NOx. == -200]<- NA
air_data$NO2.GT.[air_data$NO2.GT. == -200]<- NA
air_data$PT08.S4.NO2.[air_data$PT08.S4.NO2. == -200]<- NA
air_data$PT08.S5.O3.[air_data$PT08.S5.O3. == -200]<- NA
air_data$T[air_data$T== -200]<- NA
air_data$RH[air_data$RH== -200]<- NA
air_data$AH[air_data$AH== -200]<- NA

str(air_data)

air_data$DateTime <- paste(air_data$Date,air_data$Time, sep = " ")

air_data <- air_data[ , 3:16]

air_data$DateTime <- as.POSIXlt(air_data$DateTime,format="%m/%d/%Y %H:%M:%S", tz='UTC')

summary(air_data)

# attribute - NMHC.GT. can be dropped as the 90% of its data are missing values

air_data <- air_data[ ,-3 ]
str(air_data)


#missing value imputation
library(mice)
air_data_w_o <- air_data[, -13]
str(air_data_w_o)

air_imputed_data <- mice(air_data, m = 5, maxit = 50, method = 'pmm', seed =500)
summary(air_imputed_data)
air_imputed_data$imp$CO.GT.
air_completeData <- complete(air_imputed_data,3)
summary(air_completeData)
str(air_completeData)

air_completeData$DateTime <- air_data$DateTime

str(final_air_data)

final_air_data<- na.omit(air_completeData)



# Fit a Hidden Markov Model with two states 
install.packages('depmixS4')
library('depmixS4')
library(quantmod)


TSData<-data.frame(air_completeData[,],row.names=air_completeData$DateTime)

time_air<- as.xts(TSData)
class(time_air)
plot(time_air)
model_data <- final_air_data[, 11:13]
str(model_data)
model_data <-model_data[,-2]
hmm <- depmix(RH ~ 1, family = gaussian(), nstates = 2, data= data.frame(RH = RH))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
