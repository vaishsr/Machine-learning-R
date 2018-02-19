air <- read.csv(file.choose(), header = TRUE, sep = ',')
summary(air)
air_data <- air[, 1:15]
summary(air_data)

str(air_data)
summary(air_data)


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

summary(air_data)

air_data_1 <- air_data[, -5]

library(VIM)
imp_air_data <-kNN(air_data_1, k=3) 
summary(imp_air_data)
imp_air_data <- imp_air_data[,1:14]
sum(is.na(imp_air_data))

imp_air_data$DateTime <- paste(imp_air_data$Date,imp_air_data$Time, sep = " ")
imp_air_data$DateTime <- as.POSIXlt(imp_air_data$DateTime,format="%m/%d/%Y %H:%M:%S")

summary(clean_air_data)

clean_air_data <- imp_air_data[complete.cases(imp_air_data),]
na.omit(clean_air_data)

install.packages('depmixS4')
library('depmixS4')
library(quantmod)

TSData<-data.frame(clean_air_data[,3],row.names=clean_air_data$DateTime)
head(TSData)
TSData_air<- as.xts(TSData)
plot(TSData_air)
names(TSData_air)
colnames(TSData_air)[colnames(TSData_air) == "clean_air_data...3."] <- 'ret.Close'

names(TSData_air)

gspcRets = diff(log(Cl(TSData_air)))
returns = as.numeric(gspcRets)
plot(gspcRets)

# Fit a Hidden Markov Model with two states 
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
?posterior

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='Days', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', xlab='Days', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2'), fill=1:2, bty='n')

# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')

