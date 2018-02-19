concrete_data <- read.csv(file.choose(), header = TRUE, sep = ',')
View(concrete_data)
str(concrete_data)
summary(concrete_data)

#grouping data based on slump
concrete_slump <- concrete_data[,2:9]
str(concrete_slump)

#grouping data based on Flow
concrete_Flow <- concrete_data[,2:8]
concrete_Flow$FLOW.cm. <- concrete_data$FLOW.cm.
str(concrete_Flow)

#grouping data based on Compressive Strength
concrete_strength <- concrete_data[,2:8]
concrete_strength$Compressive.Strength <- concrete_data$Compressive.Strength..28.day..Mpa.
str(concrete_strength)

library(mclust)

#fit for slump based cluster
fit_slump <- Mclust(concrete_slump)
summary(fit_slump, parameters = TRUE)
plot(fit_slump, what = "BIC")
plot(fit_slump, what = "classification")
plot(fit_slump, what = "density")


#fit for Flow based cluster
fit_flow <- Mclust(concrete_Flow)
summary(fit_flow, parameters = TRUE)
plot(fit_flow, what = "BIC")
plot(fit_flow, what = "classification")
plot(fit_flow, what = "density")


#fit for Compressive strength based cluster
fit_strength <- Mclust(concrete_strength)
summary(fit_strength, parameters = TRUE)
plot(fit_strength, what = "BIC")
plot(fit_strength, what = "classification")
plot(fit_strength, what = "density")


#Uncertainity plot using BIC for Slump based cluster
SlumpBIC <- mclustBIC(concrete_slump)
summary(SlumpBIC)
plot(SlumpBIC)
#Uncertainity plot using BIC for flow based cluster
flowBIC <- mclustBIC(concrete_Flow)
summary(flowBIC)
plot(flowBIC)
#Uncertainity plot using BIC for strength based cluster
strengthBIC <- mclustBIC(concrete_strength)
summary(strengthBIC)
plot(strengthBIC)
