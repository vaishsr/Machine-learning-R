library(data.table)
library(neuralnet)
NNdata <-  fread("https://archive.ics.uci.edu/ml/machine-learning-databases/00198/Faults.NNA")
summary(NNdata)
str(NNdata)
setnames(NNdata, "V1", "X_Minimum")
setnames(NNdata, "V2", "X_Maximum")
setnames(NNdata, "V3", "Y_Minimum")
setnames(NNdata, "V4", "Y_Maximum")
setnames(NNdata, "V5", "Pixels_Areas")
setnames(NNdata, "V6", "X_Perimeter")
setnames(NNdata, "V7", "Y_Perimeter")
setnames(NNdata, "V8", "Sum_of_Luminosity")
setnames(NNdata, "V9", "Minimum_of_Luminosity")
setnames(NNdata, "V10", "Maximum_of_Luminosity")
setnames(NNdata, "V11", "Length_of_Conveyer")
setnames(NNdata, "V12", "TypeOfSteel_A300")
setnames(NNdata, "V13", "TypeOfSteel_A400")
setnames(NNdata, "V14", "Steel_Plate_Thickness")
setnames(NNdata, "V15", "Edges_Index")
setnames(NNdata, "V16", "Empty_Index")
setnames(NNdata, "V17", "Square_Index")
setnames(NNdata, "V18", "Outside_X_Index")
setnames(NNdata, "V19", "Edges_X_Index")
setnames(NNdata, "V20", "Edges_Y_Index")
setnames(NNdata, "V21", "Outside_Global_Index")
setnames(NNdata, "V22", "LogOfAreas")
setnames(NNdata, "V23", "Log_X_Index")
setnames(NNdata, "V24", "Log_Y_Index")
setnames(NNdata, "V25", "Orientation_Index")
setnames(NNdata, "V26", "Luminosity_Index")
setnames(NNdata, "V27", "SigmoidOfAreas")
setnames(NNdata, "V28", "Pastry")
setnames(NNdata, "V29", "Z_Scratch")
setnames(NNdata, "V30", "K_Scatch")
setnames(NNdata, "V31", "Stains")
setnames(NNdata, "V32", "Dirtiness")
setnames(NNdata, "V33", "Bumps")
setnames(NNdata, "V34", "Other_Faults")
str(NNdata)

#dropping the categorical variables
NNdata1<- NNdata[, -12]
NNdata<- NNdata1[, -12]

NNindep_data <- NNdata[, 1:26]
NNindep_data_sec <- NNdata[, 1:32]
NNindep_data_sec <- NNindep_data_sec[,-(26:31)]
str(NNindep_data_sec)

#obtaining maximum and minimum of every column
max_nn = apply(NNindep_data_sec, 2, max)
min_nn = apply(NNindep_data_sec, 2, min)

#scaling the dataset
scaled_nn = as.data.frame(scale(NNindep_data_sec, center=min_nn, scale=max_nn-min_nn))

#sampling the records and splitting into training and testing


sampledData = sample(1:nrow(NNindep_data_sec), round(0.80*nrow(NNindep_data_sec)))
training_nn = scaled_nn[sampledData,]
testing_nn = scaled_nn[-sampledData,]

names_nn = names(scaled_nn)
names_nn

formula = as.formula(paste("Other_Faults ~", paste(names_nn[!names_nn %in% "Other_Faults"], collapse = " + ")))

#hidden layer = 2 ; nodes =3 in each layer
nn_pastry = neuralnet(formula,data=training_nn,hidden=c(4,2),linear.output=T)
plot(nn_pastry)

Other_Faults = neuralnet(formula,data=training_nn,hidden=c(4,4),linear.output=T)
plot(Other_Faults)

testingdata_nn = compute(nn_pastry,testing_nn[,1:25])
testingdata_nn = compute(Other_Faults,testing_nn[,1:25])

y = testingdata_nn$net.result*(max(NNindep_data_sec$Bumps)-min(NNindep_data_sec$Bumps))+min(NNindep_data_sec$Bumps)
b = (testing_nn$Bumps)*(max(NNindep_data_sec$Bumps)-min(NNindep_data_sec$Bumps))+min(NNindep_data_sec$Bumps)

MSE = sum((b - y)^2)/nrow(testing_nn)
print(MSE) #0.06113305205
