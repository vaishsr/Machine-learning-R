#Importing three different document type files
BillsClean <- Evidence_and_Claims_CleanedBills
HearingsClean <- Evidence_and_Claims_CleanedHearings
FloorDebatesClean <- Evidence_and_Claims_CleanedFloorDebates

str(BillsClean)
str(HearingsClean)
str(FloorDebatesClean)

#changing the column names so all the 3 file column name matches
colnames(BillsClean)[3] <- "Document_title"
colnames(BillsClean)[5] <- "PolicyActor"
colnames(HearingsClean)[11] <- "PolicyActor"
colnames(HearingsClean)[12] <- "Policy_Setting"
colnames(HearingsClean)[14] <- "Claim"

colnames(FloorDebatesClean)[6] <- "PolicyActor"
colnames(FloorDebatesClean)[11] <- "Claim"
colnames(FloorDebatesClean)[14] <- "Format"
colnames(FloorDebatesClean)[15] <- "Valence"
colnames(HearingsClean)[10] <- "PrimaryPolicy"
colnames(HearingsClean)[6] <- "CongressNumber"


#Create a extra column for row binding the three datasets
FloorDebatesClean$Document_title <- NA
FloorDebatesClean$PrimaryPolicy <- NA
HearingsClean$DocumentDate <- NA
FloorDebatesClean$SecondaryPolicy <- NA
HearingsClean$SecondaryPolicy <- NA
FloorDebatesClean$SpecificSource <- NA
HearingsClean$SpecificSource <- NA
FloorDebatesClean$EvidenceMention <- NA
HearingsClean$EvidenceMention <- NA
FloorDebatesClean$CongressNumber <- NA
BillsClean$Unique_ID <- NA
BillsClean$Unique__DocID <- NA
BillsClean$Document_type <- NA
FloorDebatesClean$Document_type <- NA
BillsClean$Policy_Setting <- NA
HearingsClean <- HearingsClean[,-16]


#changing the date format
library(lubridate)
FloorDebatesClean$DocumentDate <- mdy(FloorDebatesClean$DocumentDate)

#stacking up all the dataset into one main file
MainPolicyData1 <- rbind(BillsClean,HearingsClean)
MainPolicyData <- rbind(MainPolicyData1,FloorDebatesClean)


str(MainPolicyData)

#make the respective variables into categorical variables
MainPolicyData$Claim <- as.factor(MainPolicyData$Claim)
MainPolicyData$Valence <- as.factor(MainPolicyData$Valence)
MainPolicyData$Interpretation <- as.factor(MainPolicyData$Interpretation)
MainPolicyData$Source_of_evidence <- as.factor(MainPolicyData$Source_of_evidence)
MainPolicyData$Use_of_Evidence <- as.factor(MainPolicyData$Use_of_Evidence)
MainPolicyData$Type_of_Evidence <- as.factor(MainPolicyData$Type_of_Evidence)
MainPolicyData$Relevance <- as.factor(MainPolicyData$Relevance)
MainPolicyData$Format <- as.factor(MainPolicyData$Format)



# Exporting the dataframe to the excel  
library(xlsx)
write.xlsx(MainPolicyData, file = 'D:/Vaishnavi_Acads/MIT/Spring 2018/Prof Itzhak/Data Cleaning/Policy Data/Policy Documents Data/MainPolicyData.xlsx', sheetName="Sheet1", col.names = TRUE)


summary(MainPolicyData)
