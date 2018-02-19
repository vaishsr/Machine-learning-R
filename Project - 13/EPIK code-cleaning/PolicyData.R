Policydata <- Evidence_and_Claims_Bills
summary(Policydata)
names(Policydata)

# removed the blank columns
Policydata1 <- Policydata[ ,-c(49:55)]
names(Policydata1)

#Creating the column for year
library(lubridate)
Policydata1$DocumentDate <- mdy(Policydata1$DocumentDate)
class(Policydata1$DocumentDate)
Policydata1$Year <- year(Policydata1$DocumentDate)
View(Policydata1)

#Extract COngress number and make it as new column
Policydata1$CongressNumber <- as.numeric(substr(Policydata1$MediaTitle, start = 7, stop = 9))
summary(Policydata1)

#Merging the Valence variable
Policydata1$Valence <- NA
Policydata1 <- transform(Policydata1 ,Valence =  ifelse( Valence_Support+Valence_Opposition+Valence_Neutral > 1 , '99',  
                                                         
                                                 ifelse( Valence_Support == 1 , '1' ,
                                                 ifelse( Valence_Opposition == 1 , '2', 
                                                 ifelse( Valence_Neutral == 1 , '3' , 
                                                         
                                                 ifelse( Valence_Support == 0 & 
                                                         Valence_Opposition == 0 & 
                                                         Valence_Neutral == 0, '0', 
                                                        'others')) ) )))
#Convert into the categorical data
Policydata1$Valence <- as.factor(Policydata1$Valence)
#View(Policydata1)
which.max(Policydata1$Valence)
Policydata1$Valence[250]
sum(is.na(Policydata1$Valence))
#Frequency cross checking
sum(Policydata1$Valence == '1')
names(Policydata1)
#remove the Valence column
Policydata1 <- Policydata1[ ,-c(40:42)]

#--------------------------------------------------------------------------------------------------------------------------------

# Aggregating the Interpretation
Policydata1$Interpretation <- NA
Policydata1 <- transform(Policydata1 ,Interpretation = ifelse( Interpt_ObjectiveStatus+Interpt_PolicyProblem+Interpt_Cause_of_Problem+
                                                               Interpt_ProbableSolution+Interpt_PolicyResponse+Interpt_PolicyEvaluation > 1 ,'99',
                                                               
                                                       ifelse( Interpt_ObjectiveStatus == 1 , '1' ,
                                                       ifelse( Interpt_PolicyProblem == 1 , '2' , 
                                                       ifelse( Interpt_Cause_of_Problem == 1 , '3' , 
                                                       ifelse( Interpt_ProbableSolution == 1 , '5', 
                                                       ifelse( Interpt_PolicyResponse == 1 , '6' , 
                                                       ifelse( Interpt_PolicyEvaluation == 1 , '7' , 
                                                                                                               
                                                       ifelse( Interpt_ObjectiveStatus == 0 & 
                                                               Interpt_PolicyProblem == 0 & 
                                                               Interpt_Cause_of_Problem == 0 & 
                                                               Interpt_ProbableSolution == 0 & 
                                                               Interpt_PolicyResponse == 0 & 
                                                               Interpt_PolicyEvaluation == 0, '0', 
                                                             'Others')) ) ))))))

#View(Policydata1)
#Convert into the categorical data
Policydata1$Interpretation <- as.factor(Policydata1$Interpretation)
which.max(Policydata1$Interpretation)
#Frequency cross checking
sum(Policydata1$Interpretation == '3')
Policydata1[Policydata1$Interpretation == '3',]

#removing the intrepreatation variables
names(Policydata1)
Policydata1 <- Policydata1[ ,-c(40:45)]

#-----------------------------------------------------------------------------------------------------------------------------------
# Aggregating the Source
Policydata1$Source <- NA

Policydata1 <- transform(Policydata1, Source = ifelse( Source_Generic+Source_Academic+Source_CongressionalResearch+
                                                       Source_NonpartisanResearch+Source_PartisanResearch+
                                                       Source_StateResearch+Source_InternationalResearch+
                                                       Source_FoundationResearch+Source_AdvocacyResearch+
                                                       Source_IndustryResearch+Source_News+Source_Testimonial+
                                                       Source_Anecdotal+Source_FederalAgency > 1 ,'99',
                                                       
                                               ifelse( Source_Generic == 1 , '1' ,
                                               ifelse( Source_Academic == 1 , '2' , 
                                               ifelse( Source_CongressionalResearch == 1 , '3' , 
                                               ifelse( Source_NonpartisanResearch == 1 , '4', 
                                               ifelse( Source_PartisanResearch == 1 , '5' , 
                                               ifelse( Source_StateResearch == 1 , '6' , 
                                               ifelse( Source_InternationalResearch == 1 , '7' ,
                                               ifelse( Source_FoundationResearch == 1 , '8' ,
                                               ifelse( Source_AdvocacyResearch == 1 , '9' ,
                                               ifelse( Source_IndustryResearch == 1 , '10' ,
                                               ifelse( Source_News == 1, '11',
                                               ifelse( Source_Testimonial == 1, '12',
                                               ifelse( Source_Anecdotal == 1, '13',
                                               ifelse( Source_FederalAgency == 1,'14 - Others(Source_FederalAgency)',
                                                      
                                               ifelse( Source_Generic == 0 & 
                                                       Source_Academic == 0 & 
                                                       Source_CongressionalResearch == 0 & 
                                                       Source_NonpartisanResearch == 0 & 
                                                       Source_PartisanResearch == 0 & 
                                                       Source_StateResearch == 0 & 
                                                       Source_InternationalResearch == 0 & 
                                                       Source_FoundationResearch == 0 & 
                                                       Source_AdvocacyResearch == 0 & 
                                                       Source_IndustryResearch == 0 & 
                                                       Source_News == 0 & 
                                                       Source_Testimonial == 0 & 
                                                       Source_Anecdotal == 0 & 
                                                       Source_FederalAgency == 0, '0',
                                                       'Others')))))))))))))))))

#View(Policydata1)
#Convert into the categorical data
Policydata1$Source <- as.factor(Policydata1$Source)
which.max(Policydata1$Source)
#Frequency cross checking
sum(Policydata1$Source == '5')

#removing the Source variables
names(Policydata1)
Policydata2 <- Policydata1[ ,-c(21:23)]
names(Policydata2)
Policydata3 <- Policydata2[ ,-24]
names(Policydata3)
Policydata4 <- Policydata3[ ,-25]
names(Policydata4)
Policydata1 <- Policydata4[ ,-c(26:34)]
names(Policydata1)

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Evidence Use variable

Policydata1$UseofEvidence <- NA
Policydata1 <- transform(Policydata1 ,UseofEvidence =ifelse( EvidenceUse_Conceptual+EvidenceUse_Instrumental+
                                                             EvidenceUse_Political+EvidenceUse_Tactical+
                                                             EvidenceUse_Symbolic > 1 ,'99',
                           
                                                     ifelse( EvidenceUse_Conceptual == 1 , '1' ,
                                                     ifelse( EvidenceUse_Instrumental == 1 , '2' , 
                                                     ifelse( EvidenceUse_Political == 1 , '3' , 
                                                     ifelse( EvidenceUse_Tactical == 1 , '4', 
                                                     ifelse( EvidenceUse_Symbolic == 1 , '5' , 
                                                                                                                                   
                                                     ifelse( EvidenceUse_Conceptual == 0 & 
                                                             EvidenceUse_Instrumental == 0 & 
                                                             EvidenceUse_Political == 0 & 
                                                             EvidenceUse_Tactical == 0 & 
                                                             EvidenceUse_Symbolic == 0 ,'0', 
                                                             'Others')) ) )))))
#View(Policydata1)
#Convert into the categorical data
Policydata1$UseofEvidence <- as.factor(Policydata1$UseofEvidence)
which.max(Policydata1$UseofEvidence)
Policydata1$UseofEvidence[813]
#Frequency cross checking
sum(Policydata1$UseofEvidence == '5')
Policydata1[Policydata1$UseofEvidence == '5',]

# remove the Evidence Use columns
names(Policydata1)
Policydata1 <- Policydata1[ ,-c(26:30)]

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Evidence type variable

Policydata1$TypeofEvidence <- NA
Policydata1 <- transform(Policydata1 ,TypeofEvidence =ifelse( Type_StatisticalFact+Type_ResearchStudy+
                                                              type_ResearchSynthesis+Type_PolicyAnalysis+
                                                              Type_Poll > 1 ,'99',
                                                             
                                                      ifelse( Type_StatisticalFact == 1 , '1' ,
                                                      ifelse( Type_ResearchStudy == 1 , '2' , 
                                                      ifelse( type_ResearchSynthesis == 1 , '3' , 
                                                      ifelse( Type_PolicyAnalysis == 1 , '4', 
                                                      ifelse( Type_Poll == 1 , '5' , 
                                                                       
                                                      ifelse( Type_StatisticalFact == 0 & 
                                                              Type_ResearchStudy == 0 & 
                                                              type_ResearchSynthesis == 0 & 
                                                              Type_PolicyAnalysis == 0 & 
                                                              Type_Poll == 0 ,'0', 
                                                              'Others')) ) )))))

#View(Policydata1)
#Convert into the categorical data
Policydata1$TypeofEvidence <- as.factor(Policydata1$TypeofEvidence)
which.max(Policydata1$TypeofEvidence)
Policydata1$TypeofEvidence[248]
#Frequency cross checking
sum(Policydata1$TypeofEvidence == '5')
Policydata1[Policydata1$TypeofEvidence == '99',]

# remove the Evidence types columns
names(Policydata1)
Policydata1 <- Policydata1[ ,-c(21:25)]

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Relevance variable

Policydata1$Relevance <- NA
Policydata1 <- transform(Policydata1 ,Relevance =  ifelse( Focus_GeneralObesity+Focus_ChildhoodObesity == 2 , '3',  
                                                         
                                                   ifelse( Focus_ChildhoodObesity == 1 , '1' ,
                                                   ifelse( Focus_GeneralObesity == 1 , '2', 
                                                                                 
                                                   ifelse( Focus_ChildhoodObesity == 0 & 
                                                           Focus_GeneralObesity == 0 ,'0', 
                                                           'others')) ) ))

#Convert into the categorical data
Policydata1$Relevance <- as.factor(Policydata1$Relevance)
#View(Policydata1)
which.max(Policydata1$Relevance)
Policydata1$Relevance[7]
sum(is.na(Policydata1$Relevance))
#Frequency cross checking
sum(Policydata1$Relevance == '2')
names(Policydata1)
#remove the Relevance column
Policydata1 <- Policydata1[ ,-c(7:8)]


#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Claim variable

Policydata1$Claim <- NA
Policydata1 <- transform(Policydata1 ,Claim =  ifelse( Magnitude+Causes+Health_Cons+Burden+
                                                       EffectiveSolutions+IneffectiveSolutions+
                                                       WhoNeedsToAct > 1 , '99',  
                                                           
                                               ifelse( Magnitude == 1 , '1' ,
                                               ifelse( Causes == 1 , '2',
                                               ifelse( Health_Cons == 1 , '3' ,
                                               ifelse( Burden == 1 , '4',
                                               ifelse( EffectiveSolutions == 1 , '5' ,
                                               ifelse( IneffectiveSolutions == 1 , '6',
                                               ifelse( WhoNeedsToAct == 1 , '7',
                                                                      
                                               ifelse( Magnitude == 0 & 
                                                       Causes == 0 &
                                                       Health_Cons == 0 &
                                                       Burden == 0 &
                                                       EffectiveSolutions == 0 &
                                                       IneffectiveSolutions == 0 &
                                                       WhoNeedsToAct == 0,'0',
                                                       'others')) ) )))))))


#Convert into the categorical data
Policydata1$Claim <- as.factor(Policydata1$Claim)
#View(Policydata1)
which.max(Policydata1$Claim)
Policydata1$Claim[30]
sum(is.na(Policydata1$Claim))
#Frequency cross checking
sum(Policydata1$Claim == '6')
names(Policydata1)
#remove the Claim column
Policydata2 <- Policydata1[ ,-c(10:12)]
names(Policydata2)
Policydata1 <- Policydata2[ ,-c(11:14)]
names(Policydata1)

 
