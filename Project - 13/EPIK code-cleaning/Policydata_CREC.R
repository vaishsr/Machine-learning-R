FloorDebate <- X012318_Claims_Evidence_CREC
names(FloorDebate)
FloorDebate1 <- FloorDebate[,]

names(FloorDebate1) <- gsub(" ","_",names(FloorDebate1))

#Extract COngress number and make it as new column
#FloorDebate1$CongressNumber <- as.numeric(substr(FloorDebate1$Media_Title, start = 7, stop = 9))
#summary(FloorDebate1)


#Aggregating the Relevance variable

FloorDebate1 <- transform(FloorDebate1 ,Relevance =  ifelse( General_obesity+Childhood_obesity == 2 , '3',  
                                                       
                                               ifelse( Childhood_obesity == 1 , '1' ,
                                               ifelse( General_obesity == 1 , '2', 
                                                                       
                                               ifelse( Childhood_obesity == 0 & 
                                                       General_obesity == 0 ,'0', 
                                                       'others')) ) ))

#Convert into the categorical data
FloorDebate1$Relevance <- as.factor(FloorDebate1$Relevance)
#View(Policydata1)
which.max(FloorDebate1$Relevance)
FloorDebate1$Relevance[508]
sum(is.na(FloorDebate1$Relevance))
#Frequency cross checking
sum(FloorDebate1$Relevance == '2')
names(FloorDebate1)
#remove the Relevance column
FloorDebate1 <- FloorDebate1[ ,-c(11:12)]

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Claim variable

FloorDebate1 <- transform(FloorDebate1 ,Claim =  ifelse( Claim_about_magnitude_of_problem+Claim_about_causes_of_problem+
                                                         Claim_about_health_consequences_of_problem+
                                                         Claim_about_economic.social_burden_of_problem+
                                                         Claim_about_possible_effective_solution_to_problem+
                                                         Claim_about_ineffective_solution_to_problem+
                                                         Claim_about_who_needs_to_act > 1 , '99',  
                                                       
                                                 ifelse( Claim_about_magnitude_of_problem == 1 , '1' ,
                                                 ifelse( Claim_about_causes_of_problem == 1 , '2',
                                                 ifelse( Claim_about_health_consequences_of_problem == 1 , '3' ,
                                                 ifelse( Claim_about_economic.social_burden_of_problem == 1 , '4',
                                                 ifelse( Claim_about_possible_effective_solution_to_problem == 1 , '5' ,
                                                 ifelse( Claim_about_ineffective_solution_to_problem == 1 , '6',
                                                 ifelse( Claim_about_who_needs_to_act == 1 , '7',
                                                 
                                                 ifelse( Claim_about_magnitude_of_problem == 0 & 
                                                         Claim_about_causes_of_problem == 0 &
                                                         Claim_about_health_consequences_of_problem == 0 &
                                                         Claim_about_economic.social_burden_of_problem == 0 &
                                                         Claim_about_possible_effective_solution_to_problem == 0 &
                                                         Claim_about_ineffective_solution_to_problem == 0 &
                                                         Claim_about_who_needs_to_act == 0, '0',
                                                         'others')) ) )))))))



#Convert into the categorical data
FloorDebate1$Claim <- as.factor(FloorDebate1$Claim)
names(FloorDebate1)
which.max(FloorDebate1$Claim)
FloorDebate1$Claim[494]
sum(is.na(FloorDebate1$Claim))
#Frequency cross checking
sum(FloorDebate1$Claim == '1')
names(FloorDebate1)
#remove the Claim column
FloorDebate1 <- FloorDebate1[ ,-c(12:18)]
names(FloorDebate1)

#-----------------------------------------------------------------------------------------------------------------------------------
# Aggregating the Source_of_evidence

FloorDebate1 <- transform(FloorDebate1, Source_of_evidence = ifelse( Generic_.no_specific_source_identified__e.g.__.studies_show..+
                                                                     University_or_academic_research+
                                                                     Congressional_research_.e.g.__GAO__OMB.+
                                                                     Non.partisan_government_research_.e.g.__CDC__IOM.+
                                                                     Partisan_government_research_.e.g.__White_House__Surgeon_General.+
                                                                     State.level_agency_research+
                                                                     International_agency_research_.e.g.__WHO.+
                                                                     Foundations_or_non.profit_.e.g.__think_tank._research_.e.g.__RWJF__Rudd_Center.+
                                                                     Advocacy_or_lobbying_group_research+
                                                                     Industry.sponsored_research+
                                                                     News_source+Expert_testimonial+
                                                                     Anecdotal_evidence > 1 ,'99',
                                                                   
                                                             ifelse( Generic_.no_specific_source_identified__e.g.__.studies_show.. == 1 , '1' ,
                                                             ifelse( University_or_academic_research == 1 , '2' , 
                                                             ifelse( Congressional_research_.e.g.__GAO__OMB. == 1 , '3' , 
                                                             ifelse( Non.partisan_government_research_.e.g.__CDC__IOM. == 1 , '4', 
                                                             ifelse( Partisan_government_research_.e.g.__White_House__Surgeon_General. == 1 , '5' , 
                                                             ifelse( State.level_agency_research == 1 , '6' , 
                                                             ifelse( International_agency_research_.e.g.__WHO. == 1 , '7' ,
                                                             ifelse( Foundations_or_non.profit_.e.g.__think_tank._research_.e.g.__RWJF__Rudd_Center. == 1 , '8' ,
                                                             ifelse( Advocacy_or_lobbying_group_research == 1 , '9' ,
                                                             ifelse( Industry.sponsored_research == 1 , '10' ,
                                                             ifelse( News_source == 1, '11',
                                                             ifelse( Expert_testimonial == 1, '12',
                                                             ifelse( Anecdotal_evidence == 1, '13',
                                                                             
                                                             ifelse( Generic_.no_specific_source_identified__e.g.__.studies_show.. == 0 & 
                                                                       University_or_academic_research == 0 & 
                                                                       Congressional_research_.e.g.__GAO__OMB. == 0 & 
                                                                       Non.partisan_government_research_.e.g.__CDC__IOM. == 0 & 
                                                                       Partisan_government_research_.e.g.__White_House__Surgeon_General. == 0 & 
                                                                       State.level_agency_research == 0 & 
                                                                       International_agency_research_.e.g.__WHO. == 0 & 
                                                                       Foundations_or_non.profit_.e.g.__think_tank._research_.e.g.__RWJF__Rudd_Center. == 0 & 
                                                                       Advocacy_or_lobbying_group_research == 0 & 
                                                                       Industry.sponsored_research == 0 & 
                                                                       News_source == 0 & 
                                                                       Expert_testimonial == 0 & 
                                                                       Anecdotal_evidence == 0 , '0', 
                                                                     'Others'))))))))))))))))

#View(FloorDebate1)
#Convert into the categorical data
FloorDebate1$Source_of_evidence <- as.factor(FloorDebate1$Source_of_evidence)
which.max(FloorDebate1$Source_of_evidence)
#Frequency cross checking
sum(FloorDebate1$Source_of_evidence == '4')

#removing the Source variables
names(FloorDebate1)
FloorDebate1 <- FloorDebate1[ ,-c(13:25)]
names(FloorDebate1)

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Evidence type variable

FloorDebate1 <- transform(FloorDebate1 ,Type_of_evidence =ifelse( Statistical_fact+Research_study+
                                                                  Research_synthesis_.e.g..__meta.analysis_or_systematic_review.+
                                                                  Policy_analysis_.e.g.__cost.effectiveness_study.+
                                                                  Scientific_public_opinion_poll_.e.g.__Roper__MBC_News.+
                                                                  Expert_opinion_or_testimony+ 
                                                                  Public__stakeholder__or_constituent_opinion+
                                                                  News_story+
                                                                  Narrative_or_anecdote_about_an_identifiable_individual_.e.g.__self__family_member__constituent.+
                                                                  Narrative_or_anecdote_about_an_identifiable_community_.group_of_people_with_a_similar_characteristic.+
                                                                  Narrative_or_anecdote_about_an_identifiable_organization_or_institution_.e.g.__schools__industry. > 1 ,'99',
                                                                
                                                          ifelse( Statistical_fact == 1 , '1' ,
                                                          ifelse( Research_study == 1 , '2' , 
                                                          ifelse( Research_synthesis_.e.g..__meta.analysis_or_systematic_review. == 1 , '3' , 
                                                          ifelse( Policy_analysis_.e.g.__cost.effectiveness_study. == 1 , '4', 
                                                          ifelse( Scientific_public_opinion_poll_.e.g.__Roper__MBC_News. == 1 , '5' , 
                                                          ifelse( Expert_opinion_or_testimony == 1, '6',
                                                          ifelse( Public__stakeholder__or_constituent_opinion == 1, '7',
                                                          ifelse( News_story == 1,'8',
                                                          ifelse( Narrative_or_anecdote_about_an_identifiable_individual_.e.g.__self__family_member__constituent. == 1, '9',
                                                          ifelse( Narrative_or_anecdote_about_an_identifiable_community_.group_of_people_with_a_similar_characteristic. == 1, '10',
                                                          ifelse( Narrative_or_anecdote_about_an_identifiable_organization_or_institution_.e.g.__schools__industry. == 1, '11',
                                                        
                                                                                  
                                                          ifelse( Statistical_fact == 0 & 
                                                                  Research_study == 0 & 
                                                                  Research_synthesis_.e.g..__meta.analysis_or_systematic_review. == 0 & 
                                                                  Policy_analysis_.e.g.__cost.effectiveness_study. == 0 & 
                                                                  Scientific_public_opinion_poll_.e.g.__Roper__MBC_News. == 0 &
                                                                  Expert_opinion_or_testimony == 0 &
                                                                  Public__stakeholder__or_constituent_opinion == 0 &
                                                                  News_story == 0 &
                                                                  Narrative_or_anecdote_about_an_identifiable_individual_.e.g.__self__family_member__constituent. == 0 &
                                                                  Narrative_or_anecdote_about_an_identifiable_community_.group_of_people_with_a_similar_characteristic. == 0 &
                                                                  Narrative_or_anecdote_about_an_identifiable_organization_or_institution_.e.g.__schools__industry. == 0, '0',
                                                                  'Others')) ) )))))))))))

#View(FloorDebate1)
#Convert into the categorical data
FloorDebate1$Type_of_evidence <- as.factor(FloorDebate1$Type_of_evidence)
which.max(FloorDebate1$Type_of_evidence)
FloorDebate1$Type_of_evidence[1511]
#Frequency cross checking
sum(FloorDebate1$Type_of_evidence == '11')
FloorDebate1[FloorDebate1$Type_of_evidence == '11',]

# remove the Evidence types columns
names(FloorDebate1)
FloorDebate1 <- FloorDebate1[ ,-c(14:24)]
names(FloorDebate1)
#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Format variable

FloorDebate1 <- transform(FloorDebate1 ,Format_of_evidence_.How_is_evidence_presented.. = ifelse( Narrative_.text.+Orally+
                                                     Table+Figure+Video > 1 , '99',  
                                                   
                                           ifelse( Narrative_.text. == 1 , '1' ,
                                           ifelse( Orally == 1 , '2',
                                           ifelse( Table == 1 , '3' ,
                                           ifelse( Figure == 1 , '4',
                                           ifelse( Video == 1 , '5' ,
                                                                                   
                                           ifelse( Narrative_.text. == 0 & 
                                                   Orally == 0 &
                                                   Table == 0 &
                                                   Figure == 0 &
                                                   Video == 0 , '0',
                                                  'others')) ) )))))


#Convert into the categorical data
FloorDebate1$Format_of_evidence_.How_is_evidence_presented.. <- as.factor(FloorDebate1$Format_of_evidence_.How_is_evidence_presented..)
names(FloorDebate1)
which.max(FloorDebate1$Format_of_evidence_.How_is_evidence_presented..)
FloorDebate1$Format_of_evidence_.How_is_evidence_presented..[1526]
sum(is.na(FloorDebate1$Format_of_evidence_.How_is_evidence_presented..))
#Frequency cross checking
sum(FloorDebate1$Format_of_evidence_.How_is_evidence_presented.. == '99')
names(FloorDebate1)
#remove the Format column
FloorDebate1 <- FloorDebate1[ ,-c(15:19)]
FloorDebate1[FloorDebate1$Format_of_evidence_.How_is_evidence_presented.. == '99',]
names(FloorDebate1)


#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Valence variable

FloorDebate1 <- transform(FloorDebate1 ,Valence__.Is_evidence_presented_in_the_context_of_support__opposition__or_neutral_stance_toward_the_topic.policy_discussed..
                                                    =  ifelse( Support+Opposition+Neutral > 1 , '99',  
                                                     
                                                       ifelse( Support == 1 , '1' ,
                                                       ifelse( Opposition == 1 , '2', 
                                                       ifelse( Neutral == 1 , '3' , 
                                                                             
                                                       ifelse( Support == 0 & 
                                                               Opposition == 0 & 
                                                               Neutral == 0, '0', 
                                                              'others')) ) )))

#Convert into the categorical data
FloorDebate1$Valence__.Is_evidence_presented_in_the_context_of_support__opposition__or_neutral_stance_toward_the_topic.policy_discussed.. <- as.factor(FloorDebate1$Valence__.Is_evidence_presented_in_the_context_of_support__opposition__or_neutral_stance_toward_the_topic.policy_discussed..)
which.max(FloorDebate1$Valence__.Is_evidence_presented_in_the_context_of_support__opposition__or_neutral_stance_toward_the_topic.policy_discussed..)
FloorDebate1$Valence__.Is_evidence_presented_in_the_context_of_support__opposition__or_neutral_stance_toward_the_topic.policy_discussed..[1506]
sum(is.na(FloorDebate1$Valence__.Is_evidence_presented_in_the_context_of_support__opposition__or_neutral_stance_toward_the_topic.policy_discussed..))
#Frequency cross checking
sum(FloorDebate1$Valence__.Is_evidence_presented_in_the_context_of_support__opposition__or_neutral_stance_toward_the_topic.policy_discussed.. == '99')
names(FloorDebate1)
#remove the Valence column
FloorDebate1 <- FloorDebate1[ ,-c(16:18)]
names(FloorDebate1)

#--------------------------------------------------------------------------------------------------------------------------------

# Aggregating the Interpretation
FloorDebate1 <- transform(FloorDebate1 ,Interpretation_.What__if_any__claims_are_made_.explicitly_or_implicitly._regarding_the_significance_of_the_evidence_presented..
                                                   = ifelse( Evidence_suggests_an_objective_status_of__problem_.e.g.__magnitude__burden.+
                                                             Evidence_suggests_a_policy_problem_.e.g.__lack_of_resources_or_attention.+
                                                             Evidence_suggests_cause.s._of_the_problem+
                                                             Evidence_suggests_responsibility_for_the_problem_.e.g.__personal_vs._social_or_corporate_responsibility.+
                                                             Evidence_suggests_a_probable_solution_to_the_program+
                                                             Evidence_suggests_a_particular_policy_response_to_the_problem+
                                                             Evidence_suggests_evaluation_of_policy_response_to_the_problem_.e.g.__as_part_of_oversight. > 1 ,'99',
                                                             
                                                           
                                                     ifelse( Evidence_suggests_an_objective_status_of__problem_.e.g.__magnitude__burden. == 1 , '1' ,
                                                     ifelse( Evidence_suggests_a_policy_problem_.e.g.__lack_of_resources_or_attention. == 1 , '2' , 
                                                     ifelse( Evidence_suggests_cause.s._of_the_problem == 1 , '3' , 
                                                     ifelse( Evidence_suggests_responsibility_for_the_problem_.e.g.__personal_vs._social_or_corporate_responsibility. == 1,'4',
                                                     ifelse( Evidence_suggests_a_probable_solution_to_the_program == 1 , '5', 
                                                     ifelse( Evidence_suggests_a_particular_policy_response_to_the_problem == 1 , '6' , 
                                                     ifelse( Evidence_suggests_evaluation_of_policy_response_to_the_problem_.e.g.__as_part_of_oversight. == 1 , '7' , 
                                                                                                                     
                                                     ifelse( Evidence_suggests_an_objective_status_of__problem_.e.g.__magnitude__burden. == 0 & 
                                                             Evidence_suggests_a_policy_problem_.e.g.__lack_of_resources_or_attention. == 0 & 
                                                             Evidence_suggests_cause.s._of_the_problem == 0 & 
                                                             Evidence_suggests_responsibility_for_the_problem_.e.g.__personal_vs._social_or_corporate_responsibility. == 0 &
                                                             Evidence_suggests_a_probable_solution_to_the_program == 0 & 
                                                             Evidence_suggests_a_particular_policy_response_to_the_problem == 0 & 
                                                             Evidence_suggests_evaluation_of_policy_response_to_the_problem_.e.g.__as_part_of_oversight. == 0 ,'0',
                                                             'Others')) ) )))))))

#Convert into the categorical data
FloorDebate1$Interpretation_.What__if_any__claims_are_made_.explicitly_or_implicitly._regarding_the_significance_of_the_evidence_presented.. <- as.factor(FloorDebate1$Interpretation_.What__if_any__claims_are_made_.explicitly_or_implicitly._regarding_the_significance_of_the_evidence_presented..)
which.max(FloorDebate1$Interpretation_.What__if_any__claims_are_made_.explicitly_or_implicitly._regarding_the_significance_of_the_evidence_presented..)
#Frequency cross checking
sum(FloorDebate1$Interpretation_.What__if_any__claims_are_made_.explicitly_or_implicitly._regarding_the_significance_of_the_evidence_presented.. == '8')
FloorDebate1[FloorDebate1$Interpretation_.What__if_any__claims_are_made_.explicitly_or_implicitly._regarding_the_significance_of_the_evidence_presented.. == '1',]

#removing the intrepreatation variables
names(FloorDebate1)
FloorDebate1 <- FloorDebate1[ ,-c(17:23)]
names(FloorDebate1)


#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Evidence Use variable

FloorDebate1 <- transform(FloorDebate1 ,Use_of_evidence_.How_or_for_what_purpose_is_evidence_used..
                                            =ifelse( Conceptual+
                                                     Instrumental+
                                                     Political+
                                                     Tactical+
                                                     Symbolic > 1 ,'99',
                                                   
                                             ifelse( Conceptual == 1 , '1' ,
                                             ifelse( Instrumental == 1 , '2' , 
                                             ifelse( Political == 1 , '3' , 
                                             ifelse( Tactical == 1 , '4', 
                                             ifelse( Symbolic == 1 , '5' , 
                                       
                                             ifelse( Conceptual == 0 & 
                                                     Instrumental == 0 & 
                                                     Political == 0 & 
                                                     Tactical == 0 & 
                                                     Symbolic == 0 ,'0', 
                                                     'Others')) ) )))))


#View(FloorDebate1)
#Convert into the categorical data
FloorDebate1$Use_of_evidence_.How_or_for_what_purpose_is_evidence_used.. <- as.factor(FloorDebate1$Use_of_evidence_.How_or_for_what_purpose_is_evidence_used..)
which.max(FloorDebate1$Use_of_evidence_.How_or_for_what_purpose_is_evidence_used..)
FloorDebate1$Use_of_evidence_.How_or_for_what_purpose_is_evidence_used..[1510]
#Frequency cross checking
sum(FloorDebate1$Use_of_evidence_.How_or_for_what_purpose_is_evidence_used.. == '5')
FloorDebate1[FloorDebate1$Use_of_evidence_.How_or_for_what_purpose_is_evidence_used.. == '99',]

# remove the Evidence Use columns
names(FloorDebate1)
FloorDebate1 <- FloorDebate1[ ,-c(18:22)]
names(FloorDebate1)


# Exporting the Floor Debate dataframe to the excel  
library(xlsx)
write.xlsx(FloorDebate1, file = 'D:/Vaishnavi_Acads/MIT/Spring 2018/Prof Itzhak/Data Cleaning/Policy Data/Policy Documents Data/Evidence and Claims - CleanedFloorDebates.xlsx', sheetName="Sheet1", col.names = TRUE)


