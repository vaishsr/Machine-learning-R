Hearings <- X090117_Claims_Evidence_SPSS_Important_Hearings
names(Hearings)
Hearings1 <- Hearings[,]

names(Hearings1) <- gsub(" ","_",names(Hearings1))



#Aggregating the Relevance variable

Hearings1 <- transform(Hearings1 ,Relevance =  ifelse( General_Obesity+Childhood_Obesity == 2 , '3',  
                                                           
                                               ifelse( Childhood_Obesity == 1 , '1' ,
                                               ifelse( General_Obesity == 1 , '2', 
                                                                           
                                               ifelse( Childhood_Obesity == 0 & 
                                                       General_Obesity == 0 ,'0', 
                                                      'others')) ) ))

#Convert into the categorical data
Hearings1$Relevance <- as.factor(Hearings1$Relevance)
#View(Policydata1)
which.max(Hearings1$Relevance)
Hearings1$Relevance[1]
sum(is.na(Hearings1$Relevance))
#Frequency cross checking
sum(Hearings1$Relevance == '2')
names(Hearings1)
#remove the Relevance column
Hearings1 <- Hearings1[ ,-c(14:15)]

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Claim variable

Hearings1 <- transform(Hearings1 ,Claim =  ifelse( Claim_about_magnitude_of_problem+Claim_about_causes_of_problem+
                                                   Claim_about_health_consequences_of_problem+
                                                   Claim_about_economic.social_burden_of_problem+
                                                   Claim_about_possible_effective_solution_to_problem+
                                                   Claim_about_ineffective_solution_to_problem+
                                                   Claim_about_who_needs_to_act+Claim_._Other > 1 , '99',  
                                                     
                                           ifelse( Claim_about_magnitude_of_problem == 1 , '1' ,
                                           ifelse( Claim_about_causes_of_problem == 1 , '2',
                                           ifelse( Claim_about_health_consequences_of_problem == 1 , '3' ,
                                           ifelse( Claim_about_economic.social_burden_of_problem == 1 , '4',
                                           ifelse( Claim_about_possible_effective_solution_to_problem == 1 , '5' ,
                                           ifelse( Claim_about_ineffective_solution_to_problem == 1 , '6',
                                           ifelse( Claim_about_who_needs_to_act == 1 , '7',
                                           ifelse( Claim_._Other== 1, '8', 
                                                                                                           
                                           ifelse( Claim_about_magnitude_of_problem == 0 & 
                                                   Claim_about_causes_of_problem == 0 &
                                                   Claim_about_health_consequences_of_problem == 0 &
                                                   Claim_about_economic.social_burden_of_problem == 0 &
                                                   Claim_about_possible_effective_solution_to_problem == 0 &
                                                   Claim_about_ineffective_solution_to_problem == 0 &
                                                   Claim_about_who_needs_to_act == 0 &
                                                   Claim_._Other == 0, '0',
                                                   'others')) ) ))))))))


#Convert into the categorical data
Hearings1$Claim <- as.factor(Hearings1$Claim)
names(Hearings1)
which.max(Hearings1$Claim)
Hearings1$Claim[6]
sum(is.na(Hearings1$Claim))
#Frequency cross checking
sum(Hearings1$Claim == '8')
names(Hearings1)
#remove the Claim column
Hearings1 <- Hearings1[ ,-c(15:22)]

#-----------------------------------------------------------------------------------------------------------------------------------
# Aggregating the Source

Hearings1 <- transform(Hearings1, Source = ifelse( Generic_.no_specific_source_identified._e.g.._.studies_show..+
                                                   University_or_academic_research+
                                                   Congressional_research_.e.g.._GAO._OMB.+
                                                   Non.partisan_government_research_.e.g.._CDC._IOM.+
                                                   Partisan_government_research_.e.g.._White_House._Surgeon_General.+
                                                   State.level_agency_research+
                                                   International_agency_research_.e.g.._WHO.+
                                                   Foundations_or_non.profit_.e.g.._think_tank._research_.e.g.._RWJF._Rudd_Center.+
                                                   Advocacy_or_lobbying_group_research+
                                                   Industry.sponsored_research+
                                                   News_source+Expert_testimonial+
                                                   Anecdotal_evidence+Source_._Other > 1 ,'99',
                                             
                                           ifelse( Generic_.no_specific_source_identified._e.g.._.studies_show.. == 1 , '1' ,
                                           ifelse( University_or_academic_research == 1 , '2' , 
                                           ifelse( Congressional_research_.e.g.._GAO._OMB. == 1 , '3' , 
                                           ifelse( Non.partisan_government_research_.e.g.._CDC._IOM. == 1 , '4', 
                                           ifelse( Partisan_government_research_.e.g.._White_House._Surgeon_General. == 1 , '5' , 
                                           ifelse( State.level_agency_research == 1 , '6' , 
                                           ifelse( International_agency_research_.e.g.._WHO. == 1 , '7' ,
                                           ifelse( Foundations_or_non.profit_.e.g.._think_tank._research_.e.g.._RWJF._Rudd_Center. == 1 , '8' ,
                                           ifelse( Advocacy_or_lobbying_group_research == 1 , '9' ,
                                           ifelse( Industry.sponsored_research == 1 , '10' ,
                                           ifelse( News_source == 1, '11',
                                           ifelse( Expert_testimonial == 1, '12',
                                           ifelse( Anecdotal_evidence == 1, '13',
                                           ifelse( Source_._Other == 1,'14 - Others',
                                                           
                                           ifelse( Generic_.no_specific_source_identified._e.g.._.studies_show.. == 0 & 
                                                   University_or_academic_research == 0 & 
                                                   Congressional_research_.e.g.._GAO._OMB. == 0 & 
                                                   Non.partisan_government_research_.e.g.._CDC._IOM. == 0 & 
                                                   Partisan_government_research_.e.g.._White_House._Surgeon_General. == 0 & 
                                                   State.level_agency_research == 0 & 
                                                   International_agency_research_.e.g.._WHO. == 0 & 
                                                   Foundations_or_non.profit_.e.g.._think_tank._research_.e.g.._RWJF._Rudd_Center. == 0 & 
                                                   Advocacy_or_lobbying_group_research == 0 & 
                                                   Industry.sponsored_research == 0 & 
                                                   News_source == 0 & 
                                                   Expert_testimonial == 0 & 
                                                   Anecdotal_evidence == 0 & 
                                                   Source_._Other == 0, '0',
                                                   'Others')))))))))))))))))

View(Hearings1)
#Convert into the categorical data
Hearings1$Source <- as.factor(Hearings1$Source)
which.max(Hearings1$Source)
#Frequency cross checking
sum(Hearings1$Source == '14 - Others')

#removing the Source variables
names(Hearings1)
Hearings2 <- Hearings1[ ,-c(16:26)]
names(Hearings2)
Hearings1 <- Hearings2[ ,-c(17:19)]
names(Hearings1)

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Evidence type variable

Hearings1 <- transform(Hearings1 ,Type_of_Evidence =ifelse( Statistical_fact+Research_study+
                                                            Research_synthesis_.e.g..._meta.analysis_or_systematic_review.+
                                                            Policy_analysis_.e.g.._cost.effectiveness_study.+
                                                            Scientific_public_opinion_poll_.e.g.._Roper._MBC_News.+
                                                            Expert_opinion_or_testimony+ 
                                                            Public._stakeholder._or_constituent_opinion+
                                                            News_story+
                                                            Narrative_or_anecdote_about_an_identifiable_individual_.e.g.._self._family_member._constituent.+
                                                            Narrative_or_anecdote_about_an_identifiable_community_.group_of_people_with_a_similar_characteristic.+
                                                            Narrative_or_anecdote_about_an_identifiable_organization_or_institution_.e.g.._schools._industry.+
                                                            Type_of_Evidence_._Other > 1 ,'99',
                                  
                                                    ifelse( Statistical_fact == 1 , '1' ,
                                                    ifelse( Research_study == 1 , '2' , 
                                                    ifelse( Research_synthesis_.e.g..._meta.analysis_or_systematic_review. == 1 , '3' , 
                                                    ifelse( Policy_analysis_.e.g.._cost.effectiveness_study. == 1 , '4', 
                                                    ifelse( Scientific_public_opinion_poll_.e.g.._Roper._MBC_News. == 1 , '5' , 
                                                    ifelse( Expert_opinion_or_testimony == 1, '6',
                                                    ifelse( Public._stakeholder._or_constituent_opinion == 1, '7',
                                                    ifelse( News_story == 1,'8',
                                                    ifelse( Narrative_or_anecdote_about_an_identifiable_individual_.e.g.._self._family_member._constituent. == 1, '9',
                                                    ifelse( Narrative_or_anecdote_about_an_identifiable_community_.group_of_people_with_a_similar_characteristic. == 1, '10',
                                                    ifelse( Narrative_or_anecdote_about_an_identifiable_organization_or_institution_.e.g.._schools._industry. == 1, '11',
                                                    ifelse( Type_of_Evidence_._Other == 1, '12',
                                                                              
                                                    ifelse( Statistical_fact == 0 & 
                                                            Research_study == 0 & 
                                                            Research_synthesis_.e.g..._meta.analysis_or_systematic_review. == 0 & 
                                                            Policy_analysis_.e.g.._cost.effectiveness_study. == 0 & 
                                                            Scientific_public_opinion_poll_.e.g.._Roper._MBC_News. == 0 &
                                                            Expert_opinion_or_testimony == 0 &
                                                            Public._stakeholder._or_constituent_opinion == 0 &
                                                            News_story == 0 &
                                                            Narrative_or_anecdote_about_an_identifiable_individual_.e.g.._self._family_member._constituent. == 0 &
                                                            Narrative_or_anecdote_about_an_identifiable_community_.group_of_people_with_a_similar_characteristic. == 0 &
                                                            Narrative_or_anecdote_about_an_identifiable_organization_or_institution_.e.g.._schools._industry. == 0 &
                                                            Type_of_Evidence_._Other == 0,'0', 
                                                            'Others')) ) ))))))))))))

View(Hearings1)
#Convert into the categorical data
Hearings1$Type_of_Evidence <- as.factor(Hearings1$Type_of_Evidence)
which.max(Hearings1$Type_of_Evidence)
Hearings1$Type_of_Evidence[22]
#Frequency cross checking
sum(Hearings1$Type_of_Evidence == '11')
Hearings1[Hearings1$Type_of_Evidence == '11',]

# remove the Evidence types columns
names(Hearings1)
Hearings1 <- Hearings1[ ,-c(18:29)]

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Format variable

Hearings1 <- transform(Hearings1 ,Format = ifelse( Narrative_.text.+Orally+
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
Hearings1$Format <- as.factor(Hearings1$Format)
names(Hearings1)
which.max(Hearings1$Format)
Hearings1$Format[1896]
sum(is.na(Hearings1$Format))
#Frequency cross checking
sum(Hearings1$Format == '99')
names(Hearings1)
#remove the Format column
Hearings1 <- Hearings1[ ,-c(19:23)]
Hearings1[Hearings1$Format == '99',]

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Valence variable

Hearings1 <- transform(Hearings1 ,Valence =  ifelse( Support+Opposition+Neutral > 1 , '99',  
                                                 
                                             ifelse( Support == 1 , '1' ,
                                             ifelse( Opposition == 1 , '2', 
                                             ifelse( Neutral == 1 , '3' , 
                                                           
                                             ifelse( Support == 0 & 
                                                     Opposition == 0 & 
                                                     Neutral == 0, '0', 
                                                     'others')) ) )))

#Convert into the categorical data
Hearings1$Valence <- as.factor(Hearings1$Valence)
which.max(Hearings1$Valence)
Hearings1$Valence[1175]
sum(is.na(Hearings1$Valence))
#Frequency cross checking
sum(Hearings1$Valence == '99')
names(Hearings1)
#remove the Valence column
Hearings1 <- Hearings1[ ,-c(20:22)]

#--------------------------------------------------------------------------------------------------------------------------------

# Aggregating the Interpretation
Hearings1 <- transform(Hearings1 ,Interpretation = ifelse( Evidence_suggests_an_objective_status_of__problem_.e.g.._magnitude._burden.+
                                                           Evidence_suggests_a_policy_problem_.e.g.._lack_of_resources_or_attention.+
                                                           Evidence_suggests_cause.s._of_the_problem+
                                                           Evidence_suggests_responsibility_for_the_problem_.e.g.._personal_vs._social_or_corporate_responsibility.+
                                                           Evidence_suggests_a_probable_solution_to_the_program+
                                                           Evidence_suggests_a_particular_policy_response_to_the_problem+
                                                           Evidence_suggests_evaluation_of_policy_response_to_the_problem_.e.g.._as_part_of_oversight.+
                                                           Interpretation_._Other> 1 ,'99',
                                                               
                                                   ifelse( Evidence_suggests_an_objective_status_of__problem_.e.g.._magnitude._burden. == 1 , '1' ,
                                                   ifelse( Evidence_suggests_a_policy_problem_.e.g.._lack_of_resources_or_attention. == 1 , '2' , 
                                                   ifelse( Evidence_suggests_cause.s._of_the_problem == 1 , '3' , 
                                                   ifelse( Evidence_suggests_responsibility_for_the_problem_.e.g.._personal_vs._social_or_corporate_responsibility. == 1,'4',
                                                   ifelse( Evidence_suggests_a_probable_solution_to_the_program == 1 , '5', 
                                                   ifelse( Evidence_suggests_a_particular_policy_response_to_the_problem == 1 , '6' , 
                                                   ifelse( Evidence_suggests_evaluation_of_policy_response_to_the_problem_.e.g.._as_part_of_oversight. == 1 , '7' , 
                                                   ifelse( Interpretation_._Other == 1, '8',
                                 
                                                   ifelse( Evidence_suggests_an_objective_status_of__problem_.e.g.._magnitude._burden. == 0 & 
                                                           Evidence_suggests_a_policy_problem_.e.g.._lack_of_resources_or_attention. == 0 & 
                                                           Evidence_suggests_cause.s._of_the_problem == 0 & 
                                                           Evidence_suggests_responsibility_for_the_problem_.e.g.._personal_vs._social_or_corporate_responsibility. == 0 &
                                                           Evidence_suggests_a_probable_solution_to_the_program == 0 & 
                                                           Evidence_suggests_a_particular_policy_response_to_the_problem == 0 & 
                                                           Evidence_suggests_evaluation_of_policy_response_to_the_problem_.e.g.._as_part_of_oversight. == 0 &
                                                           Interpretation_._Other == 0,'0', 
                                                          'Others')) ) ))))))))
 
#View(Hearings1)
#Convert into the categorical data
Hearings1$Interpretation <- as.factor(Hearings1$Interpretation)
which.max(Hearings1$Interpretation)
#Frequency cross checking
sum(Hearings1$Interpretation == '3')
Hearings1[Hearings1$Interpretation == '3',]

#removing the intrepreatation variables
names(Hearings1)
Hearings1 <- Hearings1[ ,-c(27:34)]

#--------------------------------------------------------------------------------------------------------------------------------
#Aggregating the Evidence Use variable

Hearings1 <- transform(Hearings1 ,Use_of_Evidence =ifelse( Conceptual+
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


View(Hearings1)
#Convert into the categorical data
Hearings1$Use_of_Evidence <- as.factor(Hearings1$Use_of_Evidence)
which.max(Hearings1$Use_of_Evidence)
Hearings1$Use_of_Evidence[459]
#Frequency cross checking
sum(Hearings1$Use_of_Evidence == '5')
Hearings1[Hearings1$Use_of_Evidence == '5',]

# remove the Evidence Use columns
names(Hearings1)
Hearings1 <- Hearings1[ ,-c(21:25)]


# Exporting the Hearings1 dataframe to the excel  
library(xlsx)
write.xlsx(Hearings1, file = 'D:/Vaishnavi_Acads/MIT/Spring 2018/Prof Itzhak/Data Cleaning/Policy Data/Policy Documents Data/Evidence and Claims - CleanedHearings.xlsx', sheetName="Sheet1", col.names = TRUE)

