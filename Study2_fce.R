
setwd("D:/Projects/3_CurrentProjects/COVID/STM")
#install.packages(c("magrittr", "data.table", 'dplyr', 'ggplot2', 'cluster'))
sapply(c("MASS","DescTools", "magrittr", "data.table", 'plyr','dplyr', 'ggplot2', 'cluster', 'dendextend','pkgbuild','devtools','R.methodsS3','stm','ngram','matrixStats','corrplot','ggpubr','wordcloud','ggplot2'), require, character.only = T)

knitr::opts_chunk$set(fig.width=12, fig.height=12) 
data <- read.csv('D:/Projects/3_CurrentProjects/COVID/UABSES_195385_20200706.csv', header=T, sep=',')

data_copy<-data
data<-data[,c(-1,-3,-4,-5,-7,-8,-9,-10,-11)]#drop irrelevant columns

justloaddata<-1

STM<-1 #whether to run STM analysis or (temporarily) switch it off

colnames(data)<-c("Response.ID","Duration","Gender","Age","Age.cohort","Region","Education","Number.Inhabitants",
                  "PostalCode","BORNINPANEL","CBIRTH","CountryofBirth","NewSurveyParticipant",
                  "Concern_poverty","Concern_water","Concern_climatechange","Concern_terrorism","Concern_economics","Concern_warconflict",
                  "Concern_pupulation","Concern_nuclear","Concern_infections","Concern_unemployment","Concern_corruption","Concern_migration",
                  "Concern_discrimination","Concern_biodiversity","Concern_airpollution","Concern_other","Concern_dontknow","Concern_Other",
                  "COVID_government_close","COVID_government_open","Area_urban_rural", "COVID_people_close","COVID_people_open",
                  
                  "Donation_Inter_Covid","Donation_Inter_Climate","Donation_Spanish_Covid","Donation_Spanish_Climate","Donation_self",
                  "Expected_Donation_Inter_Covid","Expected_Donation_Inter_Climate","Expected_Donation_Spanish_Covid",
                  "Expected_Donation_Spanish_Climate","Expected_Donation_self","Expected_Donation_Dontknow",
                    
                  "Act_Confinement_recycling","Act_Confinement_energyreduction","Act_Confinement_talkaboutclimate",
                  "Act_Confinement_smallpack","Act_Confinement_greentransport","Act_Confinement_greenfood",
                  "Act_Confinement_manifest","Act_Confinement_plance",

                  "Act_ConfinementvsPast_recycling","Act_ConfinementvsPast_energyreduction","Act_ConfinementvsPast_talkaboutclimate",
                  "Act_ConfinementvsPast_smallpack","Act_ConfinementvsPast_greentransport","Act_ConfinementvsPast_greenfood",
                  "Act_ConfinementvsPast_manifest","Act_ConfinementvsPast_plance",

                  "Act_PastvsFuture_recycling","Act_PastvsFuture_energyreduction","Act_PastvsFuture_talkaboutclimate",
                  "Act_PastvsFuture_smallpack","Act_PastvsFuture_greentransport","Act_PastvsFuture_greenfood",
                  "Act_PastvsFuture_manifest","Act_PastvsFuture_plance",

                  "ClimateConcern","ClimateThreat","ClimateExperience",
                  
                  "Acceptance_CT","ExpectedAcceptance_CT","ExpectedRejection_CT","ExpectedNeitherNor_CT",
                  
                  "ExpectedAcceptance_CO2Limit","ExpectedAcceptance_GasolineStandrads","ExpectedAcceptance_SubsidiesTesla",
                  "ExpectedAcceptance_DieselBan","ExpectedAcceptance_TrainingCars",
                  
                  "Acceptance_CT_PoorHH","Acceptance_CT_Climate","Acceptance_CT_AllHH","Acceptance_CT_Covid",
                  "BudgetPercent_CT_PoorHH","BudgetPercent_CT_Climate","BudgetPercent_CT_AllHH","BudgetPercent_CT_Covid",
                  
                  "PerceivedThreat_Covid","HealthProblems_Covid","HealthProblems_Covid_FamFri","HealthProblems_Covid_FamFri_junk",
                  
                  "PaidJob_Covid_Confinement","WorkHome_Covid_Confinement","Productivity_Confinement","Stress_Confinement","HHIncome_Confinement",
                  "ClimateCausedCovid_statement","OverallExperience_Covid",
                  
                  "Evaluation_Government_Covid","Evaluation_Citizens_Covid",
                  
                  "GrowthvsEnv_Satisfaction","GrowthvsEnv_EnvProtection","GrowthvsEnv_Stability","GrowthvsEnv_PublucService","GrowthvsEnv_DevSpace",
                  
                  "EmploymentStatus","HHIncome_LastYear",
                  
                  "InfoExperiment","TYPE_OF_ROTATION","Acceptance_CT_repeat",
                  
                  "GrowthvsEnv_Position",
                  
                  "Values_Universalism","Values_Benevolence","Values_RespectEarth","Values_ProtectionEnvironment",
                  
                  "TrustInPoliticians","PoliticalView","NationalityOther")        
summary(data$NewSurveyParticipant)
sum(data$NewSurveyParticipant==1)/length(data$NewSurveyParticipant)

#Keep only 1028 observations who participated in Study 2 but not Study 1?
#data<-data[which(data$NewSurveyParticipant==2),]

#replacingNAs
data$Education[which(data$Education==99)]<-NA
data$Number.Inhabitants[which(data$Number.Inhabitants==99)]<-NA
data$CountryofBirth[which(data$CountryofBirth==99)]<-NA
data$HealthProblems_Covid[which(data$HealthProblems_Covid==98)]<-NA
data$HealthProblems_Covid_FamFri[which(data$HealthProblems_Covid_FamFri==98)]<-NA
data$HHIncome_LastYear[which(data$HHIncome_LastYear==98)]<-NA
data$PoliticalView[which(data$PoliticalView==99)]<-NA
data$NationalityOther[which(data$NationalityOther==99)]<-NA
summary(data$PoliticalView)
sd(data$PoliticalView,na.rm=TRUE)

#recoding variables to have meaningful scale
data$InfoExperiment[which(data$InfoExperiment==2)]<-0
data$PaidJob_Covid_Confinement[which(data$PaidJob_Covid_Confinement==4)]<-0
data$WorkHome_Covid_Confinement[which(data$WorkHome_Covid_Confinement==2)]<-0 #no
data$WorkHome_Covid_Confinement[which(data$WorkHome_Covid_Confinement==1)]<-2 #yes
data$WorkHome_Covid_Confinement[which(data$WorkHome_Covid_Confinement==3)]<-1 #partially


summary(data$Duration)
summary(data$Duration/60)


for (column in c(1:ncol(data))[-c(1,31,33,36)]){
  data[,column]<-as.numeric(data[,column]) #make sure all numbers are recognized as numbers
  #data[which(data[,column]==.),column]<-NA #make 8 (no opinion) NA
}
summary(data)

message("SDs of the descriptive variables")
sapply(data[,c(1:ncol(data))[-c(1,31,33,36)]], sd, na.rm = TRUE)


#"Acceptance_CT","ExpectedAcceptance_CT","ExpectedRejection_CT","ExpectedNeitherNor_CT",
mean(data$Acceptance_CT)
sum(data$Acceptance_CT>3)/length(data$Acceptance_CT)

par(mfrow = c(1, 2) ,mar=c(11,4,4,1))
hist(data$Acceptance_CT, main="How acceptable you find CT",breaks=c(0,1,2,3,4,5), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=c("completely unacceptable","somewhat unacceptable","neither nor","somewhat acceptable","completely acceptable"),las=2) 

hist(data$Acceptance_CT_repeat, main="How acceptable you find CT (follow up)",breaks=c(0,1,2,3,4,5), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=c("completely unacceptable","somewhat unacceptable","neither nor","somewhat acceptable","completely acceptable"),las=2) 


lm_exp<-lm(data$Acceptance_CT ~ data$ExpectedAcceptance_CT)
summary(lm_exp)

lm_exp<-lm(data$Acceptance_CT ~ data$ExpectedAcceptance_CT + data$Age +data$Gender + 
             data$Education + data$HHIncome_LastYear + data$PoliticalView)
summary(lm_exp)

data$Acceptance_CT<-as.factor(data$Acceptance_CT)
m_accept <- polr(Acceptance_CT ~  ExpectedAcceptance_CT
                 , data = data, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")



m_accept <- polr(Acceptance_CT ~  ExpectedAcceptance_CT +Age +Gender + 
                   Education + HHIncome_LastYear + PoliticalView
                 , data = data, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

data$Acceptance_CT<-as.numeric(data$Acceptance_CT)

  myData_aux2<-matrix(0,12,2)
  myData_aux2<-as.data.frame(myData_aux2)
  colnames(myData_aux2)<-c("Estimate", "Acceptance")
  myData_aux2[,1]<-c("1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Non-acceptance",
                    "1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Non-acceptance",
                    "1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Non-acceptance")
  myData_aux2[,2]<-c("4-5 \n Acceptance","4-5 \n Acceptance","4-5 \n Acceptance","4-5 \n Acceptance",
                    "3 \n Undecided","3 \n Undecided","3 \n Undecided","3 \n Undecided",
                    "1-2 Rejection","1-2 Rejection","1-2 Rejection","1-2 Rejection")
  myData_aux2$mean<-c(sum(data$Acceptance_CT>3)/length(data$Acceptance_CT)*100,
                     mean(data$ExpectedAcceptance_CT[data$Acceptance_CT>3]),
                     mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT>3]),
                     mean(data$ExpectedRejection_CT[data$Acceptance_CT>3]),
                     
                     sum(data$Acceptance_CT==3)/length(data$Acceptance_CT)*100,
                     mean(data$ExpectedAcceptance_CT[data$Acceptance_CT==3]),
                     mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT==3]),
                     mean(data$ExpectedRejection_CT[data$Acceptance_CT==3]),
                     
                     sum(data$Acceptance_CT<3)/length(data$Acceptance_CT)*100,
                     mean(data$ExpectedAcceptance_CT[data$Acceptance_CT<3]),
                     mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT<3]),
                     mean(data$ExpectedRejection_CT[data$Acceptance_CT<3]))
  
  myData_aux2$sd<-c(0,sd(data$ExpectedAcceptance_CT[data$Acceptance_CT>3]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT>3]),sd(data$ExpectedRejection_CT[data$Acceptance_CT>3]),
                   0,sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==3]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==3]),sd(data$ExpectedRejection_CT[data$Acceptance_CT==3]),
                   0,sd(data$ExpectedRejection_CT[data$Acceptance_CT<3]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT<3]),sd(data$ExpectedRejection_CT[data$Acceptance_CT<3]))
  myData_aux2$n<-c(1,sum(data$Acceptance_CT>3),sum(data$Acceptance_CT>3),sum(data$Acceptance_CT>3),
                   1,sum(data$Acceptance_CT==3),sum(data$Acceptance_CT==3),sum(data$Acceptance_CT==3),
                   1,sum(data$Acceptance_CT<3),sum(data$Acceptance_CT<3),sum(data$Acceptance_CT<3))
  #myData_aux <- do.call(data.frame, myData_aux)
  myData_aux2$se<-myData_aux2$sd / sqrt(myData_aux2$n)
  
  
  dodge <- position_dodge(width = 0.9)
  limits <- aes(ymax = myData_aux2$mean + 2*myData_aux2$se,
                ymin = myData_aux2$mean - 2*myData_aux2$se)
  p <- ggplot(data = myData_aux2, aes(x = Acceptance, y = mean, fill = factor(Estimate)))
  p + geom_bar(stat = "identity", position = dodge) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    labs(x = "", y = "%")+
    scale_fill_discrete(name = "", labels=c("1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Rejection"))+ theme(
      axis.text.x = element_blank())+
    ylim(0,100)+
    #scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
    #                            'Low-income+Climate','Universal+Climate'))+ 
    theme(axis.text.x = element_text(size = 12, angle = 50, hjust = 1))+
    theme(axis.text.y = element_text(size = 12),legend.text=element_text(size=12))
  
  
  
  
  
  
  myData_aux2<-matrix(0,20,2)
  myData_aux2<-as.data.frame(myData_aux2)
  colnames(myData_aux2)<-c("Estimate", "Acceptance")
  myData_aux2[,1]<-c("1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Rejection",
                     "1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Rejection",
                     "1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Rejection",
                     "1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Rejection",
                     "1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Rejection")
  myData_aux2[,2]<-c("5 Completely \n acceptable","5 Completely \n acceptable","5 Completely \n acceptable","5 Completely \n acceptable",
                     "4 Somewhat \n acceptable","4 Somewhat \n acceptable","4 Somewhat \n acceptable","4 Somewhat \n acceptable",
                     "3 Neither/nor","3 Neither/nor","3 Neither/nor","3 Neither/nor",
                     "2 Somewhat \n unacceptable","2 Somewhat \n unacceptable","2 Somewhat \n unacceptable","2 Somewhat \n unacceptable",
                     "1 Completely \n unacceptable","1 Completely \n unacceptable","1 Completely \n unacceptable","1 Completely \n unacceptable")
  myData_aux2$mean<-c(sum(data$Acceptance_CT==5)/length(data$Acceptance_CT)*100,
                      mean(data$ExpectedAcceptance_CT[data$Acceptance_CT==5]),
                      mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT==5]),
                      mean(data$ExpectedRejection_CT[data$Acceptance_CT==5]),
                      
                      sum(data$Acceptance_CT==4)/length(data$Acceptance_CT)*100,
                      mean(data$ExpectedAcceptance_CT[data$Acceptance_CT==4]),
                      mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT==4]),
                      mean(data$ExpectedRejection_CT[data$Acceptance_CT==4]),
                      
                      sum(data$Acceptance_CT==3)/length(data$Acceptance_CT)*100,
                      mean(data$ExpectedAcceptance_CT[data$Acceptance_CT==3]),
                      mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT==3]),
                      mean(data$ExpectedRejection_CT[data$Acceptance_CT==3]),
                      
                      sum(data$Acceptance_CT==2)/length(data$Acceptance_CT)*100,
                      mean(data$ExpectedAcceptance_CT[data$Acceptance_CT==2]),
                      mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT==2]),
                      mean(data$ExpectedRejection_CT[data$Acceptance_CT==2]),
                      
                      sum(data$Acceptance_CT==1)/length(data$Acceptance_CT)*100,
                      mean(data$ExpectedAcceptance_CT[data$Acceptance_CT==1]),
                      mean(data$ExpectedNeitherNor_CT[data$Acceptance_CT==1]),
                      mean(data$ExpectedRejection_CT[data$Acceptance_CT==1]))
  
  myData_aux2$sd<-c(0,sd(data$ExpectedAcceptance_CT[data$Acceptance_CT==5]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==5]),sd(data$ExpectedRejection_CT[data$Acceptance_CT==5]),
                    0,sd(data$ExpectedAcceptance_CT[data$Acceptance_CT==4]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==4]),sd(data$ExpectedRejection_CT[data$Acceptance_CT==4]),
                    0,sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==3]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==3]),sd(data$ExpectedRejection_CT[data$Acceptance_CT==3]),
                    0,sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==2]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==2]),sd(data$ExpectedRejection_CT[data$Acceptance_CT==2]),
                    0,sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==1]),sd(data$ExpectedNeitherNor_CT[data$Acceptance_CT==1]),sd(data$ExpectedRejection_CT[data$Acceptance_CT==1]))
  myData_aux2$n<-c(1,sum(data$Acceptance_CT==5),sum(data$Acceptance_CT==5),sum(data$Acceptance_CT==5),
                   1,sum(data$Acceptance_CT==4),sum(data$Acceptance_CT==4),sum(data$Acceptance_CT==4),
                   1,sum(data$Acceptance_CT==3),sum(data$Acceptance_CT==3),sum(data$Acceptance_CT==3),
                   1,sum(data$Acceptance_CT==2),sum(data$Acceptance_CT==2),sum(data$Acceptance_CT==2),
                   1,sum(data$Acceptance_CT==1),sum(data$Acceptance_CT==1),sum(data$Acceptance_CT==1))
  #myData_aux <- do.call(data.frame, myData_aux)
  myData_aux2$se<-myData_aux2$sd / sqrt(myData_aux2$n)
  
  dodge <- position_dodge(width = 0.9)
  limits <- aes(ymax = myData_aux2$mean + 2*myData_aux2$se,
                ymin = myData_aux2$mean - 2*myData_aux2$se)
  p <- ggplot(data = myData_aux2, aes(x = Acceptance, y = mean, fill = factor(Estimate)))
  p + geom_bar(stat = "identity", position = dodge) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    labs(x = "", y = "%")+
    scale_fill_discrete(name = "", labels=c("1Actual","2Expected Acceptance", "3Expected Undecided","4Expected Rejection"))+ theme(
      axis.text.x = element_blank())+
    ylim(0,100)+
    #scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
    #                            'Low-income+Climate','Universal+Climate'))+ 
    theme(axis.text.x = element_text(size = 12, angle = 50, hjust = 1))+
    theme(axis.text.y = element_text(size = 12),legend.text=element_text(size=12))
  
  
  
  par(mfrow = c(1, 3) ,mar=c(7,4,2,2))
  hist(data$ExpectedAcceptance_CT, main="",ylab="Frequency",xlab="")
  
  hist(data$Acceptance_CT_repeat[which(data$InfoExperiment==0)], main="",ylab="Frequency",xaxt="n",breaks=c(0,1,2,3,4,5),ylim=c(0,400),xlab="")
  axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=  c("5 Completely \n acceptable","4 Somewhat \n acceptable",
                                               "3 Neither/nor","2 Somewhat \n unacceptable","1 Completely \n unacceptable"),las=2) 
  
  
  hist(data$Acceptance_CT_repeat[which(data$InfoExperiment==1)], main="",ylab="Frequency",xaxt="n",breaks=c(0,1,2,3,4,5),ylim=c(0,400),xlab="")
  axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=  c("5 Completely \n acceptable","4 Somewhat \n acceptable",
                                               "3 Neither/nor","2 Somewhat \n unacceptable","1 Completely \n unacceptable"),las=2) 
  
  
  
  #data$ExpectationGap<-data$ExpectedSpanishAcceptability-data$InfoSpanishAcceptability
  data$AcceptabilityDelta<-data$Acceptance_CT_repeat-data$Acceptance_CT
  summary(data$AcceptabilityDelta)
  
  #Table A4. Differences in means between respondents who participated in the first and second survey.
  library(conover.test)
  
  var_of_interest<-data$Acceptance_CT[which(data$NewSurveyParticipant==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$Acceptance_CT[which(data$NewSurveyParticipant==2)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  conover.test(data$Acceptance_CT, g=data$NewSurveyParticipant,method = "bonferroni",alpha=0.01)
  
  var_of_interest<-data$Acceptance_CT_repeat[which(data$NewSurveyParticipant==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$Acceptance_CT_repeat[which(data$NewSurveyParticipant==2)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  conover.test(data$Acceptance_CT_repeat, g=data$NewSurveyParticipant,method = "bonferroni",alpha=0.01)
  
  var_of_interest<-data$ExpectedAcceptance_CT[which(data$NewSurveyParticipant==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$ExpectedAcceptance_CT[which(data$NewSurveyParticipant==2)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  conover.test(data$ExpectedAcceptance_CT, g=data$NewSurveyParticipant,method = "bonferroni",alpha=0.01)
  
  var_of_interest<-data$ExpectedRejection_CT[which(data$NewSurveyParticipant==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$ExpectedRejection_CT[which(data$NewSurveyParticipant==2)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  conover.test(data$ExpectedRejection_CT, g=data$NewSurveyParticipant,method = "bonferroni",alpha=0.01)
  
  var_of_interest<-data$ExpectedNeitherNor_CT[which(data$NewSurveyParticipant==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$ExpectedNeitherNor_CT[which(data$NewSurveyParticipant==2)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  conover.test(data$ExpectedNeitherNor_CT, g=data$NewSurveyParticipant,method = "bonferroni",alpha=0.01)
  

  
  #false consensus = expected acceptability - actual acceptability
  #data$FalseConsensus<-data$ExpectedSpanishAcceptability-sum(data$CT.Acceptability_Rev2>3)/length(data$CT.Acceptability)*100
  data$FalseConsensusNew<-matrix(0,length(data$Acceptance_CT),1)
  data$FalseConsensusNew[data$Acceptance_CT>3]<-data$ExpectedAcceptance_CT[data$Acceptance_CT>3]-
                                             sum(data$Acceptance_CT>3)/length(data$Acceptance_CT)*100
  data$FalseConsensusNew[data$Acceptance_CT<3]<-data$ExpectedRejection_CT[data$Acceptance_CT<3]-
                                             sum(data$Acceptance_CT<3)/length(data$Acceptance_CT)*100
  data$FalseConsensusNew[data$Acceptance_CT==3]<-data$ExpectedNeitherNor_CT[data$Acceptance_CT==3]-
    sum(data$Acceptance_CT==3)/length(data$Acceptance_CT)*100
  
    #summary(data$FalseConsensus)
  summary(data$FalseConsensusNew)
  # 
  
  
  lm_exp<-lm(data$AcceptabilityDelta ~ data$FalseConsensusNew)
  summary(lm_exp)

  data$AcceptabilityDelta<-as.factor(data$AcceptabilityDelta)
  # m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
  #                  , data = data, Hess=TRUE)
  # summary(m_accept)
  # (ctable <- coef(summary(m_accept)))
  # ## calculate and store p values
  # p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # ## combined table
  # (ctable <- cbind(ctable, "p value" = p))
  # PseudoR2(m_accept,which="Nagelkerke")
  
  m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                   , data = data, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  
  #Table 2
  var_of_interest<-data$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data$Acceptance_CT, data$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-data[which(data$InfoExperiment==1),] #those who were provided with information
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-data[which(data$InfoExperiment==0),]
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT>3 & data$InfoExperiment==1)
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT==3 & data$InfoExperiment==1)
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT<3 & data$InfoExperiment==1)
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data,  data$Acceptance_CT>3 & data$InfoExperiment==0)
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  
  data_exp<-subset(data, data$Acceptance_CT==3 & data$InfoExperiment==0)
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data,  data$Acceptance_CT<3 & data$InfoExperiment==0)
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  
  #Table ZYX. Excluding extreme value for the regression to the mean effect
  var_of_interest<-data$Acceptance_CT[which(data$Acceptance_CT %in% c(2,3,4))]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$Acceptance_CT_repeat[which(data$Acceptance_CT %in% c(2,3,4))]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data$Acceptance_CT[which(data$Acceptance_CT %in% c(2,3,4))], 
              data$Acceptance_CT_repeat[which(data$Acceptance_CT %in% c(2,3,4))], paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-data[which(data$InfoExperiment==1 & data$Acceptance_CT %in% c(2,3,4)),] #those who were provided with information
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-data[which(data$InfoExperiment==0 & data$Acceptance_CT %in% c(2,3,4)),]
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT>3 & data$InfoExperiment==1 & data$Acceptance_CT %in% c(2,3,4))
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT==3 & data$InfoExperiment==1 & data$Acceptance_CT %in% c(2,3,4))
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT<3 & data$InfoExperiment==1 & data$Acceptance_CT %in% c(2,3,4))
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data,  data$Acceptance_CT>3 & data$InfoExperiment==0 & data$Acceptance_CT %in% c(2,3,4))
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  
  data_exp<-subset(data, data$Acceptance_CT==3 & data$InfoExperiment==0 & data$Acceptance_CT %in% c(2,3,4))
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  data_exp<-subset(data,  data$Acceptance_CT<3 & data$InfoExperiment==0 & data$Acceptance_CT %in% c(2,3,4))
  var_of_interest<-data_exp$Acceptance_CT
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT, data_exp$Acceptance_CT_repeat, paired = TRUE, alternative = "two.sided")
  
  
  
  
  
  
  
  ###Extras for revision
  
  var_of_interest<-data$Acceptance_CT_repeat[which(data$InfoExperiment==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data$Acceptance_CT_repeat[which(data$InfoExperiment==0)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data$Acceptance_CT_repeat[which(data$InfoExperiment==1)], data$Acceptance_CT_repeat[which(data$InfoExperiment==0)], alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT>3)
  var_of_interest<-data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==0)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==1)], data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==0)],  alternative = "two.sided")
  
  
  data_exp<-subset(data, data$Acceptance_CT==3)
  var_of_interest<-data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==0)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==1)], data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==0)],  alternative = "two.sided")
  
  
  
  data_exp<-subset(data, data$Acceptance_CT<3)
  var_of_interest<-data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==1)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  var_of_interest<-data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==0)]
  mean(var_of_interest)
  mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
  mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))
  
  wilcox.test(data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==1)], data_exp$Acceptance_CT_repeat[which(data_exp$InfoExperiment==0)],  alternative = "two.sided")
  
  
  
  
  # ### FULL SAMPLE TRIAL
  # data_fc<-data
  # data_fc$AcceptabilityDelta<-data_fc$Acceptance_CT_repeat-data_fc$Acceptance_CT
  # data_fc$FCE_Treatment_inter<- data_fc$FalseConsensusNew*as.numeric(data_fc$InfoExperiment==1)
  # lm_exp<-lm(data_fc$AcceptabilityDelta ~ data_fc$FCE_Treatment_inter)
  # summary(lm_exp)
  # 
  # data_fc$FCE_Treatment_Opinion_2inter<- data_fc$FalseConsensusNew*as.numeric(data_fc$InfoExperiment==0)*as.numeric(data_fc$Acceptance_CT>3)
  # lm_exp<-lm(data_fc$AcceptabilityDelta ~ data_fc$FCE_Treatment_Opinion_2inter)
  # summary(lm_exp)
  # 
  # data_fc$FCE_Treatment_Opinion_2inter<- data_fc$FalseConsensusNew*as.numeric(data_fc$InfoExperiment==1)*as.numeric(data_fc$Acceptance_CT<3)
  # lm_exp<-lm(data_fc$AcceptabilityDelta ~ data_fc$FCE_Treatment_Opinion_2inter)
  # summary(lm_exp)
  # 
  # data_fc$FCE_Treatment_Opinion_2inter<- data_fc$FalseConsensusNew*as.numeric(data_fc$InfoExperiment==1)*as.numeric(data_fc$Acceptance_CT>3)
  # lm_exp<-lm(data_fc$AcceptabilityDelta ~ data_fc$FCE_Treatment_Opinion_2inter)
  # summary(lm_exp)
  # 
  # data_fc$FCE_Treatment_Opinion_2inter<- data_fc$FalseConsensusNew*as.numeric(data_fc$InfoExperiment==0)*as.numeric(data_fc$Acceptance_CT<3)
  # lm_exp<-lm(data_fc$AcceptabilityDelta ~ data_fc$FCE_Treatment_Opinion_2inter)
  # summary(lm_exp)
  # ###
  
  
  data$AcceptabilityDelta<-data$Acceptance_CT_repeat-data$Acceptance_CT
  
  data$InteractionSelfPerceived<-data$Acceptance_CT*data$ExpectedAcceptance_CT
  summary(data$InteractionSelfPerceived)
  table(data$InteractionSelfPerceived)
  
  data_exp1<-subset(data, data$Acceptance_CT>3 & data$InfoExperiment==0)
  
  #hist(data_exp1$FalseConsensus)
  hist(data_exp1$FalseConsensusNew)
  # lm_exp<-lm(data_exp1$AcceptabilityDelta ~ data_exp1$FalseConsensus)
  # summary(lm_exp)
  
  lm_exp<-lm(data_exp1$AcceptabilityDelta ~  data_exp1$FalseConsensusNew)
  summary(lm_exp)
  lm_exp2 <- lm(data_exp1$AcceptabilityDelta ~data_exp1$Acceptance_CT+data_exp1$ExpectedAcceptance_CT+
                                              data_exp1$InteractionSelfPerceived )
  summary(lm_exp2)
  
  lm_exp<-lm( data_exp1$FalseConsensusNew ~ data_exp1$AcceptabilityDelta)
  summary(lm_exp)
  lm_exp<-lm( data_exp1$FalseConsensusNew ~ data_exp1$AcceptabilityDelta+data_exp1$Acceptance_CT)
  summary(lm_exp)
  library(caret)
  model1 <- lm(data_exp1$FalseConsensusNew ~data_exp1$AcceptabilityDelta+data_exp1$Acceptance_CT)
  summary(model1)
  car::vif(model1)
  
  
  library(quantreg)
  QR<-rq(data_exp1$FalseConsensusNew ~ data_exp1$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
  sumQR<-summary(QR)
  plot(sumQR)
  quantile(data_exp1$FalseConsensusNew, c(.2, .4, .6,.8)) 
  
  data_exp1$AcceptabilityDelta<-as.factor(data_exp1$AcceptabilityDelta)
  # m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
  #                  , data =data_exp1, Hess=TRUE)
  # summary(m_accept)
  # (ctable <- coef(summary(m_accept)))
  # ## calculate and store p values
  # p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # ## combined table
  # (ctable <- cbind(ctable, "p value" = p))
  # PseudoR2(m_accept,which="Nagelkerke")
  # 
  m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                   , data =data_exp1, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  data_exp1$AcceptabilityDelta<- data_exp1$Acceptance_CT_repeat-data_exp1$Acceptance_CT
  
  
  
  
  data_exp2<-subset(data, data$Acceptance_CT<3 & data$InfoExperiment==1)
  # hist(data_exp2$FalseConsensus)
  hist(data_exp2$FalseConsensusNew)
  lm_exp<-lm(data_exp2$AcceptabilityDelta ~ data_exp2$FalseConsensusNew )
  summary(lm_exp)
  lm_exp2 <- lm(data_exp2$AcceptabilityDelta ~data_exp2$Acceptance_CT+data_exp2$ExpectedAcceptance_CT+
                  data_exp2$InteractionSelfPerceived )
  summary(lm_exp2)
  # lm_exp<-lm(data_exp2$AcceptabilityDelta ~ data_exp2$FalseConsensus)
  # summary(lm_exp)
  # 
  lm_exp<-lm(data_exp2$FalseConsensusNew ~ data_exp2$AcceptabilityDelta)
  summary(lm_exp)
  lm_exp<-lm( data_exp2$FalseConsensusNew ~ data_exp2$AcceptabilityDelta+data_exp2$Acceptance_CT)
  summary(lm_exp)
  library(caret)
  model1 <- lm(data_exp2$FalseConsensusNew ~data_exp2$AcceptabilityDelta+data_exp2$Acceptance_CT)
  summary(model1)
  car::vif(model1)
  QR<-rq(data_exp2$FalseConsensusNew ~ data_exp2$AcceptabilityDelta, tau=seq(0.2, 0.8, by=0.15))
  sumQR<-summary(QR)
  plot(sumQR)
  quantile(data_exp2$FalseConsensusNew, c(.2, .4, .6,.8)) 
  
  data_exp2$AcceptabilityDelta<-as.factor(data_exp2$AcceptabilityDelta)
  # m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
  #                  , data =data_exp2, Hess=TRUE)
  # summary(m_accept)
  # (ctable <- coef(summary(m_accept)))
  # ## calculate and store p values
  # p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # ## combined table
  # (ctable <- cbind(ctable, "p value" = p))
  # PseudoR2(m_accept,which="Nagelkerke")
  # 
  m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                   , data =data_exp2, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  data_exp2$AcceptabilityDelta<-data_exp2$Acceptance_CT_repeat-data_exp2$Acceptance_CT

  
  data_exp3<-subset(data, data$Acceptance_CT>3 & data$InfoExperiment==1)
  # hist(data_exp3$FalseConsensus)
  hist(data_exp3$FalseConsensusNew)
  lm_exp<-lm(data_exp3$AcceptabilityDelta~ data_exp3$FalseConsensusNew )
  summary(lm_exp)
  lm_exp2 <- lm(data_exp3$AcceptabilityDelta ~data_exp3$Acceptance_CT+data_exp3$ExpectedAcceptance_CT+
                  data_exp3$InteractionSelfPerceived )
  summary(lm_exp2)
  # lm_exp<-lm(data_exp3$AcceptabilityDelta ~ data_exp3$FalseConsensus)
  # summary(lm_exp)
  # 
  lm_exp<-lm(data_exp3$FalseConsensusNew ~ data_exp3$AcceptabilityDelta)
  summary(lm_exp)
  lm_exp<-lm( data_exp3$FalseConsensusNew ~ data_exp3$AcceptabilityDelta+data_exp3$Acceptance_CT)
  summary(lm_exp)
  library(caret)
  model1 <- lm(data_exp3$FalseConsensusNew ~data_exp3$AcceptabilityDelta+data_exp3$Acceptance_CT)
  summary(model1)
  car::vif(model1)
  
  QR<-rq(data_exp3$FalseConsensusNew ~ data_exp3$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
  sumQR<-summary(QR)
  plot(sumQR)
  quantile(data_exp3$FalseConsensusNew, c(.2, .4, .6,.8)) 
  
  
  data_exp3$AcceptabilityDelta<-as.factor(data_exp3$AcceptabilityDelta)
  # m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
  #                  , data =data_exp3, Hess=TRUE)
  # summary(m_accept)
  # (ctable <- coef(summary(m_accept)))
  # ## calculate and store p values
  # p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # ## combined table
  # (ctable <- cbind(ctable, "p value" = p))
  # PseudoR2(m_accept,which="Nagelkerke")
  # 
  m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                   , data =data_exp3, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  data_exp3$AcceptabilityDelta<-data_exp3$Acceptance_CT_repeat-data_exp3$Acceptance_CT


  
  data_exp4<-subset(data, data$Acceptance_CT<3 & data$InfoExperiment==0)
  # hist(data_exp4$FalseConsensus)
  hist(data_exp4$FalseConsensusNew)
  lm_exp<-lm(data_exp4$AcceptabilityDelta ~ data_exp4$FalseConsensusNew)
  summary(lm_exp)
  lm_exp2 <- lm(data_exp4$AcceptabilityDelta ~data_exp4$Acceptance_CT+data_exp4$ExpectedAcceptance_CT+
                  data_exp4$InteractionSelfPerceived )
  summary(lm_exp2)
  # lm_exp<-lm(data_exp4$AcceptabilityDelta ~ data_exp4$FalseConsensus)
  # summary(lm_exp)
  # 
  lm_exp<-lm(data_exp4$FalseConsensusNew ~ data_exp4$AcceptabilityDelta)
  summary(lm_exp)
  lm_exp<-lm( data_exp4$FalseConsensusNew ~ data_exp4$AcceptabilityDelta+data_exp4$Acceptance_CT)
  summary(lm_exp)
  library(caret)
  model1 <- lm(data_exp4$FalseConsensusNew ~data_exp4$AcceptabilityDelta+data_exp4$Acceptance_CT)
  summary(model1)
  car::vif(model1)
  
  QR<-rq(data_exp4$FalseConsensusNew ~ data_exp4$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
  sumQR<-summary(QR)
  plot(sumQR)
  quantile(data_exp4$FalseConsensusNew, c(.2, .4, .6,.8)) 
  
  data_exp4$AcceptabilityDelta<-as.factor(data_exp4$AcceptabilityDelta)
  # m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
  #                  , data =data_exp4, Hess=TRUE)
  # summary(m_accept)
  # (ctable <- coef(summary(m_accept)))
  # ## calculate and store p values
  # p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # ## combined table
  # (ctable <- cbind(ctable, "p value" = p))
  # PseudoR2(m_accept,which="Nagelkerke")
  # 
  m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                   , data =data_exp4, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  data_exp4$AcceptabilityDelta<-data_exp4$Acceptance_CT_repeat-data_exp4$Acceptance_CT

  
  data_exp5<-subset(data, data$Acceptance_CT==3 & data$InfoExperiment==1)
  # hist(data_exp4$FalseConsensus)
  hist(data_exp5$FalseConsensusNew)
  lm_exp<-lm(data_exp5$AcceptabilityDelta ~ data_exp5$FalseConsensusNew)
  summary(lm_exp)
  lm_exp2 <- lm(data_exp5$AcceptabilityDelta ~data_exp5$Acceptance_CT+data_exp5$ExpectedAcceptance_CT+
                  data_exp5$InteractionSelfPerceived )
  summary(lm_exp2)
  # lm_exp<-lm(data_exp4$AcceptabilityDelta ~ data_exp4$FalseConsensus)
  # summary(lm_exp)
  # 
  lm_exp<-lm(data_exp5$FalseConsensusNew ~ data_exp5$AcceptabilityDelta)
  summary(lm_exp)
  lm_exp<-lm( data_exp5$FalseConsensusNew ~ data_exp5$AcceptabilityDelta+data_exp5$Acceptance_CT)
  summary(lm_exp)
  library(caret)
  model1 <- lm(data_exp5$FalseConsensusNew ~data_exp5$AcceptabilityDelta+data_exp5$Acceptance_CT)
  summary(model1)
  car::vif(model1)
  
  QR<-rq(data_exp5$FalseConsensusNew ~ data_exp5$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
  sumQR<-summary(QR)
  plot(sumQR)
  quantile(data_exp5$FalseConsensusNew, c(.2, .4, .6,.8)) 
  
  data_exp5$AcceptabilityDelta<-as.factor(data_exp5$AcceptabilityDelta)
  # m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
  #                  , data =data_exp4, Hess=TRUE)
  # summary(m_accept)
  # (ctable <- coef(summary(m_accept)))
  # ## calculate and store p values
  # p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # ## combined table
  # (ctable <- cbind(ctable, "p value" = p))
  # PseudoR2(m_accept,which="Nagelkerke")
  # 
  m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                   , data =data_exp5, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  data_exp5$AcceptabilityDelta<-data_exp5$Acceptance_CT_repeat-data_exp5$Acceptance_CT
  
  
  data_exp6<-subset(data, data$Acceptance_CT==3 & data$InfoExperiment==0)
  # hist(data_exp4$FalseConsensus)
  hist(data_exp6$FalseConsensusNew)
  lm_exp<-lm(data_exp6$AcceptabilityDelta ~ data_exp6$FalseConsensusNew)
  summary(lm_exp)
  lm_exp2 <- lm(data_exp6$AcceptabilityDelta ~data_exp6$Acceptance_CT+data_exp6$ExpectedAcceptance_CT+
                  data_exp6$InteractionSelfPerceived )
  summary(lm_exp2)
  # lm_exp<-lm(data_exp4$AcceptabilityDelta ~ data_exp4$FalseConsensus)
  # summary(lm_exp)
  # 
  lm_exp<-lm(data_exp6$FalseConsensusNew ~ data_exp6$AcceptabilityDelta)
  summary(lm_exp)
  lm_exp<-lm( data_exp6$FalseConsensusNew ~ data_exp6$AcceptabilityDelta+data_exp6$Acceptance_CT)
  summary(lm_exp)
  library(caret)
  model1 <- lm(data_exp6$FalseConsensusNew ~data_exp6$AcceptabilityDelta+data_exp6$Acceptance_CT)
  summary(model1)
  car::vif(model1)
  
  QR<-rq(data_exp6$FalseConsensusNew ~ data_exp6$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
  sumQR<-summary(QR)
  plot(sumQR)
  quantile(data_exp6$FalseConsensusNew, c(.2, .4, .6,.8)) 
  
  data_exp6$AcceptabilityDelta<-as.factor(data_exp6$AcceptabilityDelta)
  # m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
  #                  , data =data_exp4, Hess=TRUE)
  # summary(m_accept)
  # (ctable <- coef(summary(m_accept)))
  # ## calculate and store p values
  # p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # ## combined table
  # (ctable <- cbind(ctable, "p value" = p))
  # PseudoR2(m_accept,which="Nagelkerke")
  # 
  m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                   , data =data_exp6, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  data_exp6$AcceptabilityDelta<-data_exp6$Acceptance_CT_repeat-data_exp6$Acceptance_CT
  
  
  
  
  
  p1<-ggplot(data_exp3, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ ylim(-50, 100)+
    geom_smooth(method = "lm", se = FALSE,colour="red")+ 
    #scale_alpha_continuous(limits=c(0.001,0.02))+ 
    scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT acceptance and information",
                                                       x ="Change in acceptance rate", y = "False consensus effect")+
    theme(text = element_text(size = 14))
  
  
  p2<-ggplot(data_exp2, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ ylim(-50, 100)+
    geom_smooth(method = "lm", se = FALSE,colour="red")+ 
    scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT rejection and information",
                                                       x ="Change in acceptance rate", y = "False consensus effect")+
    theme(text = element_text(size = 14))
  
  p3<-ggplot(data_exp1, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ ylim(-50, 100)+
    geom_smooth(method = "lm", se = FALSE,colour="red")+ 
    scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT acceptance and no information",
                                                       x ="Change in acceptance rate", y = "False consensus effect")+
    theme(text = element_text(size = 14))
  
  p4<-ggplot(data_exp4, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ ylim(-50, 100)+
    geom_smooth(method = "lm", se = FALSE,colour="red")+ 
    scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT rejection and no information",
                                                       x ="Change in acceptance rate", y = "False consensus effect")+
    theme(text = element_text(size = 14))
  
  p5<-ggplot(data_exp5, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ ylim(-50, 100)+
    geom_smooth(method = "lm", se = FALSE,colour="red")+ 
    scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT undecided and information",
                                                       x ="Change in acceptance rate", y = "False consensus effect")+
    theme(text = element_text(size = 14))
  
  p6<-ggplot(data_exp6, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ ylim(-50, 100)+
    geom_smooth(method = "lm", se = FALSE,colour="red")+ 
    scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT undecided and no information",
                                                       x ="Change in acceptance rate", y = "False consensus effect")+
    theme(text = element_text(size = 14))
  
  
  
  
  grid.arrange(p1, p5, p2, p3, p6, p4, ncol=3)
  
  