
setwd("D:/Projects/2_Working Papers/Topic Modelling Climate/Mestre et al")
#install.packages(c("magrittr", "data.table", 'dplyr', 'ggplot2', 'cluster'))
sapply(c("MASS","DescTools", "magrittr", "data.table", 'plyr','dplyr', 'ggplot2', 'cluster', 'dendextend','pkgbuild','devtools','R.methodsS3','stm','ngram','matrixStats','corrplot','ggpubr','wordcloud','ggplot2'), require, character.only = T)

knitr::opts_chunk$set(fig.width=12, fig.height=12) 
data <- read.csv('UABSES_158632_20190822_spelling corrected_noaccent.csv', header=T, sep=',')
data<-data[,c(-1,-3,-4,-5,-7,-8,-9,-10,-74)]#drop irrelevant columns

PeerEffectAnalysis<-0

for (column in c(2:7,9:53,55:67)){
  data[,column]<-as.numeric(as.character(data[,column])) #make sure all numbers are recognized as numbers
  #data[which(data[,column]==.),column]<-NA #make 8 (no opinion) NA
}
#67 columns, with two being textual responses

# #cleaning, renaming and recoding variabe consistent with the other survey
# data$timing_rec3[data$timing_rec3==3]<-NA
# data$timing_rec3[data$timing_rec3==1]<-0
# data$timing_rec3[data$timing_rec3==2]<-1
# #colnames(data)[18]<-"belief.end.of.growth_recoded"
# data$gender[data$gender==3]<-NA
# data$gender[data$gender==2]<-0 #recoding the variable to make consistent with the other survey
# data$favored_growth_environ_strategy[data$favored_growth_environ_strategy==5]<-NA

colnames(data)<-c("Response.ID","Duration","Gender","Age","Age.cohort","Regional.Code","Number.Inhabitants",
                  "Text.CarbonTaxPolicy","Perceived.CarbonTaxKnowledge",
                  "Knowledge.Test1","Knowledge.Test2","Knowledge.Test3","Knowledge.Test4","Knowledge.Test5","Knowledge.Test6",
                  "Experiment.Group","CT.Effectiveness","CT.Fairness","CT.Object","CT.PersonalCost","CT.LowIncomeEffect","CT.TrustInPoliticians","CT.Acceptability","CT.Objective",
                  "CT.Effectiveness_Rev1","CT.Effectiveness_Rev2","CT.Effectiveness_Rev3","CT.Effectiveness_Rev4","CT.Effectiveness_Rev5","CT.Fairness_Rev1","CT.Fairness_Rev2","CT.Fairness_Rev3","CT.Fairness_Rev4","CT.Fairness_Rev5",
                  "CT.PersonalCost_Rev1","CT.PersonalCost_Rev2","CT.PersonalCost_Rev3","CT.PersonalCost_Rev4","CT.PersonalCost_Rev5",
                  "CT.LowIncomeEffect_Rev1","CT.LowIncomeEffect_Rev2","CT.LowIncomeEffect_Rev3","CT.LowIncomeEffect_Rev4","CT.LowIncomeEffect_Rev5",
                  "CT.Acceptability_Rev1","CT.Acceptability_Rev2","CT.Acceptability_Rev3","CT.Acceptability_Rev4","CT.Acceptability_Rev5","Allocation_Climate","Allocation_LowIncome","Allocation_Universal","EffectivenessOrFairness","Text.CTFairness","NumberOfPeers","NumberOfPeersClimChange","ExpectedSpanishAcceptability","InfoSpanishAcceptability","AcceptabilityAfterInfo","HouseholdSize","MonthlyIncome","Education","ClimateConcern","PoliticalView","PoliticalParty","CarUse","DrivingTime")        

# library(openxlsx)
# write.xlsx(data, 'file_for_Sara.xlsx')
data$InfoSpanishAcceptability[data$InfoSpanishAcceptability==1]<-19
data$InfoSpanishAcceptability[data$InfoSpanishAcceptability==2]<-67

summary(data[,c(2:7,9:53,55:67)])



  data$AvCT.Acceptability<-data$CT.Acceptability_Rev2
  
                           # rowMeans(cbind(data$CT.Acceptability_Rev1,data$CT.Acceptability_Rev2,
                           #        data$CT.Acceptability_Rev3,data$CT.Acceptability_Rev4,
                           #        data$CT.Acceptability_Rev5))
  
  myData_aux<-matrix(0,4,2)
  myData_aux<-as.data.frame(myData_aux)
  colnames(myData_aux)<-c("Estimate", "Acceptance")
  myData_aux[,1]<-c("Actual","Expected","Actual","Expected")
  myData_aux[,2]<-c("4-5 \n Acceptance","4-5 \n Acceptance","1-3 Non- \n acceptance","1-3 Non- \n acceptance")
  myData_aux$mean<-c(sum(data$AvCT.Acceptability>3)/length(data$AvCT.Acceptability)*100,
                     mean(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability>3]),
                     sum(data$AvCT.Acceptability<=3)/length(data$AvCT.Acceptability)*100,
                     100-mean(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability<=3]))
  myData_aux$sd<-c(0,sd(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability>3]),
                   0,sd(100-data$ExpectedSpanishAcceptability[data$AvCT.Acceptability<=3]))
  myData_aux$n<-c(1,sum(data$AvCT.Acceptability>3),1,sum(data$AvCT.Acceptability<=3))
  #myData_aux <- do.call(data.frame, myData_aux)
  myData_aux$se<-myData_aux$sd / sqrt(myData_aux$n)
  
  
  dodge <- position_dodge(width = 0.9)
  limits <- aes(ymax = myData_aux$mean + 2*myData_aux$se,
                ymin = myData_aux$mean - 2*myData_aux$se)
  p <- ggplot(data = myData_aux, aes(x = Acceptance, y = mean, fill = factor(Estimate)))
  p + geom_bar(stat = "identity", position = dodge) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    labs(x = "", y = "%")+
    scale_fill_discrete(name = "", labels=c("Actual", "Expected"))+ theme(
      axis.text.x = element_blank())+
    ylim(0,100)+
    #scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
    #                            'Low-income+Climate','Universal+Climate'))+ 
    theme(axis.text.x = element_text(size = 12, angle = 50, hjust = 1))+
    theme(axis.text.y = element_text(size = 12),legend.text=element_text(size=12))
  
  myData_aux<-matrix(0,10,2)
  myData_aux<-as.data.frame(myData_aux)
  colnames(myData_aux)<-c("Estimate", "Acceptance")
  myData_aux[,1]<-c("Actual","Expected","Actual","Expected","Actual","Expected","Actual","Expected","Actual","Expected")
  myData_aux[,2]<-c("5 Completely \n acceptable","5 Completely \n acceptable","4 Somewhat \n acceptable","4 Somewhat \n acceptable"
                    ,"3 Neither/nor","3 Neither/nor","2 Somewhat \n unacceptable","2 Somewhat \n unacceptable","1 Completely \n unacceptable","1 Completely \n unacceptable")
  myData_aux$mean<-c(sum(data$AvCT.Acceptability==5)/length(data$AvCT.Acceptability)*100,
                     mean(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==5]),
                     sum(data$AvCT.Acceptability==4)/length(data$AvCT.Acceptability)*100,
                     mean(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==4]),
                     sum(data$AvCT.Acceptability==3)/length(data$AvCT.Acceptability)*100,
                     100-mean(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==3]),
                     sum(data$AvCT.Acceptability==2)/length(data$AvCT.Acceptability)*100,
                     100-mean(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==2]),
                     sum(data$AvCT.Acceptability==1)/length(data$AvCT.Acceptability)*100,
                     100-mean(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==1]))
  myData_aux$sd<-c(0,sd(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==5]),
                   0,sd(data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==4]),
                   0,sd(100-data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==3]),
                   0,sd(100-data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==2]),
                   0,sd(100-data$ExpectedSpanishAcceptability[data$AvCT.Acceptability==1]))
  myData_aux$n<-c(1,sum(data$AvCT.Acceptability==5),1,sum(data$AvCT.Acceptability==4),
                  1,sum(data$AvCT.Acceptability==3),1,sum(data$AvCT.Acceptability==2),
                  1,sum(data$AvCT.Acceptability==1))
  #myData_aux <- do.call(data.frame, myData_aux)
  myData_aux$se<-myData_aux$sd / sqrt(myData_aux$n)
  
  #Figure 1
  dodge <- position_dodge(width = 0.9)
  limits <- aes(ymax = myData_aux$mean + 2*myData_aux$se,
                ymin = myData_aux$mean - 2*myData_aux$se)
  p <- ggplot(data = myData_aux, aes(x = Acceptance, y = mean, fill = factor(Estimate)))
  p + geom_bar(stat = "identity", position = dodge) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    labs(x = "", y = "%")+
    scale_fill_discrete(name = "", labels=c("Actual", "Expected"))+ theme(
      axis.text.x = element_blank())+
    ylim(0,100)+
    #scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
    #                            'Low-income+Climate','Universal+Climate'))+ 
    theme(axis.text.x = element_text(size = 12, angle = 50, hjust = 1))+
    theme(axis.text.y = element_text(size = 12),legend.text=element_text(size=12))
  
#Figure 1 in the Appendix
  par(mfrow = c(1, 3) ,mar=c(7,4,2,2))
  hist(data$ExpectedSpanishAcceptability, main="",ylab="Frequency",xlab="")
  
  hist(data$AcceptabilityAfterInfo[which(data$InfoSpanishAcceptability==19)], main="",ylab="Frequency",xaxt="n",breaks=c(0,1,2,3,4,5),ylim=c(0,400),xlab="")
  axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=  c("5 Completely \n acceptable","4 Somewhat \n acceptable",
    "3 Neither/nor","2 Somewhat \n unacceptable","1 Completely \n unacceptable"),las=2) 
  
  
  hist(data$AcceptabilityAfterInfo[which(data$InfoSpanishAcceptability==67)], main="",ylab="Frequency",xaxt="n",breaks=c(0,1,2,3,4,5),ylim=c(0,400),xlab="")
  axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=  c("5 Completely \n acceptable","4 Somewhat \n acceptable",
                                               "3 Neither/nor","2 Somewhat \n unacceptable","1 Completely \n unacceptable"),las=2) 
  
  
  
  
  
  
  lm_exp<-lm(data$CT.Acceptability ~ data$ExpectedSpanishAcceptability)
  summary(lm_exp)
  
  data$CT.Acceptability<-as.factor(data$CT.Acceptability)
  m_accept <- polr(CT.Acceptability ~  ExpectedSpanishAcceptability
                   , data = data, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  data$CT.Acceptability<-as.numeric(data$CT.Acceptability)
  
  data$AvCT.Acceptability<-data$CT.Acceptability_Rev2
    # rowMeans(cbind(data$CT.Acceptability_Rev1,data$CT.Acceptability_Rev2,
    #      data$CT.Acceptability_Rev3,data$CT.Acceptability_Rev4,
    #      data$CT.Acceptability_Rev5))
  
  lm_exp<-lm(data$AvCT.Acceptability ~ data$ExpectedSpanishAcceptability)
  summary(lm_exp)
  data_fcb<-data
  data_fcb$PoliticalView[data_fcb$PoliticalView==11]<-NA
  data_fcb$MonthlyIncome[data_fcb$MonthlyIncome==7]<-NA
  lm_exp<-lm(data_fcb$AvCT.Acceptability ~ data_fcb$ExpectedSpanishAcceptability + data_fcb$Age +data_fcb$Gender + 
                                       data_fcb$Education + data_fcb$MonthlyIncome + data_fcb$PoliticalView)
  summary(lm_exp)
  
  data$AvCT.Acceptability<-as.factor(data$AvCT.Acceptability)
  m_accept <- polr(AvCT.Acceptability ~  ExpectedSpanishAcceptability
                   , data = data, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")

  
  data_fcb$AvCT.Acceptability<-as.factor(data_fcb$AvCT.Acceptability)
  m_accept <- polr(AvCT.Acceptability ~  ExpectedSpanishAcceptability +Age +Gender + 
                     Education + MonthlyIncome + PoliticalView
                   , data = data_fcb, Hess=TRUE)
  summary(m_accept)
  (ctable <- coef(summary(m_accept)))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  PseudoR2(m_accept,which="Nagelkerke")
  
  data$AvCT.Acceptability<-as.numeric(data$AvCT.Acceptability)

data$ExpectationGap<-data$ExpectedSpanishAcceptability-data$InfoSpanishAcceptability
data$AcceptabilityDelta<-data$AcceptabilityAfterInfo-data$CT.Acceptability_Rev2
summary(data$AcceptabilityDelta)
#false consensus = expected acceptability - actual acceptability
data$FalseConsensus<-data$ExpectedSpanishAcceptability-sum(data$CT.Acceptability_Rev2>3)/length(data$CT.Acceptability)*100
data$FalseConsensusNew<-matrix(0,length(data$ExpectedSpanishAcceptability),1)
data$FalseConsensusNew[data$CT.Acceptability_Rev2>3]<-data$ExpectedSpanishAcceptability[data$CT.Acceptability_Rev2>3]-sum(data$CT.Acceptability_Rev2>3)/length(data$CT.Acceptability)*100
data$FalseConsensusNew[data$CT.Acceptability_Rev2<=3]<-(100-data$ExpectedSpanishAcceptability[data$CT.Acceptability_Rev2<=3])-sum(data$CT.Acceptability_Rev2<=3)/length(data$CT.Acceptability)*100

summary(data$FalseConsensus)
summary(data$FalseConsensusNew)

DataCorrplot_fcb<-cbind(data$FalseConsensusNew,data$MonthlyIncome,data$PoliticalView,data$Age,
                        data$Gender,data$Number.Inhabitants,data$CT.TrustInPoliticians,
                        data$NumberOfPeers,data$NumberOfPeersClimChange,data$HouseholdSize,
                        data$Education,data$ClimateConcern)
DataCorrplot_fcb<-as.data.frame(DataCorrplot_fcb)

colnames(DataCorrplot_fcb)<-c("FalseConsensusNew","MonthlyIncome","PoliticalView","Age",
                               "Gender","Number.Inhabitants","CT.TrustInPoliticians",
                               "NumberOfPeers","NumberOfPeersClimChange","HouseholdSize",
                               "Education","ClimateConcern")
summary(DataCorrplot_fcb)
par(mfrow = c(1, 2) ,mar=c(4,4,1,2))
hist(DataCorrplot_fcb$PoliticalView)
hist(DataCorrplot_fcb$MonthlyIncome)

DataCorrplot_fcb$PoliticalView[DataCorrplot_fcb$PoliticalView==11]<-NA
DataCorrplot_fcb$MonthlyIncome[DataCorrplot_fcb$MonthlyIncome==7]<-NA

DataCorrplot_fcb<-DataCorrplot_fcb[-which(is.na(DataCorrplot_fcb$PoliticalView)),]
DataCorrplot_fcb<-DataCorrplot_fcb[-which(is.na(DataCorrplot_fcb$MonthlyIncome)),]
ncol<-ncol(DataCorrplot_fcb)
Mmatrix<-matrix(0,ncol,ncol)
for(i in 1:ncol){
  for (j in 1:ncol){
    if (i>j){cc<-cor.test(DataCorrplot_fcb[,i],DataCorrplot_fcb[,j],method = "pearson")
    Mmatrix[i,j]<-cc$estimate
    } else if(j>i){cc<-cor.test(DataCorrplot_fcb[,i],DataCorrplot_fcb[,j],method = "spearman",exact = FALSE)
    Mmatrix[i,j]<-cc$estimate
    } else{ Mmatrix[i,j]<-1}
  }
}
colnames(Mmatrix)<-c("FalseConsensusNew","MonthlyIncome","PoliticalView","Age",
                      "Gender","Number.Inhabitants","CT.TrustInPoliticians",
                      "NumberOfPeers","NumberOfPeersClimChange","HouseholdSize",
                      "Education","ClimateConcern")
rownames(Mmatrix)<-c("FalseConsensusNew","MonthlyIncome","PoliticalView","Age",
                      "Gender","Number.Inhabitants","CT.TrustInPoliticians",
                      "NumberOfPeers","NumberOfPeersClimChange","HouseholdSize",
                      "Education","ClimateConcern")
par(mfrow = c(1, 1) ,mar=c(1,1,1,1))
corrplot(Mmatrix, method = "color",col=colorRampPalette(c("blue","white","red"))(200),
         order = "hclust", number.cex = .5,cl.lim=c(-1,1),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90,tl.cex=.6) # Text label color and rotation)



par(mfrow = c(1, 2) ,mar=c(4,4,1,2))
hist(data$FalseConsensus, main=" ", xlab="Expectation error")
hist(data$FalseConsensusNew, main=" ", xlab="False Consensus")

lm_exp<-lm(data$AcceptabilityDelta ~ data$ExpectationGap)
summary(lm_exp)

lm_exp<-lm(data$AcceptabilityDelta ~ data$FalseConsensus)
summary(lm_exp)

lm_exp<-lm(data$AcceptabilityDelta ~ data$FalseConsensusNew)
summary(lm_exp)

data$AcceptabilityDelta<-as.factor(data$AcceptabilityDelta)
m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
                 , data = data, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                 , data = data, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

var_of_interest<-data$CT.Acceptability_Rev2
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

var_of_interest<-data$AcceptabilityAfterInfo
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

wilcox.test(data$CT.Acceptability_Rev2, data$AcceptabilityAfterInfo, paired = TRUE, alternative = "two.sided")


data_exp<-data[which(data$InfoSpanishAcceptability==19),]
var_of_interest<-data_exp$CT.Acceptability_Rev2
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

var_of_interest<-data_exp$AcceptabilityAfterInfo
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

wilcox.test(data_exp$CT.Acceptability_Rev2, data_exp$AcceptabilityAfterInfo, paired = TRUE, alternative = "two.sided")


data_exp<-data[which(data$InfoSpanishAcceptability==67),]
var_of_interest<-data_exp$CT.Acceptability_Rev2
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

var_of_interest<-data_exp$AcceptabilityAfterInfo
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

wilcox.test(data_exp$CT.Acceptability_Rev2, data_exp$AcceptabilityAfterInfo, paired = TRUE, alternative = "two.sided")


data_exp<-subset(data, data$CT.Acceptability_Rev2>3 & data$InfoSpanishAcceptability==19)
var_of_interest<-data_exp$CT.Acceptability_Rev2
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

var_of_interest<-data_exp$AcceptabilityAfterInfo
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

wilcox.test(data_exp$CT.Acceptability_Rev2, data_exp$AcceptabilityAfterInfo, paired = TRUE, alternative = "two.sided")


data_exp<-subset(data, data$CT.Acceptability_Rev2>3 & data$InfoSpanishAcceptability==67)
var_of_interest<-data_exp$CT.Acceptability_Rev2
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

var_of_interest<-data_exp$AcceptabilityAfterInfo
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

wilcox.test(data_exp$CT.Acceptability_Rev2, data_exp$AcceptabilityAfterInfo, paired = TRUE, alternative = "two.sided")


data_exp<-subset(data, data$CT.Acceptability_Rev2<=3 & data$InfoSpanishAcceptability==19)
var_of_interest<-data_exp$CT.Acceptability_Rev2
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

var_of_interest<-data_exp$AcceptabilityAfterInfo
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

wilcox.test(data_exp$CT.Acceptability_Rev2, data_exp$AcceptabilityAfterInfo, paired = TRUE, alternative = "two.sided")


data_exp<-subset(data, data$CT.Acceptability_Rev2<=3 & data$InfoSpanishAcceptability==67)
var_of_interest<-data_exp$CT.Acceptability_Rev2
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

var_of_interest<-data_exp$AcceptabilityAfterInfo
mean(var_of_interest)
mean(var_of_interest)-2*sd(var_of_interest)/sqrt(length(var_of_interest))
mean(var_of_interest)+2*sd(var_of_interest)/sqrt(length(var_of_interest))

wilcox.test(data_exp$CT.Acceptability_Rev2, data_exp$AcceptabilityAfterInfo, paired = TRUE, alternative = "two.sided")





data$AcceptabilityDelta<-data$AcceptabilityAfterInfo-data$CT.Acceptability_Rev2

data$InteractionSelfPerceived<-data$CT.Acceptability_Rev2*data$ExpectedSpanishAcceptability
summary(data$InteractionSelfPerceived)
table(data$InteractionSelfPerceived)

data_exp1<-subset(data, data$CT.Acceptability_Rev2>3 & data$InfoSpanishAcceptability==19)

hist(data_exp1$FalseConsensus)
hist(data_exp1$FalseConsensusNew)
lm_exp<-lm(data_exp1$AcceptabilityDelta ~ data_exp1$FalseConsensusNew)
summary(lm_exp)
lm_exp2 <- lm(data_exp1$AcceptabilityDelta ~data_exp1$CT.Acceptability_Rev2+data_exp1$ExpectedSpanishAcceptability+
                data_exp1$InteractionSelfPerceived )
summary(lm_exp2)

lm_exp<-lm( data_exp1$FalseConsensusNew ~ data_exp1$AcceptabilityDelta)
summary(lm_exp)


lm_exp<-lm( data_exp1$FalseConsensusNew ~ data_exp1$AcceptabilityDelta+data_exp1$CT.Acceptability_Rev2)
summary(lm_exp)
library(caret)
model1 <- lm(data_exp1$FalseConsensusNew ~data_exp1$AcceptabilityDelta+data_exp1$CT.Acceptability_Rev2)
summary(model1)
car::vif(model1)


library(quantreg)
QR<-rq(data_exp1$FalseConsensusNew ~ data_exp1$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
sumQR<-summary(QR)
plot(sumQR)
quantile(data_exp1$FalseConsensusNew, c(.2, .4, .6,.8)) 

data_exp1$AcceptabilityDelta<-as.factor(data_exp1$AcceptabilityDelta)
m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
                 , data =data_exp1, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                 , data =data_exp1, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")
data_exp1$AcceptabilityDelta<-data_exp1$AcceptabilityAfterInfo-data_exp1$CT.Acceptability_Rev2



data_exp2<-subset(data, data$CT.Acceptability_Rev2<=3 & data$InfoSpanishAcceptability==67)
hist(data_exp2$FalseConsensus)
hist(data_exp2$FalseConsensusNew)

lm_exp<-lm(data_exp2$AcceptabilityDelta ~ data_exp2$FalseConsensusNew)
summary(lm_exp)
lm_exp2 <- lm(data_exp2$AcceptabilityDelta ~data_exp2$CT.Acceptability_Rev2+data_exp2$ExpectedSpanishAcceptability+
                data_exp2$InteractionSelfPerceived )
summary(lm_exp2)

lm_exp<-lm(data_exp2$FalseConsensusNew ~ data_exp2$AcceptabilityDelta)
summary(lm_exp)
lm_exp<-lm( data_exp2$FalseConsensusNew ~ data_exp2$AcceptabilityDelta+data_exp2$CT.Acceptability_Rev2)
summary(lm_exp)
library(caret)
model1 <- lm(data_exp2$FalseConsensusNew ~data_exp2$AcceptabilityDelta+data_exp2$CT.Acceptability_Rev2)
summary(model1)
car::vif(model1)

QR<-rq(data_exp2$FalseConsensusNew ~ data_exp2$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
sumQR<-summary(QR)
plot(sumQR)
quantile(data_exp2$FalseConsensusNew, c(.2, .4, .6,.8)) 


data_exp2$AcceptabilityDelta<-as.factor(data_exp2$AcceptabilityDelta)
m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
                 , data =data_exp2, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                 , data =data_exp2, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")
data_exp2$AcceptabilityDelta<-data_exp2$AcceptabilityAfterInfo-data_exp2$CT.Acceptability_Rev2


data_exp3<-subset(data, data$CT.Acceptability_Rev2>3 & data$InfoSpanishAcceptability==67)
hist(data_exp3$FalseConsensus)
hist(data_exp3$FalseConsensusNew)
lm_exp<-lm(data_exp3$AcceptabilityDelta ~ data_exp3$FalseConsensusNew)
summary(lm_exp)
lm_exp2 <- lm(data_exp3$AcceptabilityDelta ~data_exp3$CT.Acceptability_Rev2+data_exp3$ExpectedSpanishAcceptability+
                data_exp3$InteractionSelfPerceived )
summary(lm_exp2)

lm_exp<-lm(data_exp3$FalseConsensusNew ~ data_exp3$AcceptabilityDelta)
summary(lm_exp)
lm_exp<-lm( data_exp3$FalseConsensusNew ~ data_exp3$AcceptabilityDelta+data_exp3$CT.Acceptability_Rev2)
summary(lm_exp)
library(caret)
model1 <- lm(data_exp3$FalseConsensusNew ~data_exp3$AcceptabilityDelta+data_exp3$CT.Acceptability_Rev2)
summary(model1)
car::vif(model1)
QR<-rq(data_exp3$FalseConsensusNew ~ data_exp3$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
sumQR<-summary(QR)
plot(sumQR)
quantile(data_exp3$FalseConsensusNew, c(.2, .4, .6,.8)) 


data_exp3$AcceptabilityDelta<-as.factor(data_exp3$AcceptabilityDelta)
m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
                 , data =data_exp3, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                 , data =data_exp3, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")
data_exp3$AcceptabilityDelta<-data_exp3$AcceptabilityAfterInfo-data_exp3$CT.Acceptability_Rev2


data_exp4<-subset(data, data$CT.Acceptability_Rev2<=3 & data$InfoSpanishAcceptability==19)
hist(data_exp4$FalseConsensus)
hist(data_exp4$FalseConsensusNew)

lm_exp<-lm(data_exp4$AcceptabilityDelta ~ data_exp4$FalseConsensusNew)
summary(lm_exp)
lm_exp2 <- lm(data_exp4$AcceptabilityDelta ~data_exp4$CT.Acceptability_Rev2+data_exp4$ExpectedSpanishAcceptability+
                data_exp4$InteractionSelfPerceived )
summary(lm_exp2)

lm_exp<-lm(data_exp4$FalseConsensusNew ~ data_exp4$AcceptabilityDelta)
summary(lm_exp)
lm_exp<-lm( data_exp4$FalseConsensusNew ~ data_exp4$AcceptabilityDelta+data_exp4$CT.Acceptability_Rev2)
summary(lm_exp)
library(caret)
model1 <- lm(data_exp4$FalseConsensusNew ~data_exp4$AcceptabilityDelta+data_exp4$CT.Acceptability_Rev2)
summary(model1)
car::vif(model1)
QR<-rq(data_exp4$FalseConsensusNew ~ data_exp4$AcceptabilityDelta, tau=seq(0.1, 0.9, by=0.15))
sumQR<-summary(QR)
plot(sumQR)
quantile(data_exp4$FalseConsensusNew, c(.2, .4, .6,.8)) 


data_exp4$AcceptabilityDelta<-as.factor(data_exp4$AcceptabilityDelta)
m_accept <- polr(AcceptabilityDelta ~  FalseConsensus
                 , data =data_exp4, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(AcceptabilityDelta ~  FalseConsensusNew
                 , data =data_exp4, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")
data_exp4$AcceptabilityDelta<-data_exp4$AcceptabilityAfterInfo-data_exp4$CT.Acceptability_Rev2





par(mfrow = c(1, 3) ,mar=c(4,4,1,2))
plot(x=data$FalseConsensus, y=data$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="full sample")
abline(lm(data$AcceptabilityDelta ~ data$FalseConsensus))

plot(x=data_exp1$FalseConsensus, y=data_exp1$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="subsample 1")
abline(lm(data_exp1$AcceptabilityDelta ~ data_exp1$FalseConsensus))

plot(x=data_exp2$FalseConsensus, y=data_exp2$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="subsample 2")
abline(lm(data_exp2$AcceptabilityDelta ~ data_exp2$FalseConsensus))



layout(matrix(c(1,1,2,3,1,1,4,5), nrow = 2, ncol = 4, byrow = TRUE))
hist(data$FalseConsensusNew,
     xlab="False consensus", ylab="Frequency", main="Full sample", cex.main = 2)

hist(data_exp3$FalseConsensusNew,     
     xlab="False consensus", ylab="Frequency",main="CT acceptance and 67% message", cex.main = 1.2)

hist(data_exp2$FalseConsensusNew,
     xlab="False consensus", ylab="Frequency", main="CT non-acceptance and 67% message", cex.main = 1.2)

hist(data_exp1$FalseConsensusNew,  
     xlab="False consensus", ylab="Frequency",main="CT acceptance and 19% message", cex.main = 1.2)

hist(data_exp4$FalseConsensusNew, 
     xlab="False consensus", ylab="Frequency", main="CT non-acceptance and 19% message", cex.main = 1.2)



layout(matrix(c(1,1,2,3,1,1,4,5), nrow = 2, ncol = 4, byrow = TRUE))
plot(x=data$FalseConsensusNew, y=data$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="Full sample", cex.main = 2)
abline(lm(data$AcceptabilityDelta ~ data$FalseConsensusNew))

plot(x=data_exp3$FalseConsensusNew, y=data_exp3$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="CT acceptance and 67% message", cex.main = 1.2)
abline(lm(data_exp3$AcceptabilityDelta ~ data_exp3$FalseConsensusNew))

plot(x=data_exp2$FalseConsensusNew, y=data_exp2$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="CT non-acceptance and 67% message", cex.main = 1.2)
abline(lm(data_exp2$AcceptabilityDelta ~ data_exp2$FalseConsensusNew))

plot(x=data_exp1$FalseConsensusNew, y=data_exp1$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="CT acceptance and 19% message", cex.main = 1.2)
abline(lm(data_exp1$AcceptabilityDelta ~ data_exp1$FalseConsensusNew))

plot(x=data_exp4$FalseConsensusNew, y=data_exp4$AcceptabilityDelta, 
     xlab="False consensus", ylab="Change in acceptability rate",ylim=c(-4,4), main="CT non-acceptance and 19% message", cex.main = 1.2)
abline(lm(data_exp4$AcceptabilityDelta ~ data_exp4$FalseConsensusNew))

# Show the area only
ggplot(data, aes(x=FalseConsensusNew, y=AcceptabilityDelta))+ geom_point() +  
#ggplot(data, aes(x=FalseConsensusNew, y=AcceptabilityDelta) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  ylim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="Full sample",
  x ="False consensus", y = "Change in acceptance rate")


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
p1<-ggplot(data_exp3, aes(x=FalseConsensusNew, y=AcceptabilityDelta) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  ylim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  scale_alpha_continuous(limits=c(0.001,0.02))+ 
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT acceptance and 67% message",
                                                     x ="False consensus \n expected acceptance  - public acceptance", y = "Change in acceptance rate")


p2<-ggplot(data_exp2, aes(x=FalseConsensusNew, y=AcceptabilityDelta) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  ylim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT non-acceptance and 67% message",
                                                     x ="False consensus \n expected non-acceptance  - public non-acceptance", y = "Change in acceptance rate")

p3<-ggplot(data_exp1, aes(x=FalseConsensusNew, y=AcceptabilityDelta) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  ylim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT acceptance and 19% message",
                                                     x ="False consensus \n expected acceptance  - public acceptance", y = "Change in acceptance rate")

p4<-ggplot(data_exp4, aes(x=FalseConsensusNew, y=AcceptabilityDelta) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  ylim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT non-acceptance and 19% message",
                                                     x ="False consensus \n expected non-acceptance  - public non-acceptance", y = "Change in acceptance rate")

grid.arrange(p1, p2, p3, p4, ncol=2)


sp <- ggscatter(data, x = "FalseConsensus", y = "AcceptabilityDelta",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE, # Add confidence interval
                ylim=c(-10,+10))
# Add correlation coefficient
sp + stat_cor(method = "spearman", label.x = 3, label.y = 1.5, cex=3)




p1<-ggplot(data_exp3, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+  ylim(-50,100)+
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  #scale_alpha_continuous(limits=c(0.001,0.02))+ 
  #scale_alpha_continuous(limits=c(0.001,0.02))+ 
  labs(fill = "density", title="CT acceptance and 67% message",
  x ="Change in acceptance rate", y = "False consensus effect")+
  theme(text = element_text(size = 14)) 


p2<-ggplot(data_exp2, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+  ylim(-50,100)+
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT non-acceptance and 67% message",
                                                     x ="Change in acceptance rate", y = "False consensus effect")+
  theme(text = element_text(size = 14)) 

p3<-ggplot(data_exp1, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+    ylim(-50,100)+
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT acceptance and 19% message",
                                                     x ="Change in acceptance rate", y = "False consensus effect")+
  theme(text = element_text(size = 14)) 

p4<-ggplot(data_exp4, aes(x=AcceptabilityDelta, y=FalseConsensusNew) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+  xlim(-4, 4)+ 
  geom_smooth(method = "lm", se = FALSE,colour="red")+   ylim(-50,100)+
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="CT non-acceptance and 19% message",
                                                     x ="Change in acceptance rate", y = "False consensus effect")+
  theme(text = element_text(size = 14)) 

grid.arrange(p1, p2, p3, p4, ncol=2)

