if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","glmnet","MASS","e1071", "readxl","rpart","randomForest","xgboost","dplyr" ) 

setwd("C:/Users/neela/Desktop")

data1 <- read.csv("credit data.csv")
data2 <- read.csv("new applications.csv")

creditdata <- data1
newappli <- data2[,-25]

newappli$default_0 = 0

str(creditdata)
str(newappli)

full <-rbind(creditdata, newappli)

lapply(full[c(3:5,7:12,25)], table)

full_1<-full%>%
  dplyr::select(13:24)

full_1$Zero_Values<-apply(full_1,1,function(x)sum(x == 0))

full_2<-full%>%
  dplyr::select(1:12,25)

full_3<-full_2%>%
  dplyr::select(7:12)

full_3$Positive_Values<-apply(full_3,1,function(x)sum(x>=0))

full_3<-full_3%>%
  dplyr::select(7)

full<-cbind(full_2,full_3,full_1)%>%
  dplyr::select(1:12,27,14:26,13)

full$Flag<-ifelse(full$Positive_Values>=3,"At Risk","Ok")

full$Positive_Values<-as.factor(full$Positive_Values)
full$Zero_Values<-as.factor(full$Zero_Values)
full$Flag<-as.factor(full$Flag)

full$bene <- ifelse(full$PAY_1 == -1 & full$PAY_2 == -1 & full$PAY_3 == -1 & full$PAY_4 == -1 & full$PAY_5 == -1 & full$PAY_6 == -1,1,0)
full$rev <- ifelse(full$PAY_1 == 0 & full$PAY_2 == 0 & full$PAY_3 == 0 & full$PAY_4 == 0 & full$PAY_5 == 0 & full$PAY_6 == 0,1,0)

full$nopay2 <- ifelse(full$BILL_AMT2 != 0 & full$PAY_1 == -2, 1, 0)
full$nopay3 <- ifelse(full$BILL_AMT3 != 0 & full$PAY_2 == -2, 1, 0)
full$nopay4 <- ifelse(full$BILL_AMT4 != 0 & full$PAY_3 == -2, 1, 0)
full$nopay5 <- ifelse(full$BILL_AMT5 != 0 & full$PAY_4 == -2, 1, 0)
full$nopay6 <- ifelse(full$BILL_AMT6 != 0 & full$PAY_5 == -2, 1, 0)

full$completepay2 <- ifelse(full$BILL_AMT2 <= full$PAY_1, 1, 0)
full$completepay3 <- ifelse(full$BILL_AMT3 <= full$PAY_2, 1, 0)
full$completepay4 <- ifelse(full$BILL_AMT4 <= full$PAY_3, 1, 0)
full$completepay5 <- ifelse(full$BILL_AMT5 <= full$PAY_4, 1, 0)
full$completepay6 <- ifelse(full$BILL_AMT6 <= full$PAY_5, 1, 0)

full$pay_full <- full$completepay2 + full$completepay3 + full$completepay4 + full$completepay5 + full$completepay6

full$MARRIAGE <- ifelse(full$MARRIAGE == 0, 3 , full$MARRIAGE)
full$EDUCATION <- ifelse(full$EDUCATION == 0 | full$EDUCATION == 6 , 5 , full$EDUCATION)

full$PAY_1_five <- as.factor(ifelse(full$PAY_1 >= 5 , 1 , 0))
full$PAY_2_five <- as.factor(ifelse(full$PAY_2 >= 5 , 1 , 0))
full$PAY_3_five <- as.factor(ifelse(full$PAY_3 >= 5 , 1 , 0))
full$PAY_4_five <- as.factor(ifelse(full$PAY_4 >= 5 , 1 , 0))
full$PAY_5_five <- as.factor(ifelse(full$PAY_5 >= 5 , 1 , 0))
full$PAY_6_five <- as.factor(ifelse(full$PAY_6 >= 5 , 1 , 0))

full$PAY_1 <- ifelse(full$PAY_1 >= 4 , 4 , full$PAY_1)
full$PAY_2 <- ifelse(full$PAY_2 >= 4 , 4 , full$PAY_2)
full$PAY_3 <- ifelse(full$PAY_3 >= 4 , 4 , full$PAY_3)
full$PAY_4 <- ifelse(full$PAY_4 >= 4 , 4 , full$PAY_4)
full$PAY_5 <- ifelse(full$PAY_5 >= 4 , 4 , full$PAY_5)
full$PAY_6 <- ifelse(full$PAY_6 >= 4 , 4 , full$PAY_6)

full$agegroup1 <- as.factor(ifelse(full$AGE <= 24, 1, 0))
full$agegroup2 <- as.factor(ifelse(full$AGE >= 25 & full$AGE <=34 , 1, 0))
full$agegroup3 <- as.factor(ifelse(full$AGE >= 35 & full$AGE <=44 , 1, 0))
full$agegroup4 <- as.factor(ifelse(full$AGE >= 45 & full$AGE <=54 , 1, 0))
full$agegroup5 <- as.factor(ifelse(full$AGE >= 55, 1, 0))


full$PAY_RATIO_APR<-ifelse(is.nan(full$PAY_AMT1/full$BILL_AMT1),0,
                           ifelse(is.infinite(full$PAY_AMT1/full$BILL_AMT1),0,round(full$PAY_AMT1/full$BILL_AMT1,2)))

full$PAY_RATIO_MAY<-ifelse(is.nan(full$PAY_AMT2/full$BILL_AMT2),0,
                           ifelse(is.infinite(full$PAY_AMT2/full$BILL_AMT2),0,round(full$PAY_AMT2/full$BILL_AMT2,2)))

full$PAY_RATIO_JUNE<-ifelse(is.nan(full$PAY_AMT3/full$BILL_AMT3),0,
                            ifelse(is.infinite(full$PAY_AMT3/full$BILL_AMT3),0,round(full$PAY_AMT3/full$BILL_AMT3,2)))

full$PAY_RATIO_JULY<-ifelse(is.nan(full$PAY_AMT4/full$BILL_AMT4),0,
                            ifelse(is.infinite(full$PAY_AMT4/full$BILL_AMT4),0,round(full$PAY_AMT4/full$BILL_AMT4,2)))

full$PAY_RATIO_AUG<-ifelse(is.nan(full$PAY_AMT5/full$BILL_AMT5),0,
                           ifelse(is.infinite(full$PAY_AMT5/full$BILL_AMT5),0,round(full$PAY_AMT5/full$BILL_AMT5,2)))

full$PAY_RATIO_SEPT<-ifelse(is.nan(full$PAY_AMT6/full$BILL_AMT6),0,
                            ifelse(is.infinite(full$PAY_AMT6/full$BILL_AMT6),0,round(full$PAY_AMT6/full$BILL_AMT6,2)))


full$TwoMonth_BillAmount<-rowSums(full[,15:16])
full$ThreeMonth_BillAmount<-rowSums(full[,15:17])
full$FourMonth_BillAmount<-rowSums(full[,15:18])
full$FiveMonth_BillAmount<-rowSums(full[,15:19])
full$SixMonth_BillAmount<-rowSums(full[,15:20])


full$TwoMonth_PaymentAmount<-rowSums(full[,21:22])
full$ThreeMonth_PaymentAmount<-rowSums(full[,21:23])
full$FourMonth_PaymentAmount<-rowSums(full[,21:24])
full$FiveMonth_PaymentAmount<-rowSums(full[,21:25])
full$SixMonth_PaymentAmount<-rowSums(full[,21:26])

full$OneMonth_BillAmount_Ratio<-ifelse(is.nan(full$BILL_AMT1/full$SixMonth_BillAmount),0,
                                       ifelse(is.infinite(full$BILL_AMT1/full$SixMonth_BillAmount),0,round(full$BILL_AMT1/full$SixMonth_BillAmount,2)))

full$TwoMonth_BillAmount_Ratio<-ifelse(is.nan(full$TwoMonth_BillAmount/full$SixMonth_BillAmount),0,
                                       ifelse(is.infinite(full$TwoMonth_BillAmount/full$SixMonth_BillAmount),0,round(full$TwoMonth_BillAmount/full$SixMonth_BillAmount,2)))

full$ThreeMonth_BillAmount_Ratio<-ifelse(is.nan(full$ThreeMonth_BillAmount/full$SixMonth_BillAmount),0,
                                         ifelse(is.infinite(full$ThreeMonth_BillAmount/full$SixMonth_BillAmount),0,round(full$ThreeMonth_BillAmount/full$SixMonth_BillAmount,2)))

full$FourMonth_BillAmount_Ratio<-ifelse(is.nan(full$FourMonth_BillAmount/full$SixMonth_BillAmount),0,
                                        ifelse(is.infinite(full$FourMonth_BillAmount/full$SixMonth_BillAmount),0,round(full$FourMonth_BillAmount/full$SixMonth_BillAmount,2)))

full$FiveMonth_BillAmount_Ratio<-ifelse(is.nan(full$FiveMonth_BillAmount/full$SixMonth_BillAmount),0,
                                        ifelse(is.infinite(full$FiveMonth_BillAmount/full$SixMonth_BillAmount),0,round(full$FiveMonth_BillAmount/full$SixMonth_BillAmount,2)))



full$OneMonth_PaymentAmount_Ratio<-ifelse(is.nan(full$PAY_AMT1/full$SixMonth_PaymentAmount),0,
                                          ifelse(is.infinite(full$PAY_AMT1/full$SixMonth_PaymentAmount),0,round(full$PAY_AMT1/full$SixMonth_PaymentAmount,2)))

full$TwoMonth_PaymentAmount_Ratio<-ifelse(is.nan(full$TwoMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,
                                          ifelse(is.infinite(full$TwoMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,round(full$TwoMonth_PaymentAmount/full$SixMonth_PaymentAmount,2)))

full$ThreeMonth_PaymentAmount_Ratio<-ifelse(is.nan(full$ThreeMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,
                                            ifelse(is.infinite(full$ThreeMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,round(full$ThreeMonth_PaymentAmount/full$SixMonth_PaymentAmount,2)))

full$FourMonth_PaymentAmount_Ratio<-ifelse(is.nan(full$FourMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,
                                           ifelse(is.infinite(full$FourMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,round(full$FourMonth_PaymentAmount/full$SixMonth_PaymentAmount,2)))

full$FiveMonth_PaymentAmount_Ratio<-ifelse(is.nan(full$FiveMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,
                                           ifelse(is.infinite(full$FiveMonth_PaymentAmount/full$SixMonth_PaymentAmount),0,round(full$FiveMonth_PaymentAmount/full$SixMonth_PaymentAmount,2)))


pay_1_var<-ifelse(as.character(full$PAY_1)>0,1,0)
pay_2_var<-ifelse(as.character(full$PAY_2)>0,1,0)
pay_3_var<-ifelse(as.character(full$PAY_3)>0,1,0)                 
pay_4_var<-ifelse(as.character(full$PAY_4)>0,1,0)
pay_5_var<-ifelse(as.character(full$PAY_5)>0,1,0)
pay_6_var<-ifelse(as.character(full$PAY_6)>0,1,0)

total<-pay_1_var+pay_2_var+pay_3_var+pay_4_var+pay_5_var+pay_6_var
full$def<-ifelse(total>3,1,0)

full$PAY_1 <- as.factor(full$PAY_1)
full$PAY_2 <- as.factor(full$PAY_2)
full$PAY_3 <- as.factor(full$PAY_3)
full$PAY_4 <- as.factor(full$PAY_4)
full$PAY_5 <- as.factor(full$PAY_5)
full$PAY_6 <- as.factor(full$PAY_6)

full$SEX <- as.factor(full$SEX)
full$MARRIAGE <- as.factor(full$MARRIAGE)
full$EDUCATION <- as.factor(full$EDUCATION)
full$default_0 <- as.factor(full$default_0)

full$Limit_Amount1<-round((full$BILL_AMT1/full$LIMIT_BAL)*100,3)
full$Limit_Amount2<-round((full$BILL_AMT2/full$LIMIT_BAL)*100,3)
full$Limit_Amount3<-round((full$BILL_AMT3/full$LIMIT_BAL)*100,3)
full$Limit_Amount4<-round((full$BILL_AMT4/full$LIMIT_BAL)*100,3)
full$Limit_Amount5<-round((full$BILL_AMT5/full$LIMIT_BAL)*100,3)
full$Limit_Amount6<-round((full$BILL_AMT6/full$LIMIT_BAL)*100,3)

full$LIMIT_BAL <- log(full$LIMIT_BAL)

########################################################################################

finalcreditdata <- full[1:24000,-1]
finalnewappli <- full[24001:25000,-1]

#training<-finalcreditdata
set.seed(123)
inTrain <- createDataPartition(y = finalcreditdata$default_0,
                               p = 22999/24000, list = FALSE)
training <- finalcreditdata[ inTrain,]
testing <- finalcreditdata[ -inTrain,]

model_logistic_stepwiseAIC<-glm(default_0 ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
                                  AGE + PAY_1 + PAY_2 + PAY_4 + PAY_5 + PAY_6 + Zero_Values + 
                                  Positive_Values + BILL_AMT2 + PAY_AMT1 + PAY_AMT2 + PAY_AMT5 + 
                                  PAY_AMT6 + rev + nopay3 + completepay4 + completepay5 + completepay6 + 
                                  PAY_2_five + PAY_4_five + PAY_5_five + PAY_RATIO_APR + TwoMonth_BillAmount_Ratio + 
                                  ThreeMonth_BillAmount_Ratio + FiveMonth_BillAmount_Ratio + 
                                  OneMonth_PaymentAmount_Ratio + FiveMonth_PaymentAmount_Ratio + 
                                  def, family = binomial(link = "logit"), data = finalcreditdata)



#gc command in the console 
#summary(model_logistic) 

#model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1)
summary(model_logistic_stepwiseAIC) 

############################################################################################################################################

logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=finalnewappli,type="response") #Predict probabilities
logistic_classification<-rep("1",1000)
logistic_classification[logistic_probabilities<0.2211]="0" 
logistic_classification<-as.factor(logistic_classification)
logistic_classification<-as.data.frame(logistic_classification)
logistic_classification2<-as.data.frame(logistic_classification)
logistic_classification$logistic_classification<-ifelse(logistic_classification$logistic_classification==0,1,0)

write.csv(logistic_classification,"Predictions_Logistic_Model1.csv",row.names = F)


#logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=finalnewappli,type="response") #Predict probabilities
finalnewappli2<-finalnewappli
finalnewappli2$default_prob<-logistic_probabilities
finalnewappli2<-finalnewappli2%>%
  mutate(give_loan=ifelse(default_prob>0.2211,0,1))

write.csv(finalnewappli2,"Issue Credit_Logistic_Model1.csv",row.names = F)

############################################################################################################################################

#Model 1
glm(formula = default_0 ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
      AGE + PAY_1 + PAY_2 + PAY_4 + PAY_5 + PAY_6 + Zero_Values + 
      Positive_Values + BILL_AMT2 + PAY_AMT1 + PAY_AMT2 + PAY_AMT5 + 
      PAY_AMT6 + rev + nopay3 + completepay4 + completepay5 + completepay6 + 
      PAY_2_five + PAY_4_five + PAY_5_five + PAY_RATIO_APR + TwoMonth_BillAmount_Ratio + 
      ThreeMonth_BillAmount_Ratio + FiveMonth_BillAmount_Ratio + 
      OneMonth_PaymentAmount_Ratio + FiveMonth_PaymentAmount_Ratio + 
      def, family = binomial(link = "logit"), data = training)

#Model 2
glm(formula = default_0 ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
      AGE + PAY_1 + PAY_2 + PAY_4 + PAY_5 + PAY_6 + Zero_Values + 
      Positive_Values + BILL_AMT2 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + 
      PAY_AMT2 + PAY_AMT4 + PAY_AMT6 + rev + nopay3 + completepay4 + 
      completepay5 + PAY_2_five + PAY_4_five + PAY_5_five + agegroup1 + 
      PAY_RATIO_APR + TwoMonth_BillAmount_Ratio + ThreeMonth_BillAmount_Ratio + 
      FiveMonth_BillAmount_Ratio + OneMonth_PaymentAmount_Ratio + 
      ThreeMonth_PaymentAmount_Ratio + FourMonth_PaymentAmount_Ratio + 
      FiveMonth_PaymentAmount_Ratio + def + Limit_Amount2 + Limit_Amount5 + 
      Limit_Amount6, family = binomial(link = "logit"), data = training)



