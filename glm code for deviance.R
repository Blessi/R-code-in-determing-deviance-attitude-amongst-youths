install.packages("lmtest")
install.packages("aod")
install.packages("rms")
install.packages("car")

library(MASS)
library(dplyr)
require(lmtest)
library(car)
#glm
library(glm2)

ose<-read.csv('ose.csv',header=TRUE,sep=(","))
mdata<- ose[complete.cases(ose), ] # create copy
#recode value 1:5 to 0,1
mdata$TT2<-recode(mdata$TT2,"1='0';2:5='1'")	
mdata$TT3<-recode(mdata$TT3,"1='0';2:5='1'")	
mdata$BV2<-recode(mdata$BV2,"1='0';2:5='1'")	
mdata$BV3<-recode(mdata$BV3,"1='0';2:5='1'")	
mdata$FW2<-recode(mdata$FW2,"1='0';2:5='1'")	
mdata$FW3<-recode(mdata$FW3,"1='0';2:5='1'")	
mdata$DR2<-recode(mdata$DR2,"1='0';2:5='1'")	
mdata$DR3<-recode(mdata$DR3,"1='0';2:5='1'")	
mdata$SM2<-recode(mdata$SM2,"1='0';2:5='1'")	
mdata$SM3<-recode(mdata$SM3,"1='0';2:5='1'")	
mdata$SF2<-recode(mdata$SF2,"1='0';2:5='1'")	
mdata$SF3<-recode(mdata$SF3,"1='0';2:5='1'")	
mdata$SC2<-recode(mdata$SC2,"1='0';2:5='1'")	
mdata$SC3<-recode(mdata$SC3,"1='0';2:5='1'")	
mdata$RP2<-recode(mdata$RP2,"1='0';2:5='1'")	
mdata$RP3<-recode(mdata$RP3,"1='0';2:5='1'")	
mdata$FTC<-recode(mdata$FTC,"1='0';2:5='1'")
mdata$MTC<-recode(mdata$MTC,"1='0';2:5='1'")
mdata$TTS<-recode(mdata$TTS,"1='0';2:5='1'")
mdata$ES28<-recode(mdata$ES28,"1='0';2:5='1'")
mdata$TGC29<-recode(mdata$TGC29,"1='0';2:5='1'")
mdata$CAG30<-recode(mdata$CAG30,"1='0';2:5='1'")
mdata$RE31<-recode(mdata$RE31,"1='0';2:5='1'")
mdata$ECA32<-recode(mdata$ECA32,"1='0';2:5='1'")
mdata$APCS<-recode(mdata$APCS,"1='0';2:5='1'")
mdata$TH34<-recode(mdata$TH34,"1='0';2:5='1'")
mdata$RF35<-recode(mdata$RF35,"1='0';2:5='1'")
mdata$RM36<-recode(mdata$RM36,"1='0';2:5='1'")
mdata$FU37<-recode(mdata$FU37,"1='0';2:5='1'")
mdata$MU38<-recode(mdata$MU38,"1='0';2:5='1'")
mdata$CF39<-recode(mdata$CF39,"1='0';2:5='1'")
mdata$CM40<-recode(mdata$CM40,"1='0';2:5='1'")
mdata$EDU41<-recode(mdata$EDU41,"1='0';2:5='1'")
mdata$EDU42<-recode(mdata$EDU42,"1='0';2:5='1'")

head(mdata)
#to test our model's performance

#convert to factors
mdata[cols]<-lapply(mdata[cols],factor) #as.factor could also be used
sapply(mdata,class)
typeof(mdata)   
class(mdata)
mdata[,cols]<-factor(mdata[,cols])
typeof(mdata)
class(mdata)
str(mdata)

#split to training and test sets
'%ni%'<-Negate('%in%')#define 'not in'func
options(scipen=999)#prevents printing scientific notations

##Prep training and test sets
library(caret)
#pilfering
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)

#Building models
model0<-glm2(formula=TT1~1,data=trainData,family='binomial')
summary(model0)
model1<-glm2(formula=TT1~TT2+TT3+gender+FTC+TTS+CAG30+RE31+EDU41,data=trainData,family='binomial')
summary(model1)

#Likelihood ratio test for MLE method 
lrtest (model0, model1)

#coefficient of model
coef(model1)
exp(cbind(coef(model1), confint(model1)))
#
library(aod)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=8)

anova(model1,test="Chisq")

model1$deviance-model0$deviance
par(mfrow=c(2,2))
plot(model1,)

#prediction 
pred <- predict(model1,newdata = testData, type = "response")
pred
pred <- predict(model1, newdata = trainData, type = "response")
pred
confmatrix<-table(actual_value=trainData$TT1,predicted_value=pred>0.5)
confmatrix
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)


#DRINKING
set.seed(10)
trainDataIndex<-createDataPartition(mdata$DR1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$DR1)
#Building models
model0<-glm2(formula=DR1~1,data=trainData,family='binomial')
summary(model0)
model2<-glm2(formula=DR1~DR2+DR3+gender+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model2)
str(mdata)
model2<-glm2(formula=DR1~DR2+DR3+gender+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model2)

#Likelihood ratio test for MLE method 
lrtest (model0, model2)
#
library(aod)
wald.test(b=coef(model2), Sigma=vcov(model2), Terms=8)

anova(model2,test="Chisq")

model1$deviance-model0$deviance
par(mfrow=c(2,2))
plot(model2)

#prediction 
pred <- predict(model2,newdata = testData, type = "response")
pred
pred <- predict(model2, newdata = trainData, type = "response")
pred
coef(model1)
exp(cbind(coef(model2), confint(model1)))
confmatrix<-table(actual_value=trainData$DR1,predicted_value=pred>0.5)
confmatrix

model3<-glm2(formula=FW1~FW2+FW3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=ose1,family=binomial(link="logit"))
summary(model3)
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$FW1)
#Building models
model0<-glm2(formula=TT1~1,data=trainData,family='binomial')
summary(model0)
model3<-glm2(formula=FW1~FW2+FW3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=ose1,family=binomial(link="logit"))
summary(model3)
model3<-glm2(formula=FW1~FW2+FW3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=ose1,family=binomial(link="logit"))
summary(model3)

#Likelihood ratio test for MLE method 
lrtest (model0, model3)
#
library(aod)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=2)

anova(model1,test="Chisq")

model1$deviance-model0$deviance
plot(model1)

#prediction 
pred <- predict(model3,newdata = testData, type = "response")
pred
pred <- predict(model3, newdata = trainData, type = "response")
pred
coef(model3)
exp(cbind(coef(model3), confint(model3)))
confmatrix<-table(actual_value=trainData$TT1,predicted_value=pred>0.5)
confmatrix

model4<-glm2(formula=SM1~SM2+SM3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=ose1,family=binomial(link="logit"))
summary(model4)
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)
#Building models
model0<-glm2(formula=TT1~1,data=trainData,family='binomial')
summary(model0)
model1<-glm2(formula=TT1~TT2+TT3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model1)

#Likelihood ratio test for MLE method 
lrtest (model0, model1)
#
library(aod)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=2)
install.packages("rms")

anova(model1,test="Chisq")

model1$deviance-model0$deviance
plot(model1)

#prediction 
pred <- predict(model1,newdata = testData, type = "response")
pred
pred <- predict(model1, newdata = trainData, type = "response")
pred
coef(model1)
exp(cbind(coef(model1), confint(model1)))
confmatrix<-table(actual_value=trainData$TT1,predicted_value=pred>0.5)
confmatrix

model5<-glm2(formula=DR1~DR2+DR3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=train,family=binomial(link="logit"))
summary(model5)
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)
#Building models
model0<-glm2(formula=TT1~1,data=trainData,family='binomial')
summary(model0)
model1<-glm2(formula=TT1~TT2+TT3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model1)

#Likelihood ratio test for MLE method 
lrtest (model0, model1)
#
library(aod)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=2)
install.packages("rms")

anova(model1,test="Chisq")

model1$deviance-model0$deviance
plot(model1)

#prediction 
pred <- predict(model1,newdata = testData, type = "response")
pred
pred <- predict(model1, newdata = trainData, type = "response")
pred
coef(model1)
exp(cbind(coef(model1), confint(model1)))
confmatrix<-table(actual_value=trainData$TT1,predicted_value=pred>0.5)
confmatrix

model6<-glm2(formula=SF1~SF2+SF3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=train,family=binomial(link="logit"))
summary(model6)
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)
#Building models
model0<-glm2(formula=TT1~1,data=trainData,family='binomial')
summary(model0)
model1<-glm2(formula=TT1~TT2+TT3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model1)

#Likelihood ratio test for MLE method 
lrtest (model0, model1)
#
library(aod)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=2)
install.packages("rms")

anova(model1,test="Chisq")

model1$deviance-model0$deviance
plot(model1)

#prediction 
pred <- predict(model1,newdata = testData, type = "response")
pred
pred <- predict(model1, newdata = trainData, type = "response")
pred
coef(model1)
exp(cbind(coef(model1), confint(model1)))
confmatrix<-table(actual_value=trainData$TT1,predicted_value=pred>0.5)
confmatrix

model7<-glm2(formula=SC1~SC2+SC3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=train,family=binomial(link="logit"))
summary(model7)
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)
#Building models
model0<-glm2(formula=TT1~1,data=trainData,family='binomial')
summary(model0)
model1<-glm2(formula=TT1~TT2+TT3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model1)

#Likelihood ratio test for MLE method 
lrtest (model0, model1)
#
library(aod)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=2)
install.packages("rms")

anova(model1,test="Chisq")

model1$deviance-model0$deviance
plot(model1)

#prediction 
pred <- predict(model1,newdata = testData, type = "response")
pred
pred <- predict(model1, newdata = trainData, type = "response")
pred
coef(model1)
exp(cbind(coef(model1), confint(model1)))
confmatrix<-table(actual_value=trainData$TT1,predicted_value=pred>0.5)
confmatrix

model8<-glm2(formula=RP1~RP2+RP3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=train,family=binomial(link="logit"))
summary(model8)
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)
#Building models
model0<-glm2(formula=TT1~1,data=trainData,family='binomial')
summary(model0)
model1<-glm2(formula=TT1~TT2+TT3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model1)

#Likelihood ratio test for MLE method 
lrtest (model0, model1)
#
library(aod)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=2)
install.packages("rms")

anova(model1,test="Chisq")

model1$deviance-model0$deviance
plot(model1)

#prediction 
pred <- predict(model1,newdata = testData, type = "response")
pred
pred <- predict(model1, newdata = trainData, type = "response")
pred
coef(model1)
exp(cbind(coef(model1), confint(model1)))
confmatrix<-table(actual_value=trainData$TT1,predicted_value=pred>0.5)
confmatrix





library(caret)
#split to training and test sets
'%ni%'<-Negate('%in%')#define 'not in'func
options(scipen=999)#prevents printing scientific notations
##Prep training and test sets
set.seed(10)
trainDataIndex<-createDataPartition(mdata$gender,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)
trainData$TT1<-as.factor(trainData$TT1)

#convert to factors
ose[cols] <- lapply(ose[cols], factor) 
# to down sample
set.seed(10)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "mdata$gender"],
y = trainData$gender)
table(down_train$gender)

up_train<-upSample(x=trainData[,colnames(trainData)%ni%"mdata$gender"],
           y=trainData$gender)
table(up_train$gender)

#Build a Logistic Model

#transforming TT1

pred<-predict(model1,newdata=testData,type="response")
#get probability cutoff
y_pred_num<-ifelse(pred>0.5,1,0)
y_pred<-factor(y_pred_num,levels=c(0,1))
y_act<-testData$gender
#compute accuracy
mean(y_pred==y_act) #94+%
mdata<- ddata[complete.cases(ddata), ] # create copy
#split to training and test sets
'%ni%'<-Negate('%in%')#define 'not in'func
options(scipen=999)#prevents printing scientific notations

##Prep training and test sets
library(caret)
set.seed(10)
trainDataIndex<-createDataPartition(mdata$TT1,p=0.7,list=F)
trainData<-mdata[trainDataIndex,]
testData<-mdata[-trainDataIndex,]
#table
table(trainData$TT1)
pred <- predict(model1, newdata = testData, type = "response")
pred
# to down sample
set.seed(10)
trainData$TT1<-as.factor(trainData$TT1)

up_train <- upSample(x = trainData[, colnames(trainData) %ni% "mdata$TT1"],
y = trainData$TT1)
table(up_train$TT1)
trainData$gender<-as.factor(trainData$gender)
up_train<-upSample(x=trainData[,colnames(trainData)%ni%"mdata$gender"],
           y=trainData$gender)
table(up_train$gender)


#barplot
barplot(table(ddata$gender,ddata$TT1),beside=T,
 cex.names=0.7,legend.text=c("Female","Male"),
 args.legend=list(x=12,y=25,cex=0.8),
 col=c("pink","light blue"))

 boxplot(TT1~sex,data=dataframe,names=c("Female","Male"),
 ylab="Sum of scores")
#
install.packages("caTools")
library(caTools)
ose<-read.csv('ose.csv',header=TRUE,sep=(","))	
head(ose)
ose1<- ddata[complete.cases(ose), ] # create copy

library(caTools)
split<-sample.split(ose1,SplitRatio=0.8)
split
train<-subset(ose1,split=="TRUE")
test<-subset(ose1,split=="FALSE")
str(ose1)

# changing to factor
ose1$TT1<-as.factor(ose1$TT1)
ose1$BV1<-as.factor(ose1$BV1)
ose1$FW1<-as.factor(ose1$FW1)
ose1$SM1<-as.factor(ose1$SM1)
ose1$DR1<-as.factor(ose1$DR1)
ose1$SF1<-as.factor(ose1$SF1)
ose1$SC1<-as.factor(ose1$SC1)
ose1$RP1<-as.factor(ose1$RP1)
#
ose1$TT2<-as.factor(ose1$TT2)
ose1$BV2<-as.factor(ose1$BV2)
ose1$FW2<-as.factor(ose1$FW2)
ose1$SM2<-as.factor(ose1$SM2)
ose1$DR2<-as.factor(ose1$DR2)
ose1$SF2<-as.factor(ose1$SF2)
ose1$SC2<-as.factor(ose1$SC2)
ose1$RP2<-as.factor(ose1$RP2)
#
ose1$TT3<-as.factor(ose1$TT3)
ose1$BV3<-as.factor(ose1$BV3)
ose1$FW3<-as.factor(ose1$FW3)
ose1$SM3<-as.factor(ose1$SM3)
ose1$DR3<-as.factor(ose1$DR3)
ose1$SF3<-as.factor(ose1$SF3)
ose1$SC3<-as.factor(ose1$SC3)
ose1$RP3<-as.factor(ose1$RP3)
ose1$gender<-as.factor(ose1$gender)
ose1$FTC<-as.factor(ose1$FTC)
ose1$MTC<-as.factor(ose1$MTC)
ose1$TTS<-as.factor(ose1$TTS)
ose1$ES28<-as.factor(ose1$ES28)
ose1$TGC29<-as.factor(ose1$TGC29)
ose1$CAG30<-as.factor(ose1$CAG30)
ose1$RE31<-as.factor(ose1$RE31)
ose1$ECA32<-as.factor(ose1$ECA32)
ose1$APCS<-as.factor(ose1$APCS)
ose1$TH34<-as.factor(ose1$TH34)
ose1$RF35<-as.factor(ose1$RF35)
ose1$RM36<-as.factor(ose1$RM36)
ose1$FU37<-as.factor(ose1$FU37)
ose1$MU38<-as.factor(ose1$MU38)
ose1$CF39<-as.factor(ose1$CF39)
ose1$CM40<-as.factor(ose1$CM40)
ose1$EDU41<-as.factor(ose1$EDU41)
ose1$EDU42<-as.factor(ose1$EDU42)
ose1$SUB<-as.factor(ose1$SUB)


model1<-glm2(formula=ose1$TT1~ose1$TT2+ose1$TT3+ose1$gender+,data=ose1,family=binomial(link="logit"))
summary(model1)
glm2(formula=DR1~DR2+DR3,data=ddata,family=binomial(link="logit"))->model1
summary(model1)
glm2(formula=FW1~FW2+FW3,data=ddata,family=binomial(link="logit"))->model1
summary(model1)
glm2(formula=SM1~SM2+SM3+gender+age,data=ose,family=binomial(link="logit"))->model1
summary(model1)
glm2(formula=DR1~DR2+DR3,data=ddata,family=binomial(link="logit"))->model1
summary(model1)
glm2(formula=SF1~SF2+SF3,data=ddata,family=binomial(link="logit"))->model1
summary(model1)
glm2(formula=SC1~SC2+SC3+gender+age,data=train,family=binomial(link="logit"))->model1
summary(model1)
glm2(formula=RP1~RP2+RP3,data=ddata,family=poisson(link="log"))->model1
summary(model1)

pred <- predict(model1,newdata = test, type = "response")
pred
pred <- predict(model1, newdata = train, type = "response")
pred

#odd ratio
require(MASS)
(coef(model1))
exp(cbind(coef(model1)))
exp(cbind(coef(model1), confint(model1)))


#confusion matrix
confmatrix<-table(actual_value=train$SC1,predicted_value=pred>0.5)
confmatrix
confmatrix[[1,1]]+confmatrix[[2,2]]/sum(confmatrix)


#plot graph
head(ddata)
glm2(formula=TT1~FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40,data=ddata,family=poisson(link="log"))->model1
summary(model1)

model1<-glm2(formula=TT1~TT2+TT3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40,data=ddata,family=poisson(link="log"))
summary(model1)
model1<-glm2(formula=TT1~TH34,data=ddata,family=poisson(link="log"))
summary(model1)
model1<-glm2(formula=TT1~family.size+TH34,data=ddata,family=poisson(link="log"))
summary(model1)


model1<-glm2(formula=TT1~gender+EDST+PRO45+INCOME+MATT+EDU41+EDU42,data=ddata,family=poisson(link="log"))
summary(model1)
glm2(formula=TT1~RM35,data=ddata,family=poisson(link="log"))->model1
summary(model1)
glm2(formula=TT1~MATT,data=ddata,family=poisson(link="log"))->model1
summary(model1)
glm2(formula=TT1~INCOME,data=ddata,family=poisson(link="log"))->model1
summary(model1)
glm2(formula=family.size~TT1,data=ddata,family=poisson(link="log"))->model1
summary(model1)




#PARENTING 
model1<-glm2(formula=TT1~TT2+TT3+RF35+RM36+FU37+MU38+CF39+CM40,data=ddata,family=poisson(link="log"))
summary(model1)
+RF35+RM36+FU37+MU38+CF39+CM40



#code gender from 1,2 to 0,1
gender<-c(0,1)
ddata$gender<-as.numeric(gender)
head(ddata$gender)

res<-predict(model1,data.frame(TT2=c(2:5),TT3=c(2:5),gender=c(1,2),type="response"))
res<-predict(model1,data=ddata,type="response")
confmatrix<-table(actual_value=ddata$TT1,predictive_value=res>0.5)

glm2(formula=TT1~TT2+TT3+gender,data=ddata,family=binomial(link="logit"))->model1
summary(model1)
glm2(formula=FW1~FW2+FW3,data=ddata,family=binomial(link="logit"))->model1
summary(model1)
head(ddata)
ddata$BV1=as.numeric(ddata$BV1)
ddata$BV2=as.numeric(ddata$BV2)
ddata$BV3=as.numeric(ddata$BV3)
glm2(formula=BV1~BV2+BV3,data=ddata,family=poisson(link="log"))->model1
summary(model1)
ddata$SM1=as.numeric(ddata$SM1)
ddata$SM2=as.numeric(ddata$SM2)
ddata$SM3=as.numeric(ddata$SM3)

ddata$SC1=as.numeric(ddata$SC1)
ddata$SC2=as.numeric(ddata$SC2)
ddata$SC3=as.numeric(ddata$SC3)

ddata$SF1=as.numeric(ddata$SF1)
ddata$SF2=as.numeric(ddata$SF2)
ddata$SF3=as.numeric(ddata$SF3)

ddata$RP1=as.numeric(ddata$RP1)
ddata$RP2=as.numeric(ddata$RP2)
ddata$RP3=as.numeric(ddata$RP3)
ddata<-read.csv('group.csv',header=TRUE,sep=(","))	
head(ddata)

model1<-glm2(formula=TT1~TT2+TT3+gender+age+FTC+MTC+TTS+ES28+TGC29+CAG30+RE31+ECA32+APCS+TH34+RF35+RM36+FU37+MU38+CF39+CM40+EDU41+EDU42,data=trainData,family='binomial')
summary(model1)

#predict gives the predicted value in terms of logits
plot.dat <- data.frame(prob = menarche$Menarche/menarche$Total,
                       age = menarche$Age,
                       fit = predict(m, menarche))
#convert those logit values to probabilities
plot.dat$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))

library(ggplot2)
ggplot(plot.dat, aes(x=age, y=prob)) + 
  geom_point() +
  geom_line(aes(x=age, y=fit_prob))

