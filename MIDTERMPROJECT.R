mydata<-read.csv("E:/Dataset.csv",header=TRUE,sep=",")

numberOfColumn<-ncol(mydata)
numberOfColumn

numberOfRow<-nrow(mydata)
numberOfRow

names(mydata)

str(mydata)

class(mydata$age)

mydata$age

mydata$age<-as.integer(mydata$age)

class(mydata$age)

mydata$age

class(mydata$anaemia)

mydata$anaemia<-as.factor(mydata$anaemia)

class(mydata$anaemia)

levels(mydata$anaemia)

class(mydata$creatinine_phosphokinase)

mydata$creatinine_phosphokinase

notNumericRow <- which(is.na(as.numeric(mydata$creatinine_phosphokinase)))

mydata[notNumericRow,"creatinine_phosphokinase"]

class(mydata$creatinine_phosphokinase)

mydata$creatinine_phosphokinase<-as.integer(as.character(mydata$creatinine_phosphokinase))

class(mydata$creatinine_phosphokinase)

class(mydata$diabetes)

unique(mydata$diabetes)

mydata$diabetes<-as.factor(mydata$diabetes)

str(mydata$diabetes)

class(mydata$ejection_fraction)

class(mydata$high_blood_pressure)

unique(mydata$high_blood_pressure)

mydata$high_blood_pressure<-as.factor(mydata$high_blood_pressure)

str(mydata$high_blood_pressure)

str(mydata$platelets)

str(mydata$serum_creatinine)

class(mydata$serum_sodium)

class(mydata$sex)

unique(mydata$sex)

mydata$sex<-factor((mydata$sex),levels = c("Male","Female"))

unique(mydata$sex)

str(mydata$sex)

mydata$sex<-factor(mydata$sex,levels=c("Male","Female"),labels=c(1,2))
str(mydata$sex)

class(mydata$smoking)

unique(mydata$smoking)

mydata$smoking<-as.factor(mydata$smoking)
str(mydata$smoking)

str(mydata$time)

str(mydata$DEATH_EVENT)
unique(mydata$DEATH_EVENT)

mydata$DEATH_EVENT<-as.factor(mydata$DEATH_EVENT)
str(mydata$DEATH_EVENT)

library(ggplot2)
boxplot(mydata$age)

findOutliers<-boxplot.stats(mydata$age)$out
findOutliers

findOutliersIndex<-which(mydata$age %in% c(findOutliers))
findOutliersIndex

notOutliers<-!mydata$age %in% findOutliers
withoutOutliersData<-mydata[notOutliers, ]
boxplot(withoutOutliersData$age)

myNewData<-withoutOutliersData

is.na(myNewData$age)

sum(is.na(myNewData$age))

which(is.na(myNewData$age))

myNewData$age[which(is.na(myNewData$age))]=round(mean(myNewData$age,na.rm = TRUE))
sum(is.na(myNewData$age))

str(myNewData$anaemia)
sum(is.na(myNewData$anaemia))

withoutNAValueTocountMode<-na.omit(myNewData$anaemia)
mode<-names(which.max(table(withoutNAValueTocountMode)))
mode
myNewData$anaemia[is.na(myNewData$anaemia)]<-mode
sum(is.na(myNewData$anaemia))

sum(is.na(myNewData$creatinine_phosphokinase))
hist(myNewData$creatinine_phosphokinase)
meanValue<-mean(myNewData$creatinine_phosphokinase,na.rm = TRUE)
meanValue
medianValue<-median(myNewData$creatinine_phosphokinase,na.rm = TRUE)
medianValue
nrow(myNewData)
myNewData<-myNewData[!is.na(myNewData$creatinine_phosphokinase),]
sum(is.na(myNewData$creatinine_phosphokinase))
nrow(myNewData)

is.na(myNewData$diabetes)
sum(is.na(myNewData$diabetes))
which(is.na(myNewData$diabetes))
withoutNAvalueForDiabetes<-na.omit(myNewData$diabetes)
modeForDiabetes<-names(which.max(table(withoutNAvalueForDiabetes)))
modeForDiabetes
myNewData$diabetes[is.na(myNewData$diabetes)]<-modeForDiabetes
sum(is.na(myNewData$diabetes))

sum(is.na(myNewData$ejection_fraction))

sum(is.na(myNewData$high_blood_pressure))

sum(is.na(myNewData$platelets))
which(is.na(myNewData$platelets))
boxplot(myNewData$platelets)
meanValueOfPlatelets<-mean(myNewData$platelets,na.rm = TRUE)
meanValueOfPlatelets
medianValueOfPlatelets<-median(myNewData$platelets,na.rm = TRUE)
medianValueOfPlatelets
myNewData$platelets[is.na(myNewData$platelets)]<-medianValueOfPlatelets
sum(is.na(myNewData$platelets))

sum(is.na(myNewData$serum_creatinine))

sum(is.na(myNewData$serum_sodium))

sum(is.na(myNewData$sex))
which(is.na(myNewData$sex))
withoutNAvalueForGender<-na.omit(myNewData$sex)
modeForGender<-names(which.max(table(withoutNAvalueForGender)))
modeForGender
myNewData$sex[is.na(myNewData$sex)]<-modeForGender
sum(is.na(myNewData$sex))

sum(is.na(myNewData$smoking))
which(is.na(myNewData$smoking))
withoutNAvalueForSmoking<-na.omit(myNewData$smoking)
modeForSmoking<-names(which.max(table(withoutNAvalueForSmoking)))
modeForSmoking
myNewData$smoking[is.na(myNewData$smoking)]<-modeForSmoking
sum(is.na(myNewData$smoking))

sum(is.na(myNewData$time))

sum(is.na(myNewData$DEATH_EVENT))

recoveredMissingData<-myNewData
removeOutlierData<-myNewData

boxplot(removeOutlierData$age)

boxplot(removeOutlierData$creatinine_phosphokinase)
summary(removeOutlierData$creatinine_phosphokinase)
findOutliersOfCreatinine<-boxplot.stats(removeOutlierData$creatinine_phosphokinase)$out
findOutliersOfCreatinine

boxplot(removeOutlierData$ejection_fraction)
findOutlierOfejection<-boxplot.stats(removeOutlierData$ejection_fraction)$out
findOutlierOfejection
summary(removeOutlierData$ejection_fraction)
upperBound<-quantile(removeOutlierData$ejection_fraction,0.95)
upperBound
removeOutlierData$ejection_fraction[removeOutlierData$ejection_fraction %in% findOutlierOfejection]<-upperBound
boxplot(removeOutlierData$ejection_fraction)

boxplot(removeOutlierData$platelets)
findOutlierOfplatelets<-boxplot.stats(removeOutlierData$platelets)$out
findOutlierOfplatelets

boxplot(removeOutlierData$serum_creatinine)
findOutlierOfserumcreatinine<-boxplot.stats(removeOutlierData$serum_creatinine)$out
findOutlierOfserumcreatinine

boxplot(removeOutlierData$serum_sodium)
summary(removeOutlierData$serum_sodium)

myPreparedData<-removeOutlierData

normalization<-function(x){
  
  return((x-min(x))/(max(x)-min(x)))
  
}
myPreparedData$platelets<-normalization(myPreparedData$platelets)
myFinalData<-myPreparedData
View(myFinalData)
