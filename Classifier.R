library(dplyr)
library(randomForest)
library(ROCR)
library(tidyr)
library(stats)
library(doParallel)
library(foreach)
library(memisc)
library(foreign)
library(Hmisc)
library(data.table)
library(ggmap)
library(caret)
library(maptools) 
library(gpclib)
library(sp)
library(rgdal)
library(readr)
library(tidyverse)
library(lubridate)


sqf.data<-read.csv(file="sqf_08_16.csv")
View(sqf.data)
#Filter the data to include on CPW stops
sqf_filter <- sqf.data %>% filter(suspected.crime=='cpw')
#rm(sqf.data)
sqf <- sqf_filter %>% select(id,year,found.weapon,precinct,location.housing,stopped.bc.object,stopped.bc.desc,stopped.bc.casing,stopped.bc.lookout,stopped.bc.clothing,stopped.bc.drugs,stopped.bc.furtive,stopped.bc.violent,stopped.bc.bulge,stopped.bc.other,additional.associating,additional.direction,additional.evasive,additional.highcrime,additional.investigation,additional.other,additional.proximity,additional.report,additional.sights,additional.time,suspect.age,suspect.sex,suspect.build,suspect.height,suspect.weight,inside,radio.run,observation.period,day,month,time.period,officer.uniform)
View(sqf)
rm(sqf_filter)

#A
# Convert variable types as necessary for later random forest modeling
sqf$found.weapon = as.factor(sqf$found.weapon)
sqf$suspect.build = as.factor(sqf$suspect.build)
sqf$suspect.sex = as.factor(sqf$suspect.sex)
sqf$location.housing = as.factor(sqf$location.housing)
sqf$stopped.bc.object= as.factor(sqf$stopped.bc.object)
sqf$stopped.bc.desc= as.factor(sqf$stopped.bc.desc)
sqf$stopped.bc.casing = as.factor(sqf$stopped.bc.casing)
sqf$stopped.bc.lookout= as.factor(sqf$stopped.bc.lookout)
sqf$stopped.bc.clothing = as.factor(sqf$stopped.bc.clothing)
sqf$stopped.bc.drugs= as.factor(sqf$stopped.bc.drugs)
sqf$stopped.bc.furtive= as.factor(sqf$stopped.bc.furtive)
sqf$stopped.bc.violent= as.factor(sqf$stopped.bc.violent)
sqf$stopped.bc.bulge= as.factor(sqf$stopped.bc.bulge)
sqf$stopped.bc.other= as.factor(sqf$stopped.bc.other)
sqf$additional.associating = as.factor(sqf$additional.associating)
sqf$additional.direction = as.factor(sqf$additional.direction)
sqf$additional.evasive = as.factor(sqf$additional.evasive)
sqf$additional.highcrime = as.factor(sqf$additional.highcrime)
sqf$additional.investigation = as.factor(sqf$additional.investigation)
sqf$additional.other = as.factor(sqf$additional.other)
sqf$additional.proximity = as.factor(sqf$additional.proximity)
sqf$additional.report = as.factor(sqf$additional.report)
sqf$additional.sights = as.factor(sqf$additional.sights)
sqf$additional.time = as.factor(sqf$additional.time)
sqf$inside = as.factor(sqf$inside)
sqf$radio.run = as.factor(sqf$radio.run)
sqf$officer.uniform = as.factor(sqf$officer.uniform)
sqf$day = as.factor(sqf$day)
sqf$month = as.factor(sqf$month)
sqf$time.period = as.factor(sqf$time.period)
sqf$precinct = as.factor(sqf$precinct)

#Generate binary dummy variables of precincts
mydf <- data.frame(precinct= sqf$precinct,
                   target = sqf$found.weapon)
dummyMat <- model.matrix(target ~ precinct, mydf,contrasts.arg = list(precinct = contrasts(mydf$precinct, contrasts = F))) # set contrasts.arg to keep all levels
View(dummyMat)
dim(dummyMat)
dummyMat <- dummyMat[,c(2:ncol(dummyMat))] # removing intercept column
#Combine dummy variables of precincts to sqf dataset
sqf<- cbind(sqf, dummyMat) 
sqf<-sqf %>% select(-precinct)
for (i in 36:112){
  sqf[,i] <- as.factor(sqf[,i])
}
rm(dummyMat)
rm(mydf)
write.csv(sqf,file="C:/Chanel/NYU/Messy Data and Machine Learning, Section 001 Resources/HW/HW5/sqf.csv")

sqf<-read.csv("sqf.csv")
View(sqf)
sqf<-sqf[,-1]
# standardize function
standardize <- function(x) {
  x.std <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  x.std
}
sqf<- sqf %>% mutate(suspect.height=standardize(suspect.height),
                     suspect.weight=standardize(suspect.weight),
                     suspect.age=standardize(suspect.age),
                     observation.period=standardize(observation.period))
#Restrict to years 2013-2014
sqf_13_14<- sqf %>% filter(year==2013|year==2014) #59398 obs
#Restrict to complete cases
complete_sqf_13_14<-complete.cases(sqf_13_14)
sqf_13_14<-sqf_13_14[complete_sqf_13_14,] #58764 obs
# Randomly shuffling and spliting into 50% train and 50% test 
set.seed(1111)
shuff <- sqf_13_14 %>% sample_frac(1,replace=F)
ind <- 1:ceiling(nrow(shuff)*0.5)
train_half<- shuff[ind,] #29382 obs
test_half <- shuff[-ind,] #29382 obs
#Restrict to year 2015
test_later<- sqf %>% filter(year==2015) #6812 obs
#Restrict to complete cases
complete_test_later<-complete.cases(test_later)
test_later<-test_later[complete_test_later,] #6741 obs
#Remove the stop id and year columns
train_half<- train_half %>% select(-id,-year)
test_half<- test_half %>% select(-id,-year)
test_later<- test_later %>% select(-id,-year)
rm(sqf_13_14)
rm(shuff)

#B
str(sqf)
#Fit a random forest model on train_half
#no_cores <- detectCores() # Number of cores
#cl<-makeCluster(no_cores) #4
#registerDoParallel(cl)
#sqf.rf<- foreach(ntree=rep(200, 3), .combine=combine, .packages='randomForest', verbose=TRUE) %dopar% { 
# randomForest(found.weapon~.,data=train_half, ntree=ntree,na.action=na.exclude)
#}

sqf.rf <- randomForest(found.weapon~.,data=train_half,ntree=200,na.action=na.exclude)

#C
#Generate predicted probabilities
pred1.probability <- predict(sqf.rf, newdata = test_half, type='response') 
pred2.probability <- predict(sqf.rf, newdata = test_later, type='response') 
#Compute AUC of the model on each test set
test1.prediction <- prediction(as.numeric(pred1.probability), as.numeric(test_half$found.weapon))
test1.performance <- performance(test1.prediction, "auc")
auc1<-100*test1.performance@y.values[[1]]
test2.prediction <- prediction(as.numeric(pred2.probability), as.numeric(test_later$found.weapon))
test2.performance <- performance(test2.prediction, "auc")
auc2<-100*test2.performance@y.values[[1]]
auc1 
#83.83537
auc2
#74.67614




