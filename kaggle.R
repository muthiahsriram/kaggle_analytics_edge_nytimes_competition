training_test[training_test$SectionName =="Travel" & training_test$NewsDesk =="",]$NewsDesk ="Travel"
training_test[training_test$SectionName =="U.S." & training_test$NewsDesk =="",]$NewsDesk ="Styles"
training_test[training_test$SectionName =="World" & training_test$NewsDesk =="",]$NewsDesk ="Foreign"
table(training_test$NewsDesk,training_test$SubsectionName)
training_test[training_test$NewsDesk =="Foreign" & training_test$SubsectionName =="",]$SubsectionName ="Asia Pacific"
training_test[training_test$NewsDesk =="National" & training_test$SubsectionName =="",]$SubsectionName ="Politics"
training_test[training_test$NewsDesk =="OpEd" & training_test$SubsectionName =="",]$SubsectionName ="Room For Debate"
training_test[training_test$NewsDesk =="Styles" & training_test$SubsectionName =="",]$SubsectionName ="Education"
library(tm)
cleancorpus = function(a,sparsity = 0.95,character = 'a'){
corpus =  Corpus(VectorSource(a))
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus , PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,c(stopwords("english")))
corpus =tm_map(corpus,stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm =removeSparseTerms(dtm,sparsity)
dtm=as.data.frame(as.matrix(dtm))
colnames(dtm) = make.names(colnames(dtm))
colnames(dtm) = paste(character, colnames(dtm))
return (dtm)}
hlDTM = cleancorpus(training_test$Headline,0.99,"h")
snippetDTM = cleancorpus(training_test$Snippet,0.99,"s")
abstractDTM =cleancorpus(training_test$Abstract,0.99,"a")
training_test = cbind(training_test[,-c(4,5,6)],hlDTM,snippetDTM,abstractDTM)
training =training_test[1:nrow(train),]
training$Popular = train$Popular
testing =training_test[-(1:nrow(train)),]
View(training_test)
library(rpart)
library(rpart.plot)
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class")
prp(cart)
predict_train = predict(cart,type = "class")
table(training$Popular,predict_train)
(5267+606+487+172)
5267+606/6532
(5267+606)/6532
prtrain = predict(cart)
library(ROCR)
ROCRpred = prediction(prtrain,training$Popular)
len(prtraing)
length(prtraing)
length(prtrain)
length(training$Popular)
nrow(training)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain,training$Popular)
length(training$Popular)
length(prtrain)
View(prtrain)
View(prtrain)
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
plot(ROCRPerf)
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
library(caret)
library(e1071)
numFolds = trainControl(method = "cv",number = 10)
cpGrid =expand.grid(.cp =seq(0.01,0.05,0.01))
train(Popular~.,data =training[,-c(5,6)] ,method = "rpart",trControl =numFolds,tuneGrid =cpGrid)
warning
warning()
warnings()
warnings(1)
numFolds = trainControl(method = "cv",number = 10)
cpGrid =expand.grid(.cp =seq(0.002,0.1,0.02))
train(Popular~.,data =training[,-c(5,6)] ,method = "rpart",trControl =numFolds,tuneGrid =cpGrid)
warnings()
View(training)
colnames(training)
View(training$Popular)
table(training$SectionName,training$Popular)
table(training$NewsDesk,training$Popular)
table(training$WordCount,training$Popular)
summary(training$WordCount)
table(training$WordCount>524,training$Popular)
table(training$WordCount>374,training$Popular)
table(training$WordCount<374,training$Popular)
table(training$weekday,training$Popular)
table(training$period,training$Popular)
train(Popular~.,data =training[,-c(5,6)] ,method = "rpart",trControl =numFolds,tuneGrid =cpGrid)
warnings()
View(training[,-c(5,6)])
table(training$NewsDesk,training$WordCount)
tapply(training$WordCount,training$NewsDesk,mean)
tapply(training$WordCount,training$NewsDesk,max)
with(training,table(NewsDesk,SectionName,SubsectionName))
with(training,table(Subsection,SectionName,NewsDesk))
with(training,table(SubsectionName,SectionName,NewsDesk))
tapply(training$WordCount,training$SubsectionName,mean)
tapply(training$WordCount,training$SubsectionName,min)
tapply(training$WordCount,training$SubsectionName,median)
tapply(training$WordCount,training$SubsectionName,max)
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class")
prp(cart)
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.1)
prp(cart)
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.001)
prp(cart)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.002)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.0009)
auc <- as.numeric(auc.tmp@y.values)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.0008)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
predict_train = predict(cart,type = "class")
table(training$Popular,predict_train)
5251+757
6008/(6008+188+336)
predict_test =predict(cart ,newdata = testing)
submission = cbind(UniqueID = test$UniqueID,Probability1 = predict_test[,2])
write.csv(submission, file = "nytime_2.csv", row.names = FALSE)
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.0001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
prp(cart)
predict_train = predict(cart,type = "class")
table(training$Popular,predict_train)
5247+775
6022/(318+192+6022)
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.001)
prp(cart)
prtrain = predict(cart,data =training[,-c(5,6)] )
library(ROCR)
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.001)
predict_train = predict(cart,type = "class")
prtrain = predict(cart,data =training[,-c(5,6)] )
table(training$Popular,predict_train)
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
predict_test =predict(cart ,newdata = testing)
submission = cbind(UniqueID = test$UniqueID,Probability1 = predict_test[,2])
write.csv(submission, file = "nytime_3.csv", row.names = FALSE)
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.0009)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
predict_test =predict(cart ,newdata = testing)
submission = cbind(UniqueID = test$UniqueID,Probability1 = predict_test[,2])
write.csv(submission, file = "nytime_4.csv", row.names = FALSE)
prp(cart)
plot(cart)
text(cart)
unique(training$weekday)
library(randomForest)
rf =randomForest(Popular~.,data = training[,-c(4,5)])
rf =randomForest(Popular~.,data = training[,-c(4,5)])
training$h X2014
training$X2014
View(training[,-c(4,5)])
train = read.csv("NYTimesBlogTrain.csv",stringsAsFactors=F)
test = read.csv("NYTimesBlogTest.csv",stringsAsFactors=F)
str(train)
# train$NewsDesk = as.factor(train$NewsDesk)
# train$SectionName = as.factor(train$SectionName)
# train$SubsectionName = as.factor(train$SubsectionName)
#
# test$NewsDesk = as.factor(test$NewsDesk)
# test$SectionName = as.factor(test$SectionName)
# test$SubsectionName = as.factor(test$SubsectionName)
training  = train[,-9]
training_test =rbind(training,test)
str(training_test)
training_test$NewsDesk =  as.factor(training_test$NewsDesk)
training_test$SectionName = as.factor(training_test$SectionName)
training_test$SubsectionName = as.factor(training_test$SubsectionName)
training_test$PubDate = date
training_test$weekday = weekdays(date)
summary(training_test$PubDate)
training_test$period = as.numeric(format(date, "%H"))
training_test$period = ifelse(training_test$period <12,"Morning",
ifelse(training_test$period <17 ,"AfterNoon","Evening"))
training_test[training_test$NewsDesk =="Culture" & training_test$SectionName =="",]$SectionName ="Arts"
training_test[training_test$NewsDesk =="Foreign" & training_test$SectionName =="",]$SectionName ="World"
training_test[training_test$NewsDesk =="OpEd" & training_test$SectionName =="",]$SectionName ="Opinion"
training_test[training_test$NewsDesk =="Science" & training_test$SectionName =="",]$SectionName ="Health"
training_test[training_test$NewsDesk =="National" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$NewsDesk =="Styles" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$SectionName =="Arts" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Crosswords/Games" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Health" & training_test$NewsDesk =="",]$NewsDesk ="Science"
training_test[training_test$SectionName =="N.Y. / Region" & training_test$NewsDesk =="",]$NewsDesk ="Metro"
training_test[training_test$SectionName =="Business Day" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Opinion" & training_test$NewsDesk =="",]$NewsDesk ="OpEd"
training_test[training_test$SectionName =="Technology" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Travel" & training_test$NewsDesk =="",]$NewsDesk ="Travel"
training_test[training_test$SectionName =="U.S." & training_test$NewsDesk =="",]$NewsDesk ="Styles"
training_test[training_test$SectionName =="World" & training_test$NewsDesk =="",]$NewsDesk ="Foreign"
table(training_test$NewsDesk,training_test$SubsectionName)
training_test[training_test$NewsDesk =="Foreign" & training_test$SubsectionName =="",]$SubsectionName ="Asia Pacific"
training_test[training_test$NewsDesk =="National" & training_test$SubsectionName =="",]$SubsectionName ="Politics"
training_test[training_test$NewsDesk =="OpEd" & training_test$SubsectionName =="",]$SubsectionName ="Room For Debate"
training_test[training_test$NewsDesk =="Styles" & training_test$SubsectionName =="",]$SubsectionName ="Education"
date= strptime(training_test$PubDate,format = "%Y-%m-%d %H:%M:%S")
training_test$PubDate = date
training_test$weekday = weekdays(date)
summary(training_test$PubDate)
training_test$period = as.numeric(format(date, "%H"))
training_test$period = ifelse(training_test$period <12,"Morning",
ifelse(training_test$period <17 ,"AfterNoon","Evening"))
training_test[training_test$NewsDesk =="Culture" & training_test$SectionName =="",]$SectionName ="Arts"
training_test[training_test$NewsDesk =="Foreign" & training_test$SectionName =="",]$SectionName ="World"
training_test[training_test$NewsDesk =="OpEd" & training_test$SectionName =="",]$SectionName ="Opinion"
training_test[training_test$NewsDesk =="Science" & training_test$SectionName =="",]$SectionName ="Health"
training_test[training_test$NewsDesk =="National" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$NewsDesk =="Styles" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$SectionName =="Arts" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Crosswords/Games" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Health" & training_test$NewsDesk =="",]$NewsDesk ="Science"
training_test[training_test$SectionName =="N.Y. / Region" & training_test$NewsDesk =="",]$NewsDesk ="Metro"
training_test[training_test$SectionName =="Business Day" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Opinion" & training_test$NewsDesk =="",]$NewsDesk ="OpEd"
training_test[training_test$SectionName =="Technology" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Travel" & training_test$NewsDesk =="",]$NewsDesk ="Travel"
training_test[training_test$SectionName =="U.S." & training_test$NewsDesk =="",]$NewsDesk ="Styles"
training_test[training_test$SectionName =="World" & training_test$NewsDesk =="",]$NewsDesk ="Foreign"
table(training_test$NewsDesk,training_test$SubsectionName)
training_test[training_test$NewsDesk =="Foreign" & training_test$SubsectionName =="",]$SubsectionName ="Asia Pacific"
training_test[training_test$NewsDesk =="National" & training_test$SubsectionName =="",]$SubsectionName ="Politics"
training_test[training_test$NewsDesk =="OpEd" & training_test$SubsectionName =="",]$SubsectionName ="Room For Debate"
training_test[training_test$NewsDesk =="Styles" & training_test$SubsectionName =="",]$SubsectionName ="Education"
library(tm)
cleancorpus = function(a,sparsity = 0.95,character = 'a'){
corpus =  Corpus(VectorSource(a))
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus , PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,c(stopwords("english")))
corpus =tm_map(corpus,stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm =removeSparseTerms(dtm,sparsity)
dtm=as.data.frame(as.matrix(dtm))
colnames(dtm) = make.names(colnames(dtm))
colnames(dtm) = paste(character, colnames(dtm),sep="")
return (dtm)}
hlDTM = cleancorpus(training_test$Headline,0.99,"h")
snippetDTM = cleancorpus(training_test$Snippet,0.99,"s")
abstractDTM =cleancorpus(training_test$Abstract,0.99,"a")
training_test = cbind(training_test[,-c(4,5,6)],hlDTM,snippetDTM,abstractDTM)
training =training_test[1:nrow(train),]
train = read.csv("NYTimesBlogTrain.csv",stringsAsFactors=F)
test = read.csv("NYTimesBlogTest.csv",stringsAsFactors=F)
str(train)
# train$NewsDesk = as.factor(train$NewsDesk)
# train$SectionName = as.factor(train$SectionName)
# train$SubsectionName = as.factor(train$SubsectionName)
#
# test$NewsDesk = as.factor(test$NewsDesk)
# test$SectionName = as.factor(test$SectionName)
# test$SubsectionName = as.factor(test$SubsectionName)
training  = train[,-9]
training_test =rbind(training,test)
str(training_test)
training_test$NewsDesk =  as.factor(training_test$NewsDesk)
training_test$SectionName = as.factor(training_test$SectionName)
training_test$SubsectionName = as.factor(training_test$SubsectionName)
#Date operation
date= strptime(training_test$PubDate,format = "%Y-%m-%d %H:%M:%S")
training_test$PubDate = date
training_test$weekday = weekdays(date)
summary(training_test$PubDate)
training_test$period = as.numeric(format(date, "%H"))
training_test$period = ifelse(training_test$period <12,"Morning",
ifelse(training_test$period <17 ,"AfterNoon","Evening"))
training_test[training_test$NewsDesk =="Culture" & training_test$SectionName =="",]$SectionName ="Arts"
training_test[training_test$NewsDesk =="Foreign" & training_test$SectionName =="",]$SectionName ="World"
training_test[training_test$NewsDesk =="OpEd" & training_test$SectionName =="",]$SectionName ="Opinion"
training_test[training_test$NewsDesk =="Science" & training_test$SectionName =="",]$SectionName ="Health"
training_test[training_test$NewsDesk =="National" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$NewsDesk =="Styles" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$SectionName =="Arts" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Crosswords/Games" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Health" & training_test$NewsDesk =="",]$NewsDesk ="Science"
training_test[training_test$SectionName =="N.Y. / Region" & training_test$NewsDesk =="",]$NewsDesk ="Metro"
training_test[training_test$SectionName =="Business Day" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Opinion" & training_test$NewsDesk =="",]$NewsDesk ="OpEd"
training_test[training_test$SectionName =="Technology" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Travel" & training_test$NewsDesk =="",]$NewsDesk ="Travel"
training_test[training_test$SectionName =="U.S." & training_test$NewsDesk =="",]$NewsDesk ="Styles"
training_test[training_test$SectionName =="World" & training_test$NewsDesk =="",]$NewsDesk ="Foreign"
table(training_test$NewsDesk,training_test$SubsectionName)
training_test[training_test$NewsDesk =="Foreign" & training_test$SubsectionName =="",]$SubsectionName ="Asia Pacific"
training_test[training_test$NewsDesk =="National" & training_test$SubsectionName =="",]$SubsectionName ="Politics"
training_test[training_test$NewsDesk =="OpEd" & training_test$SubsectionName =="",]$SubsectionName ="Room For Debate"
training_test[training_test$NewsDesk =="Styles" & training_test$SubsectionName =="",]$SubsectionName ="Education"
hlDTM = cleancorpus(training_test$Headline,0.99,"h")
snippetDTM = cleancorpus(training_test$Snippet,0.99,"s")
abstractDTM =cleancorpus(training_test$Abstract,0.99,"a")
training_test = cbind(training_test[,-c(4,5,6)],hlDTM,snippetDTM,abstractDTM)
training =training_test[1:nrow(train),]
training$Popular = train$Popular
testing =training_test[-(1:nrow(train)),]
View(training)
rf =randomForest(Popular~.,data = training[,-c(4,5)])
summary(training)
training_test$period =as.factor(training_test$period)
training_test$weekday =as.factor(training_test$weekday)
summary(training)
class(training_test$weekday)
class(training_test$period)
class(training_test$NewsDesk)
setwd("C:\\Studies\\Analytics_edge\\Kaggle")
train = read.csv("NYTimesBlogTrain.csv",stringsAsFactors=F)
test = read.csv("NYTimesBlogTest.csv",stringsAsFactors=F)
str(train)
# train$NewsDesk = as.factor(train$NewsDesk)
# train$SectionName = as.factor(train$SectionName)
# train$SubsectionName = as.factor(train$SubsectionName)
#
# test$NewsDesk = as.factor(test$NewsDesk)
# test$SectionName = as.factor(test$SectionName)
# test$SubsectionName = as.factor(test$SubsectionName)
training  = train[,-9]
training_test =rbind(training,test)
str(training_test)
training_test$NewsDesk =  as.factor(training_test$NewsDesk)
training_test$SectionName = as.factor(training_test$SectionName)
training_test$SubsectionName = as.factor(training_test$SubsectionName)
#Date operation
date= strptime(training_test$PubDate,format = "%Y-%m-%d %H:%M:%S")
training_test$PubDate = date
training_test$weekday = weekdays(date)
summary(training_test$PubDate)
training_test$period = as.numeric(format(date, "%H"))
training_test$period = ifelse(training_test$period <12,"Morning",
ifelse(training_test$period <17 ,"AfterNoon","Evening"))
training_test$period =as.factor(training_test$period)
training_test$weekday =as.factor(training_test$weekday)
training_test[training_test$NewsDesk =="Culture" & training_test$SectionName =="",]$SectionName ="Arts"
training_test[training_test$NewsDesk =="Foreign" & training_test$SectionName =="",]$SectionName ="World"
training_test[training_test$NewsDesk =="OpEd" & training_test$SectionName =="",]$SectionName ="Opinion"
training_test[training_test$NewsDesk =="Science" & training_test$SectionName =="",]$SectionName ="Health"
training_test[training_test$NewsDesk =="National" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$NewsDesk =="Styles" & training_test$SectionName =="",]$SectionName ="U.S."
training_test[training_test$SectionName =="Arts" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Crosswords/Games" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Health" & training_test$NewsDesk =="",]$NewsDesk ="Science"
training_test[training_test$SectionName =="N.Y. / Region" & training_test$NewsDesk =="",]$NewsDesk ="Metro"
training_test[training_test$SectionName =="Business Day" & training_test$NewsDesk =="",]$NewsDesk ="Business"
training_test[training_test$SectionName =="Opinion" & training_test$NewsDesk =="",]$NewsDesk ="OpEd"
training_test[training_test$SectionName =="Technology" & training_test$NewsDesk =="",]$NewsDesk ="Culture"
training_test[training_test$SectionName =="Travel" & training_test$NewsDesk =="",]$NewsDesk ="Travel"
training_test[training_test$SectionName =="U.S." & training_test$NewsDesk =="",]$NewsDesk ="Styles"
training_test[training_test$SectionName =="World" & training_test$NewsDesk =="",]$NewsDesk ="Foreign"
table(training_test$NewsDesk,training_test$SubsectionName)
training_test[training_test$NewsDesk =="Foreign" & training_test$SubsectionName =="",]$SubsectionName ="Asia Pacific"
training_test[training_test$NewsDesk =="National" & training_test$SubsectionName =="",]$SubsectionName ="Politics"
training_test[training_test$NewsDesk =="OpEd" & training_test$SubsectionName =="",]$SubsectionName ="Room For Debate"
training_test[training_test$NewsDesk =="Styles" & training_test$SubsectionName =="",]$SubsectionName ="Education"
library(tm)
cleancorpus = function(a,sparsity = 0.95,character = 'a'){
corpus =  Corpus(VectorSource(a))
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus , PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,c(stopwords("english")))
corpus =tm_map(corpus,stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm =removeSparseTerms(dtm,sparsity)
dtm=as.data.frame(as.matrix(dtm))
colnames(dtm) = make.names(colnames(dtm))
colnames(dtm) = paste(character, colnames(dtm),sep="")
return (dtm)}
hlDTM = cleancorpus(training_test$Headline,0.99,"h")
snippetDTM = cleancorpus(training_test$Snippet,0.99,"s")
abstractDTM =cleancorpus(training_test$Abstract,0.99,"a")
training_test = cbind(training_test[,-c(4,5,6)],hlDTM,snippetDTM,abstractDTM)
training =training_test[1:nrow(train),]
training$Popular = train$Popular
testing =training_test[-(1:nrow(train)),]
summary(training_test)
summary(training_test$weekday)
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.0001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.001)
prp(cart)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.0001)
predict_test =predict(cart ,newdata = testing)
ubmission = cbind(UniqueID = test$UniqueID,Probability1 = predict_test[,2])
write.csv(submission, file = "nytime_5.csv", row.names = FALSE)
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.00009)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.00008)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.00001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.000009)
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~NewsDesk+SectionName+SubsectionName+WordCount+weekday+period,data = training[,-c(5,6)] ,method="class",cp=0.0001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.0009)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.0008)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.0001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.00001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.0001)
prtrain = predict(cart,data =training[,-c(5,6)] )
predict_test =predict(cart ,newdata = testing)
submission = cbind(UniqueID = test$UniqueID,Probability1 = predict_test[,2])
write.csv(submission, file = "nytime_6.csv", row.names = FALSE)
cart = rpart(Popular~.,data = training[,-c(5,6)] ,method="class",cp =0.001)
prtrain = predict(cart,data =training[,-c(5,6)] )
ROCRpred = prediction(prtrain[,2],training$Popular)
ROCRPerf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
savehistory(file = "KaggleNytime.Rhistory")
