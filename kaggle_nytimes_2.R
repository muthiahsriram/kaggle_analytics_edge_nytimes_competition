setwd("C:\\Studies\\Analytics_edge\\Kaggle")
training_test = read.csv("training_test.csv")
training_test$WordCount = log(1+training_test$WordCount)
training =training_test[1:nrow(train),]
training$Popular = train$Popular
training$Popular = as.factor(training$Popular)
testing =training_test[-(1:nrow(train)),]

install.packages("e1071")
svm123 = svm(Popular~.,data =training[,c(1,2,3,4,7,8,95)],probability = TRUE ,decision.values = TRUE ,gama = 0.0001,cost=10)
svm123.pred = predict(svm123, testing[,c(1,2,3,4,7,8)], probability= TRUE)
svm123.pred = attr(svm123.pred, "probabilities")
View(svm123.pred)
ROCRpred = prediction(svm123.pred[,1],training$Popular)
ROCRperf = performance(ROCRpred,"tpr","fpr")
auc.tmp = performance(ROCRpred,'auc')
auc =as.numeric(auc.tmp@y.values)
auc


predict_test1 = predict(svm.fit,testing,probabiltiy =TRUE)
predict_test1 = attr(predict_test1, "probabilities")


submission = cbind(UniqueID = test$UniqueID,Probability1 = svm123.pred[,1])
colnames(submission) = c("UniqueID","Probability1")
write.csv(submission, file = "nytime_17_svm.csv", row.names = FALSE)
summary(svm123.pred[,1])
View(read.csv("nytime_17_svm.csv"))

View(predict_test1)
svm.fit<-svm(as.factor(Popular) ~ ., data=training[,-c(5,6)], method="C-classification", 
             kernel="radial", cost=100, gamma=0.001, cross=10, probability=TRUE)
