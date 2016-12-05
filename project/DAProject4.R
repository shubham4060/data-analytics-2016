#Group 10 - DA Final churn prediction project

#Rameshwar Bhaskaran
#Apurv Kumar
#Aniket Choudhary
#Shubham Sharma
#Asket Agarwal



#packages required by our program
require('caret')
require('corrplot')
require('ROCR')
require('randomForest')

#reading data as data frame into readData
readData <- read.csv("data.csv",stringsAsFactors = F)
churnData <-readData
#View(churnData)

#changing true to 1 and false to 0 to calculate the correlation matrix which requires every variable to be
#of type numeric
churnData$churn[churnData$churn == " True"] <- 1
churnData$churn[churnData$churn == " False"] <- 0

#churnData$churn <- as.factor(churnData$churn)
churnData$churn <- as.numeric(churnData$churn)
churnData$churn

#summary(churnData$churn)

#changing states to numeric data for correlation matrix
churnData$state <- as.factor(churnData$state)
churnData$state <- as.numeric(churnData$state)
#summary(churnData$state)

#changing international_plan to numeric data for correlation matrix
churnData$international_plan[churnData$international_plan == " yes"] <- 1
churnData$international_plan[churnData$international_plan == " no"] <- 0
#churnData$international_plan <- as.factor(churnData$international_plan)
churnData$international_plan <- as.numeric(churnData$international_plan)
#summary(churnData$international_plan)

#changing voice_mail_plan to numeric data for correlation matrix
churnData$voice_mail_plan[churnData$voice_mail_plan == " yes"] <- 1
churnData$voice_mail_plan[churnData$voice_mail_plan == " no"] <- 0
#churnData$voice_mail_plan <- as.factor(churnData$voice_mail_plan)
churnData$voice_mail_plan <- as.numeric(churnData$voice_mail_plan)


#removing phone number (non numeric and redundant regressor)
churnData <- churnData[,-c(5)]

#correlation matrix and temperature plot to determine redundant regressors
corr <- cor(churnData)
View(corr)
#corrplot(corr, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#removing id, state, area code,total_eve_calls,total_night_calls (mod of correlation with churn less than 0.015)
churnData <- churnData[,-c(1,2,4,12,15)]


# partition the data in 2/3 : 1/3 ratio for training and testing
# The relative proportions of true and false in
# the target variable are maintained in training and test set by the following function
inTrain = createDataPartition(churnData$churn, p=2/3, list=FALSE)

#making the target variable factors to turn it into a classification problem
churnData$churn <- as.factor(churnData$churn)
dfTrain=churnData[inTrain,]
dfTest=churnData[-inTrain,]

#The training dataset is in dfTrain and the test dataset in #dfTest (removing the target variable i.e churn)
trainX = dfTrain[-16]
testX = dfTest[-16]

#Make a model for Naive Bayes
model = train(trainX,dfTrain$churn,'nb',trControl=trainControl(method='cv',number=10)) # 10 fold cross-validation set
# find the confusion matrix
predictions = predict(model$finalModel,testX)$class
truth = dfTest$churn
result_NB<- confusionMatrix(predictions, truth)
precision <- result_NB$byClass['Pos Pred Value']
recall <- result_NB$byClass['Sensitivity']
#accuracy is mentioned already as such

#print accuracy, precision, recall and result of Naive Bayes
result_NB

pred <- prediction(as.numeric(predictions), as.numeric(truth));
# # Recall-Precision curve
# RP.perf <- performance(pred, "prec", "rec");
# plot (RP.perf,main="PR curve for NB");

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr");
plot (ROC.perf,col="green",main="ROC Curve");

# ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)

auc

#Make a model for decision tree
model = train(trainX,dfTrain$churn,method="rpart",trControl=trainControl(method='cv',number=10)) # 10 fold cross-validation set
predictions = predict(model,testX)
truth = dfTest$churn
result_DT<- confusionMatrix(predictions, truth)
precision <- result_DT$byClass['Pos Pred Value']
recall <- result_DT$byClass['Sensitivity']
#accuracy is mentioned already as such

#print result of Decision Tree
result_DT

pred2 <- prediction(as.numeric(predictions), as.numeric(truth));
# # Recall-Precision curve
# RP.perf2 <- performance(pred2, "prec", "rec");
# plot (RP.perf2,main="PR curve for DT");

# ROC curve
ROC.perf2 <- performance(pred2, "tpr", "fpr");
plot (ROC.perf2, add = TRUE ,col="red");

# ROC area under the curve
auc.tmp <- performance(pred2,"auc");
auc <- as.numeric(auc.tmp@y.values)

auc


#Make a model for polynomial SVM
model = train(trainX,dfTrain$churn,'svmPoly',trControl=trainControl(method='cv',number=10)) # 10 fold cross-validation set
predictions = predict(model,testX)
truth = dfTest$churn
result_SVM<- confusionMatrix(predictions, truth)
precision <- result_SVM$byClass['Pos Pred Value']
recall <- result_SVM$byClass['Sensitivity']
#accuracy is mentioned already as such

#print result of SVM
result_SVM

pred3 <- prediction(as.numeric(predictions), as.numeric(truth));
# # Recall-Precision curve
# RP.perf3 <- performance(pred3, "prec", "rec");
# plot (RP.perf3,main="PR curve for SVM_poly");

# ROC curve
ROC.perf3 <- performance(pred3, "tpr", "fpr");
plot (ROC.perf3, add = TRUE ,col="black");

# ROC area under the curve
auc.tmp <- performance(pred3,"auc");
auc <- as.numeric(auc.tmp@y.values)

auc


#Make model for random forest classifier
model <- randomForest(dfTrain$churn ~ .,
                    data=trainX, 
                    importance=TRUE, 
                    ntree=6000)

#to identify more redundant regressors
importance(model)
predictions = predict(model,testX)
truth = dfTest$churn
result_RF<- confusionMatrix(predictions, truth)
precision <- result_SVM$byClass['Pos Pred Value']
recall <- result_SVM$byClass['Sensitivity']
#accuracy is mentioned already as such

#print result of SVM
result_RF

pred4 <- prediction(as.numeric(predictions), as.numeric(truth));
# # Recall-Precision curve
# RP.perf4 <- performance(pred4, "prec", "rec");
# plot (RP.perf4,main="PR curve for RF");

# ROC curve
ROC.perf4 <- performance(pred4, "tpr", "fpr");
plot (ROC.perf4, add = TRUE ,col="blue");

# ROC area under the curve
auc.tmp <- performance(pred4,"auc");
auc <- as.numeric(auc.tmp@y.values)

auc
