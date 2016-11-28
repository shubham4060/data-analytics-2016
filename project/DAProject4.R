require('caret')
require('corrplot')

readData <- read.csv("data.csv",stringsAsFactors = F)
churnData <-readData
#View(churnData)

churnData$churn[churnData$churn == " True"] <- 1
churnData$churn[churnData$churn == " False"] <- 0

#churnData$churn

churnData$churn <- as.factor(churnData$churn)
churnData$churn <- as.numeric(churnData$churn)

churnData <- churnData[,-c(5)]

#summary(churnData$churn)


churnData$state <- as.factor(churnData$state)
churnData$state <- as.numeric(churnData$state)
#summary(churnData$state)

churnData$international_plan[churnData$international_plan == " yes"] <- 1
churnData$international_plan[churnData$international_plan == " no"] <- 0
churnData$international_plan <- as.factor(churnData$international_plan)
churnData$international_plan <- as.numeric(churnData$international_plan)
#summary(churnData$international_plan)

#corr <- cor(x = as.numeric(churnData$international_plan),y = as.numeric(churnData$churn))

churnData$voice_mail_plan[churnData$voice_mail_plan == " yes"] <- 1
churnData$voice_mail_plan[churnData$voice_mail_plan == " no"] <- 0

churnData$voice_mail_plan <- as.factor(churnData$voice_mail_plan)
churnData$voice_mail_plan <- as.numeric(churnData$voice_mail_plan)

#cor(as.numeric(churnData$voice_mail_plan),as.numeric(churnData$churn))

corr <- cor(churnData[sapply(churnData,is.numeric)])

corrplot(corr, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# partition the data in 2/3 : 1/3 ratio for training and testing

inTrain = createDataPartition(churnData$churn, p=2/3, list=FALSE)
dfTrain=churnData[inTrain,]
dfTest=churnData[-inTrain,]

trainX = dfTrain[,c(2:3,5:20)] #exclude area code
testX = dfTest[,c(2:3,5:20)] #exclude area code
dfTest$churn <- as.factor(dfTest$churn)
dfTrain$churn <- as.factor(dfTrain$churn)
#The training dataset is in dfTrain and the test dataset in #dfTest

#Make a model for Naive Bayes
model = train(trainX,dfTrain$churn,'nb',trControl=trainControl(method='cv',number=10)) # 10 fold cross-validation set
# find the confusion matrix
prediction = predict(model$finalModel,testX)$class
truth = dfTest$churn
result_NB<- confusionMatrix(prediction, truth)
precision <- result_NB$byClass['Pos Pred Value']    
recall <- result_NB$byClass['Sensitivity']
#accuracy is mentioned already as such

#print result of Naive Bayes
result_NB

#Make a model for decision tree
model = train(trainX,dfTrain$churn,method="rpart",trControl=trainControl(method='cv',number=10)) # 10 fold cross-validation set
prediction = predict(model,testX)
truth = dfTest$churn
result_DT<- confusionMatrix(prediction, truth)
precision <- result_DT$byClass['Pos Pred Value']    
recall <- result_DT$byClass['Sensitivity']
#accuracy is mentioned already as such

#print result of Decision Tree
result_DT

#Make a model for polynomial SVM
model = train(trainX,dfTrain$churn,'svmPoly',trControl=trainControl(method='cv',number=10)) # 10 fold cross-validation set
prediction = predict(model,testX)
truth = dfTest$churn
result_SVM<- confusionMatrix(prediction, truth)
precision <- result_SVM$byClass['Pos Pred Value']    
recall <- result_SVM$byClass['Sensitivity']
#accuracy is mentioned already as such

#print result of SVM
result_SVM

