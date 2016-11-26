churnData <- read.csv("data.csv",stringsAsFactors = F)
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

library(corrplot)
corrplot(corr, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# partition the data in 2/3 : 1/3 ratio for training and testing

library(caret)
inTrain = createDataPartition(churnData$churn, p=2/3, list=FALSE)
dfTrain=churnData[inTrain,]
dfTest=churnData[-inTrain,]