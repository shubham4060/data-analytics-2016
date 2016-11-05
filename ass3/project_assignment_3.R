#reading the data into a table
data <-  read.csv("Mumbai_Housing.csv")

#randomly selecting 70% data as the training set
row <- sample(nrow(data),nrow(data)*.7)

#trainData contains a matrix with 70% rows of the original data matrix
#testData contains a matrix with the leftover 30% of the original data matrix
trainData <- data[row,]
testData <- data[-row,]

#MEDV is the response variable i.e y in our multiple linear regression model
#other variables are the repressor variables i.e x in our model
original <- testData$MEDV

#storing the multiple linear regression in variable model for future use
#The repressors are all variables except the last two columns of the dataframe data
model <- lm(formula = MEDV ~ .- CAT..MEDV,data = trainData)

#model summary
summary(model)

#predicted values stored in result
result <- predict.lm(model,testData[-14])
result

#doing R2 test for the predicted and actual MEDV values to test our model accuracy
R2orig <- 1 - sum((original - result)^2)/sum((original - mean(original))^2)
R2orig

#computing the correlation of MEDV with all other variables
#We observe that several variables have negative correlation with MEDV also they dont seem to have
#significant positive correlation with variables having positive correlation with MEDV
#so the variables that can be removed for better results are : (based on several experiments)
#INDUS
#AGE
corr <-cor(trainData)
#corr[14,]
#corr

#we compute model now without the above variables as the repressors and observe the results
model1 <- lm(formula = MEDV ~ . - (CAT..MEDV + INDUS + AGE ) ,trainData)
result1 <- predict.lm(model1,testData[-14])
#result1

R2mod <- 1 - sum((original - result1)^2)/sum((original - mean(original))^2)

#comparison of results
R2mod
R2orig