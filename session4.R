library (tidyverse)
library(ggpubr)
library(propagate)
library(factoextra)
library(arules)
library(arulesViz)
library(clValid)


##improt dataset

getwd()
data <- read.csv( 'House_Predict.txt' , sep = '\t' , header = TRUE )
head (data)
any(is.na(data))


##data exploration 
##after excluding all categorical data

summary(data)
cor(data$SalePrice , data$YearBuilt ) ## direct realtion but moderate
cor(data$SalePrice , data$OverallQual) ## direct relation and strong
hist(data$OverallQual) ## almost normal distrbiuted most houses have quality between 4-7
hist(data$YearBuilt) ##negative skeweness distribution which means most data were built in the last 20 years

boxplot(data$SalePrice) ## data has outliers

##splitting data 80-20

set.seed(100)

index <- sample(1:nrow(data) , 0.8*nrow(data))

length (index)

traindata <- data[index,]
testdata <- data[-index,]


###use yearbuilt and overal qual to predict other houses price 

## multiple regression 


trainmod <- lm( SalePrice ~ YearBuilt+OverallQual , data = traindata)
testmod <- predict (trainmod , testdata)

summary(trainmod)   ##R-squared:  0.6202 = 62.6% variation in dependent variable (price) caused by those two parameters

mse <- mean((testdata$SalePrice- testmod) ^2)

mse  ## mean square error value is high due to high prices

write.csv(testmod, file = "prediction.csv", row.names = FALSE) ## save predicted values on csv file 
