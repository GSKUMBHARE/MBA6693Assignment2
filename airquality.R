#we load the dataset from R directory
datasets::airquality

#install different libraries 
install.packages("caret")
install.packages("ggthemes")
install.packages("lattice")
install.packages("e1071")
install.packages("nnet")
install.packages("tidyverse")
install.packages("car")
install.packages("class")

#libraries loaded
library(ggthemes)
library(ggplot2)
library(caret)
library(ggiraphExtra)
library(ggplot2)
library(broom)
library(readr)
library(MASS)
library(e1071)
library(nnet)
library(corrplot)
library(tidyverse)
library(car)


#we view our dataset
View(airquality)

#understanding data structure
str(airquality)

#we remove all the N/A values from the dataset
na<- na.omit(airquality)
print(na)

#summarise cleaned dataset
summary(na)

#we will split the dataset into subset of 80:20(trainging:validation)
split_data <- createDataPartition(na$Month, p=0.8, list=FALSE)
testset <- na[-split_data,]
trainset <- na[split_data]

#Summarise the training dataset
summary(trainset)

#We plot histogram
ggplot(data = na,mapping =  aes(Ozone))+geom_histogram( bins = 10)
ggplot(data = na,mapping =  aes(Temp))+geom_histogram(bins = 10)
ggplot(data = na,mapping =  aes(Wind))+geom_histogram(bins = 10)
ggplot(data = na,mapping =  aes(Solar.R))+geom_histogram(bins = 10)

#We plot boxplpot
ggplot(data = airquality,mapping =  aes(-1,Ozone))+geom_boxplot()
ggplot(data = airquality,mapping =  aes(-1,Temp))+geom_boxplot()
ggplot(data = airquality,mapping =  aes(-1,Wind))+geom_boxplot()
ggplot(data = airquality,mapping =  aes(-1,Solar.R))+geom_boxplot()


featurePlot(x=na[,1:4], y=na[,5], plot="box", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3))

#we find correlation between the variables 
correlations <- cor(na[,1:5])
corrplot(correlations, method = "circle")

#overall plot  
plot(na)

#we will be building some plots using multinomial logistic Regression,
#linear Discriminant Analysis and K-nearest Neighbor
#Losgistic regression
#We use logistic regression with two predictor
#1.first predictors are Wind and Temp according to month 
log_fit1=multinom(Month~Temp+Wind, data=na)
print(log_fit1)

#2.Second predictor are Ozone and Solar radiation according to month
log_fit2=multinom(Month~Ozone+Solar.R, data = na)
print(log_fit2)

#3.Next we use model logit with all predictors
log_fit_all=multinom(Month~Ozone+Solar.R+Wind+Temp, data = na)
print(log_fit_all)

#linear Discriminant analysis
#Model LDA1 with one predictor
library(class)
#fit the model
set.seed(7)
fit.knn <- train(Method~Ozone, data=na, method="knn", metric=metric, trControl=control)
print

prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=10)