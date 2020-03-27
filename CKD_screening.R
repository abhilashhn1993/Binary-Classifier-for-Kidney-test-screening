df <- read.csv('CKD.csv')

library(aod)
library(ggplot2)
library (ROCR)
library(caret)

dim(df)
#8529 rows and 24 columns
str(df)

df <- subset(df, select = -c(X, bmi, fam_hypertension, waist, fam_diabetes, fam_cvd))

sum(is.na(df))
#Total NA values are 3926

#Performing Variable transformation
df$obese <- as.factor(df$obese)
df$dyslipidemia <- as.factor(df$dyslipidemia)
df$pvd <- as.factor(df$pvd)
df$activity <- as.factor(df$activity)
df$poorvision <- as.factor(df$poorvision)
df$smoker <- as.factor(df$smoker)
df$hypertension <- as.factor(df$hypertension)
df$diabetes <- as.factor(df$diabetes)
df$stroke <- as.factor(df$stroke)
df$cvd <- as.factor(df$cvd)
df$chf <- as.factor(df$chf)
df$anemia <- as.factor(df$anemia)
df$ckd <- as.factor(df$ckd)

dim(df)
#8529 rows and 18 columns

######################################################
##Target Variable - CKD (1-0 indicating whether a patient has CKD or not)

#Dividing data into train and test using a manual split.
#The reason I did a manual split is, the dataset was such that the rows from 5796 to 8530 were unlabelled
#and I simply choose to use the unlabeled dataset as my test data to achieve a better training accuracy

train <- df[0:5795,]
tail(train)

test <- df[5795:8530,]
head(test)
tail(test)

#Removing NA values from the train and test dataset
for (i in 1:ncol(train))
{
  if(sum(is.na(train[,i])) != 0){
    na <- which(is.na(train[,i]))
    train <- train[-na,]
  }
}
str(test)
dim(test)
test[,2]
test <-  test[-17]

for (i in 1:ncol(test))
{
  if(sum(is.na(test[,i])) != 0){
    na <- which(is.na(test[,i]))
    test <- test[-na,]
  }
}

#Performing chi-square analysis to identify the relationship between the target variable ckd and other
#independent variables
tb1 <- table(train$age,train$ckd)
chi_result1 <- chisq.test(tb)
chi_result1

tb2 <- table(train$female,train$ckd)
chi_result2 <- chisq.test(tb2)
chi_result2

tb3<- table(train$racegrp,train$ckd)
chi_result3 <- chisq.test(tb3)
chi_result3

tb4<- table(train$obese,train$ckd)
chi_result4 <- chisq.test(tb4)
chi_result4

tb6<- table(train$pvd,train$ckd)
chi_result6 <- chisq.test(tb6)
chi_result6

tb7<- table(train$smoker,train$ckd)
chi_result7 <- chisq.test(tb7)
chi_result7

tb8<- table(train$hypertension,train$ckd)
chi_result8 <- chisq.test(tb8)
chi_result8

tb9<- table(train$diabetes,train$ckd)
chi_result9 <- chisq.test(tb9)
chi_result9

tb10<- table(train$cvd,train$ckd)
chi_result10 <- chisq.test(tb10)
chi_result10

tb11<- table(train$activity,train$ckd)
chi_result11 <- chisq.test(tb11)
chi_result11

tb12<- table(train$stroke,train$ckd)
chi_result12 <- chisq.test(tb12)
chi_result12

dim(train)
str(test)

test$ckd <- as.factor(test$ckd)

test <- test$age[!is.na(test$age)]

View(test2)

#Logistic Regression model to predict CKD values
logreg <- glm(ckd~., data = train, family = "binomial")
summary(logreg)

logreg$fitted.values

anova(logreg, test="Chisq")

#exp(coef(logreg))
#exp(cbind(OR = coef(logreg), confint(logreg)))

#Validating Predictions on training data
pred <- plogis(predict(logreg, train))
pred <- ifelse(pred > 0.5,1,0)
#Using 0.5 as the threshold initially


#test2$ckd <- plogis(predict(logreg, test2))
#test2$ckd <- ifelse(test2$ckd  > 0.5,1,0)
#table(test2$ckd)
#test2 <- test

#test2$pred <- NULL

#test$ckd <- plogis(predict(logreg, test))
#test$ckd <- ifelse(test$ckd  > 0.1,1,0)
#View(test)

#table(test2$ckd)
#dim(test2)

confmat <- table(pred, train$ckd)
sum(diag(confmat))/sum(confmat)
#accuracy = 93.2686 
#recall = 0.5408

#Plotting an ROC curve for the predictions on the training data
library(pROC)
roc(train$ckd, logreg$fitted.values, plot=TRUE)

#Testing predictions with a new threshold of 0.1
pred2 <- plogis(predict(logreg, train))
pred2 <- ifelse(pred2 > 0.1,1,0)

confmat2 <- table(pred2, train$ckd)
sum(diag(confmat2))/sum(confmat2)
#accuracy = 0.83685 
#recall = 0.2664

plot(density(pred))
hist(pred)

plot(density(pred2))
hist(pred2)

library(caret)
confusionMatrix(pred, train$ckd)
pred <- as.integer(pred)

misClasificError <- mean(pred != train$ckd)
print(paste('Accuracy',1-misClasificError))
#93% accuracy on train data

#RP.perf <- performance(pred, "prec", "rec")

#recall <- sensitivity(pred, y, positive="1")

pred

#plotROC(test$ckd, predicted)

#Concordance(test$ckd, predicted)

#confusionMatrix(test$ckd, predicted, threshold = optCutOff)
