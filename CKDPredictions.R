library(ggplot2)
library(caret)
library(dplyr)

df <- read_csv("CKD.csv")

df_bkp <- df

dim(df) 
#8529 rows and 24 features in the dataset
View(df)

str(df)
names(df) <- sub(" ", "_", names(df))

df$obese <- as.factor(df$obese)
df$dyslipidemia <- as.factor(df$dyslipidemia)
df$pvd <- as.factor(df$pvd)
df$activity <- as.factor(df$activity)
df$poorvision <- as.factor(df$poorvision)
df$racegrp <- as.factor(df$racegrp)
df$smoker <- as.factor(df$smoker)
df$hypertension <- as.factor(df$hypertension)
df$diabetes <- as.factor(df$diabetes)
df$stroke <- as.factor(df$stroke)
df$cvd <- as.factor(df$cvd)
df$chf <- as.factor(df$chf)
df$anemia <- as.factor(df$anemia)
df$ckd <- as.factor(df$ckd)

str(df)

dim(df)#8529 rows and 24 features

df$X1 <- NULL

df2 <- na.omit(df)

#Univariate Analysis

#Target variable CKD
table(df2$ckd)
#4653 NOs and 327 Yes

barplot(prop.table(table(df2$ckd))*100, main = "CKD variable distribution", xlab = "CKD", 
        ylab = "Frequency", col=c("steelblue", "red"), ylim=c(0,100))
box()

#The target variable classes are highly imbalanced

#We could balance the data by employing undersampling or oversampling the data. 
#However, considering the sensitivity of the data, we decide to go ahead with the unbalanced data,
#but later use measures like Recall and Precision to measure the performance of the model instead of using
#accuracy.


#Train and test split
set.seed(123)
indx = sample(2, nrow(df2), replace=T, prob=c(0.8,0.2))
df_train <- df2[indx==1,]
df_test <- df2[indx==2,]

dim(df_train) #3994 rows 
dim(df_test)  #986 rows

#We fit a logistic regression model with stepwise regression

mod <- glm(ckd~., data=df_train, family="binomial")
summary(mod)

library(caret)
pred <- predict(mod, newdata=df_test, type="response")
#Predictions using the threshold value as 0.5
pred <- ifelse(pred > 0.5, 1,0)
confusionMatrix(table(pred, df_test$ckd))
#Sensitivity 98.68
#Specificity 13.51

#RANDOM FOREST MODEL
library(randomForest)
rf1 = randomForest(ckd~. , data=df_train, ntree=100)
rf1

plot(rf1)
legend("topright",legend=colnames(rf1$err.rate), cex=0.5, lty=c(1,2,3,4),
       col=c(1,2,3,4), horiz=T)

#parameter tuning of the rf function
rf <- randomForest(ckd~. , data=df_train, ntree=100, replace=TRUE, sampsize= ceiling(0.65*nrow(df_train)),
importance=TRUE, mtry=sqrt(ncol(df_train)))
rf

plot(rf)
legend("topright",legend=colnames(rf$err.rate), cex=0.5, lty=c(1,2,3,4),
       col=c(1,2,3,4), horiz=T)


#Predictions using the randomForest model
pred_rf1 <- predict(rf1, newdata = df_test, type="response")
confusionMatrix(table(pred_rf1, df_test$ckd))
#Sensitivity 99.232
#Specificity 4%

pred_rf <- predict(rf, newdata = df_test, type="response")
confusionMatrix(table(pred_rf, df_test$ckd))
#Sensitivity 99.342
#Specificity 4%


#Plotting ROC to determine the optimal threshold value
library(pROC)

roc(df_train$ckd, mod$fitted.values, plot=TRUE, percent = TRUE, legacy.axes=TRUE,  
    print.auc=TRUE, print.auc.x=23,
    xlab="FP percentage", ylab="TP percentage", col="blue", lwd=3)
#AUC 0.9031

plot.roc(df_train$ckd, rf$votes[,1], percent=TRUE, col="#4daf4a", lwd=4,
         print.auc=TRUE, print.auc.y=42, print.auc.x=23, add=TRUE)
legend("bottomright", legend=c("Logistic Reg Model", "Random Forest model"), col=c("blue", "#4daf4a"), lwd=4)


#Logistic Regression Model provides a better performance on this data  

