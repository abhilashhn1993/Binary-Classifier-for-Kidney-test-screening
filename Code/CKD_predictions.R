library(readxl)
ckd_final <- read_excel("C:/Users/skota/Downloads/ckd_final.xlsx")
ckd1 <- read_excel("C:/Users/skota/Downloads/ckd.xls", sheet = "All Data")

train <- ckd1[,-c(1,5,6,7,8,9,10,11,12,14,19,20,23,26,28,32,33)]

df <- which(is.na(train$CKD), arr.ind = TRUE)
test <- train[df,]
train <- train[-df,]
test <- test[,-c(16,17)]

for (i in 1:ncol(train))
{
  
  
  if(sum(is.na(train[,i])) != 0){
    na <- which(is.na(train[,i]))
    train <- train[-na,]
    
  }
  
}
for (i in 1:ncol(test))
{
  
  
  if(sum(is.na(test[,i])) != 0){
    na <- which(is.na(test[,i]))
    test <- test[-na,]
    
  }
  
}

sapply(test, function(x) sum(is.nan(x)))

train$Female <- as.factor(train$Female)
train$Racegrp <- as.factor(train$Racegrp)
train$Obese <-as.factor(train$Obese)
#train$Dyslipidemia <-as.factor(train$Dyslipidemia)
train$PVD <- as.factor(train$PVD)
train$Smoker <- as.factor(train$Smoker)
train$Hypertension <- as.factor(train$Hypertension)
train$Diabetes <- as.factor(train$Diabetes)
train$CVD <- as.factor(train$CVD)
train$CKD <- as.factor(train$CKD)
train$Activity <- as.factor(train$Activity)
train$Stroke <- as.factor(train$Stroke)

str(train)

#univariate analysis for age

#boxplot(train$Age)
#age_CKD <- train[which(train$CKD==1),]
#age <- train[-c(which(train$CKD==1)),]

#boxplot(age_CKD$Age, main = "With CKD = 1")
#mean(age_CKD$Age)
#outlier <- boxplot(age_CKD$Age)$out

#boxplot(age$Age, main = "With CKD = 0")
#mean(age_CKD$Age)
#outlier <- boxplot(age_CKD$Age)$out

#train <- train[,-14]

#train$Age <- as.factor(ifelse(a < 59,1,0))

#tbl <- table(train$Age,train$CKD)

#chi_result <- chisq.test(tbl)

#Bivariate analysis for numerical variables

#co <- train[,c(6,7,8)]
#cor_tbl <- cor(co)

#Chi-Sqr test for all cat vars

tbl1 <- table(train$Age,train$CKD)
chi_result1 <- chisq.test(tbl)


tbl2 <- table(train$Female,train$CKD)
chi_result2 <- chisq.test(tbl2)

tbl3<- table(train$Racegrp,train$CKD)
chi_result3 <- chisq.test(tbl3)

tbl4<- table(train$Obese,train$CKD)
chi_result4 <- chisq.test(tbl4)

#tbl5 <- table(train$Dyslipidemia,train$CKD)
#chi_result5 <- chisq.test(tbl5)

tbl6<- table(train$PVD,train$CKD)
chi_result6 <- chisq.test(tbl6)

tbl7<- table(train$Smoker,train$CKD)
chi_result7 <- chisq.test(tbl7)

tbl8<- table(train$Hypertension,train$CKD)
chi_result8 <- chisq.test(tbl8)

tbl9<- table(train$Diabetes,train$CKD)
chi_result9 <- chisq.test(tbl9)

tbl10<- table(train$CVD,train$CKD)
chi_result10 <- chisq.test(tbl10)

tbl11<- table(train$CKD,train$Activity)
chi_result11 <- chisq.test(tbl11)

tbl12<- table(train$CKD,train$Stroke)
chi_result12 <- chisq.test(tbl12)


train <- train [,-16]


#Logistic Regression


#train$CKD <- ifelse(train$CKD==1,0,1)

#train <- train[,-4]
train$CKD <-  as.numeric(train$CKD)
train <- train[,-2]
#str(train)
#sum(is.na(train$CKD))

#train <- cbind(as.data.frame(df_train$Age), train)

train$CKD <- as.factor(train$CKD)
train$Activity <- as.factor(train$Activity)
#str(train)
mod <- glm(CKD~., data = train[,-c(4,2,16)], family = binomial("logit"),maxit = 100)
pred <- predict(mod,test, type = 'response')
hist(pred)
label <- ifelse(pred > 0.1, 1, 0)
tb <- table(label,train$CKD)
acc <- sum(diag(tb))/sum(tb)
sum(label)
summary(mod)

cor(train$Age,train$SBP)
train$Age
train$SBP
aget <- t.test(train$Age~train$CKD)
sbt <- t.test(train$SBP~train$CKD)
dbt <- t.test(train$DBP~train$CKD)
hdl <- t.test(train$HDL~train$CKD)
ldl <- t.test(train$LDL)

(1-exp(coef(mod)))*100 

str(test)
test$Female <- as.factor(test$Female)
test$Racegrp <- as.factor(test$Racegrp)
test$Obese <- as.factor(test$Obese)
test$PVD <- as.factor(test$PVD)
test$Activity <- as.factor(test$Activity)
test$Smoker <- as.factor(test$Smoker)
test$Hypertension <- as.factor(test$Hypertension)
test$Diabetes <- as.factor(test$Diabetes)
test$Stroke <- as.factor(test$Stroke)
test$CVD <- as.factor(test$CVD)
test

