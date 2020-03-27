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

