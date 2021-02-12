# Factors Causing Chronic Kidney Disease
Built the relationship model between factors causing chronic kidney disease. Identified the individuals who could be affected.  

### TABLE OF CONTENTS
* [Objective](#objective)
* [Conceptual Model](#conceptual_model)
* [Variable Selection](#variable_selection)
* [Data Exploration](#data_exploration)
* [Logistic Regression Model](#logistic_regression_model)
* [Results](#results)

## OBJECTIVE 
* To  identify factors causing chronic kidney disease

* Build a predictive model to identify subjects, who could potentially be affected by CKD.

* Interpret the statistical model to estimate the impact of each factor leading to CKD.

*NOTE:* Data is restricted for public access

## CONCEPTUAL Model

![GitHub Logo](https://github.com/skotak2/Factors_Causing_Chronic_Kidney_Disease/blob/main/Images/conceptual_model.PNG)

## VARIABLE SELECTION

Based on the conceptual model

* The variables capturing details such as income, martial status, education, source of medical care and insurance details, were discarded.

* The variables like BMI, waist size, weight and height were discarded, considering no direct impact with the target variable.

* Total cholesterol, inheritance  of diabetes and hypertension from family were removed

### STATISTICAL TESTS

Chi – Square test to identify the relation between categorical variables and target (binary) variables.

T-test to identify the relationship with target variables and the variable under consideration

![GitHub Logo](https://github.com/skotak2/Factors_Causing_Chronic_Kidney_Disease/blob/main/Images/data_exp.PNG)

## LOGISTIC REGRESSION

Built the logistic regression model to understand the relation the between *variables* (predictors for the chronic kidney disorder) and the *traget variable* (weather an individual is affected or not)

![GitHub](https://github.com/skotak2/Factors_Causing_Chronic_Kidney_Disease/blob/main/Images/Logistic%20Regression.PNG)

![GitHub](https://github.com/skotak2/Factors_Causing_Chronic_Kidney_Disease/blob/main/Images/results.PNG)

## RESULTS

Factors like AGE, RACE(hispanic), HDL levels, HYPERTENSION, DIABETES and CARDIO VASCULAR DISEASES proved to be the significant predictors for the target variable.
