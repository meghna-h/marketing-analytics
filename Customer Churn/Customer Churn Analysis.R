library(plyr)
library(dplyr)
library(pastecs)
library(corrplot)
library(caTools)
library(randomForest)
library(caret)
library(party)


# Load the data -----------------------------------------------------------

setwd("/Volumes/Meghna/Marketing Analytics/Analyses/Customer Churn")
dfCustomer <- read.csv(file="Telco-Customer-Churn.csv", header = TRUE, sep = ",")
print(str(dfCustomer))

# Data Exploration --------------------------------------------------------

# Check for missing values
sapply(dfCustomer, function(x) sum(is.na(x)))

# Drop rows with missing values
dfCustomer <- dfCustomer[complete.cases(dfCustomer), ]

# Crossverify content of categorical variable by calculating frequencies

#tableCustomer <- table(dfCustomer$MultipleLines)
#tableCustomer <- table(dfCustomer$OnlineSecurity)
# tableCustomer <- table(dfCustomer$OnlineBackup)
# tableCustomer <- table(dfCustomer$DeviceProtection)
# tableCustomer <- table(dfCustomer$TechSupport)
# tableCustomer <- table(dfCustomer$StreamingTV)
# tableCustomer <- table(dfCustomer$StreamingMovies)
# tableCustomer

# Replace non-categorical values like "No phone service" with "No" for column “MultipleLines”

dfCustomer$MultipleLines <- as.factor(mapvalues(dfCustomer$MultipleLines, from=c("No phone service"), to=c("No")))

# Replace non-categorical values like "No internet service" with "No" 
  #for columns “OnlineSecurity”, “OnlineBackup”, “DeviceProtection”, “TechSupport”, “streamingTV”, “streamingMovies”

for(i in 10:15) {
  dfCustomer[,i] <- as.factor(mapvalues(dfCustomer[,i], from =c("No internet service"),to=c("No")))
}

# Change seniorcitizen from numerical to categorical

dfCustomer$SeniorCitizen <- as.factor(mapvalues(dfCustomer$SeniorCitizen,from=c("0","1"),to=c("No", "Yes")))

# Group "tenure" to change it from contibuous to categorical 

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
dfCustomer$tenure_group <- sapply(dfCustomer$tenure,group_tenure)
dfCustomer$tenure_group <- as.factor(dfCustomer$tenure_group)

# Remove the columns we do not need for the analysis.

dfCustomer$customerID <- NULL
dfCustomer$tenure <- NULL

print(str(dfCustomer))

#Convert character data into factors

dfCustomer$Churn <- as.factor(dfCustomer$Churn)
dfCustomer$gender <- as.factor(dfCustomer$gender)
dfCustomer$PaperlessBilling <- as.factor(dfCustomer$PaperlessBilling)
dfCustomer$Contract <- as.factor(dfCustomer$Contract)
dfCustomer$Partner <- as.factor(dfCustomer$Partner)
dfCustomer$Dependents <- as.factor(dfCustomer$Dependents)
dfCustomer$PhoneService <- as.factor(dfCustomer$PhoneService)
print(str(dfCustomer))

# Summary statistics
numeric_var <- sapply(dfCustomer, is.numeric)
options(scipen=100)
options(digits = 2)
stat.desc(dfCustomer[,numeric_var])


# Feature Selection -------------------------------------------------------

# Numeric Variables using correlation matrix

corr.matrix <- cor(dfCustomer[,numeric_var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

dfCustomer$TotalCharges <- NULL #reomve totalcharges because it is correlated to monthlycharges

# Categorical variables using Pearson's chi-squared test
#chisq.test(dfCustomer$Partner, dfCustomer$Churn, correct=FALSE)

# Data Partioning ---------------------------------------------------------

# set the seed, it will output same output when ever the model is executed
set.seed(2017)

# sample the input data with 70% for training and 30% for testing
sample <- sample.split(dfCustomer$Churn,SplitRatio=0.70)
trainData <- subset(dfCustomer,sample==TRUE)
testData <- subset(dfCustomer,sample==FALSE)

#Confirm the splitting is correct.
dim(trainData); dim(testData)


# Build a Model -----------------------------------------------------------


# LOGISTIC REGRESSION -----------------------------------------------------

telecomModel <- glm(Churn ~ .,family=binomial(link="logit"),data=trainData)
print(summary(telecomModel))

#Feature analysis:
#The top three most-relevant features include Contract, Paperless Billing and tenure group, all of which are categorical variables.
anova(telecomModel, test="Chisq")

#Test the model with test dataset
test_prediction <- predict(telecomModel,newdata=testData,type='response')

# if the prediction probability is greater than 0.5 then those customers are 
#classified as churned customer less than 0.5 are classified as not churning customer
fitted_results <- ifelse(test_prediction > 0.5,1,0)
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"

# calculating the misclassfication rate
misClasificationError <- mean(fitted_results!=testData$Churn)

# calculating the accuracy rate
accuracyRate <- 1-misClasificationError
print(paste('Logistic Regression Accuracy',accuracyRate))

# confusion matrix
print("Confusion Matrix for Logistic Regression"); table(testData$Churn, test_prediction > 0.5)

# Odds Ratio
exp(cbind(OR=coef(telecomModel), confint(telecomModel)))


# DECISION TREE -----------------------------------------------------------

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, trainData)
plot(tree, type ="simple")

#Test the model with test dataset
pred_tree <- predict(tree, testData)

#Check for Accuracy
p1 <- predict(tree, trainData)
tab1 <- table(Predicted = p1, Actual = trainData$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testData$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

# confusion matrix
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testData$Churn)
