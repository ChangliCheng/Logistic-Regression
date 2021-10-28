#Logistic regression
#Performance evaluation
#Dessislava Pachamanova

############################
#State whether scientific notation is preferable
############################
options(scipen=999) #R will not use scientific notation
#options(scipen=0) #R will return to scientific notation

##########################
#Install required packages
##########################
if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

######################
#Import data file
######################
#set working directory to appropriate path, e.g., setwd("~/Courses/QTM2000/Data")
myData <- read.csv("housing_cat.csv")
anyNA(myData)
#######################myData$ZIP.Code <- as.factor(myData$ZIP.Code)
myData$CHAS <- as.factor(myData$CHAS)
myData$CAT.MEDV <- as.factor(myData$CAT.MEDV)

#####################
#Transform variables into their correct type
############################################

########################################################
#Split data randomly into a training set and a test set
########################################################
trainSetSize <- floor(0.75 * nrow(myData))   
RNGkind(sample.kind = "Rejection")
set.seed(50)                       
trainInd <- sample(seq_len(nrow(myData)), size = trainSetSize) 
myDataTrain <- myData[trainInd, ]               
myDataTest <- myData[-trainInd, ] 
dim(myDataTrain)
dim(myDataTest)


##############################
#Create a logistic regression model from training data with 
#all input variables (except ZIP.Code and ID). 
##############################
logRegrModel <- glm(CAT.MEDV ~ CRIM + INDUS + NOX+CHAS + RM + AGE + DIS + RAD + TAX + LSTAT, 
                    data = myDataTrain,
                    family ="binomial")
#summarize logistic regression output 
summary(logRegrModel)

#Even though we are using all explanatory variables in the data set, we cannot abbreviate
#the set of predictors with "." because this data set contains an ID column, which 
#we would not want to use as a predictor in the logistic regression model
#However, you could do it if you first remove the column myData$ID (or any 
#other column you do not want) with the following command:
#myData$ID <- NULL

###########################
#Exponentiated coefficients
###########################
exp(coef(logRegrModel)) 

#########################################################
#Score the logistic regression model on the test data set
#########################################################
predTestScores <- predict(logRegrModel, type="response", newdata=myDataTest) 

######################################################
#Classify test observations based on the probabilities 
#calculated from the logistic regression model
######################################################
##Set cutoff value 
cutoff <- 0.65
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores >= cutoff] <- 1

###############################################
#Output predictions and classifications to file
###############################################
dfToExport <- data.frame(myDataTest,predTestScores,predTestClass)
write.csv(dfToExport, file = "../ROutput/predictedLoans.csv")
################################################
#Create a confusion matrix to determine accuracy
################################################
actualTestClass <- myDataTest$CAT.MEDV
actualTestClass <- as.numeric(as.character(actualTestClass))

#Simply using tables
confMx <- table(predTestClass, actualTestClass) 
confMx

#Using confustionMatrix from the caret package
confusionMatrix(as.factor(predTestClass), as.factor(actualTestClass), positive = "1")
# Use model to predict new observations
###############################################
#Predict the predict the probability (score) that the median owner-occupied home value in a new tract in Boston is above $20,000
#First, store the data about the new observation in a data frame called newObs 
newObs <- data.frame(CRIM = 0.4000, INDUS = 7.00, NOX = 0.400, CHAS = "0", 
                      RM = 4.20, AGE = 50, 
                     DIS = 5.45, RAD = 3, TAX = 300, LSTAT = 4.00)
#Then, ask R to calculate the probability using logRegrModel

predTestScores <- predict(logRegrModel, type="response", newdata=newObs) 
predTestScores
