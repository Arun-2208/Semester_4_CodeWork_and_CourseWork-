library("caret")

iris.data <- read.csv(file.choose())
# a better visualisation of tabular data
View(iris.data)

#Tell r that the species column is the variable we want to predict
#So that it does not attempt to use it for calculations 
iris.data$species <- as.factor(iris.data$species)

# Use a random variable to pick the cases for the training set
# Specify a seed so the cases picked are the same if we run this again
set.seed(32984)
# The random number sequence gets used here to pick the numbers of the rows
# We specify that species is the variable we wish to predict, so we have a
# similar number of cases of each class included
# 0.7 means we use 70% of the cases for training
indexes <- createDataPartition(iris.data$species, times = 1,
                               p = 0.7, list = FALSE)
# We make a separate table for the training set using the cases we picked
train <- iris.data[indexes,]
# We make a separate table for the testing set using the other (-indexes) cases than for the training
test <- iris.data[-indexes,]

# this stores the parameters for the training 
# we are using repeated crossvalidation with 10 samples and repeat this 3 times
trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

######################Linear SVM###############################################
# The actual training happens here. The species column is our target. All others (~.) are used 
# for prediction. We are using the training dataset and a linear kernel.
# We also give the algorithm the parameters we saved in trainctrl (about crossvalidation)
svmlin <- train(species ~., data=train, method="svmLinear", trControl=trainctrl, 
                preProcess = c("center", "scale"), tuneLength=10)
svmlin
# Testing the model captured in svmlin agains the test dataset
svmresult <- predict(svmlin, newdata=test)
# This variable contains the classification per case
svmresult
# Here we check whether the classification in testresult was accurate
confusionMatrix(table(svmresult, test$species))

######################Linear SVM###############################################

######################Random Forest ###############################################
rfmodel <- train(species ~., data=train, method="ranger", trControl=trainctrl, 
                 preProcess = c("center", "scale"), tuneLength=10)

rfresult <- predict(rfmodel, newdata=test)

confusionMatrix(table(rfresult, test$species))
######################Random Forest ###############################################
