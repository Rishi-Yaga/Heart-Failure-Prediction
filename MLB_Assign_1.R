# Installing necessary packages for the project.
library(tidyverse)
library(imputeTS)
library(ggplot2)
library(rio)
library(kernlab)
library(caret)
library(ggplot2)
library(e1071)
library(rpart)
library(rpart.plot)

#Checking for NULL Values in the dataframe.
sum(is.na(HeartFailure_1_$age))
sum(is.na(HeartFailure_1_$anaemia))
sum(is.na(HeartFailure_1_$creatinine_phosphokinase))
sum(is.na(HeartFailure_1_$diabetes))
sum(is.na(HeartFailure_1_$ejection_fraction))
sum(is.na(HeartFailure_1_$high_blood_pressure))
sum(is.na(HeartFailure_1_$platelets))
sum(is.na(HeartFailure_1_$serum_creatinine))
sum(is.na(HeartFailure_1_$serum_sodium))
sum(is.na(HeartFailure_1_$sex))
sum(is.na(HeartFailure_1_$smoking))
sum(is.na(HeartFailure_1_$time))
sum(is.na(HeartFailure_1_$DEATH_EVENT))


summary(HeartFailure_1_)



# Perceptron 1 with 2 continuous variables.
HeartFailure_2<- HeartFailure_1_[1:299, c("serum_creatinine", "time", "DEATH_EVENT")]


Plot1<- ggplot(data = HeartFailure_2)+aes(x=serum_creatinine, y=time) +geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT),size = 3)+scale_shape_binned()+ xlab("Serum_creatinine") + ylab("Time") +ggtitle("Death Event vs serum_creatinine and time") +theme(plot.title = element_text(hjust = 0.5))
Plot1


HeartFailure_2$death <- lapply(HeartFailure_2$DEATH_EVENT,function(x) {
  if(x == 0)
    HeartFailure_2$death <- -1
  else if(x == 1)
    HeartFailure_2$death <- 1
  else
    HeartFailure_2$death <- NULL
})  


death_index <- sample(nrow(HeartFailure_2), 0.7 * nrow(HeartFailure_2))
death_train <- HeartFailure_2[death_index, ]
death_test <- HeartFailure_2[-death_index, ] 



X <- death_train[, c("serum_creatinine", "time")] # Input Matrix
y <- death_train$death # Output Vector


#Perceptron Learning Algorithm
perceptron <- function(X, Y, numEpochs) {
  results <- list()
  w <- runif(ncol(X), -10, 10) #Initalize weights
  
  # For loop - number of generations(epochs) - number of times dataset is ran through
  for(j in 1:numEpochs) {
    predictedResult <- numeric(length=100) # Initalize predictedResult vector
    numIncorrect = 0 # Keeps track of # of missclassified points
    
    # For loop - loop throught dataset
    for(i in 1:length(Y)) {
      xi = as.numeric(unlist(X[i,])) # Convert dataframe to vector
      predictedResult[i] = sign(w %*% xi) # Predict the point
      
      # If predicted point is incorrect - change weight
      if(predictedResult[i] != Y[i]) {
        numIncorrect = numIncorrect + 1 # Add one to # of missclassified points
        w <- w + as.numeric(Y[i]) * xi # Update the weight w <- w + WiXi
      }
    }
    # Print results of this generation(epoch)
    cat("\nEpoch #: ", j)
    cat("\nNumber Incorrect: ", numIncorrect)
    cat("\nFinal Weight: ", w)
  }
} 



Y<-as.numeric(unlist(y))

perceptron(X,Y,8)

weight = c(419.8008,-68.27399)
death_test$Wserum_creatinine <- death_test$serum_creatinine*weight[1]
death_test$Wtime <- death_test$time*weight[2]

#Add all calculated weights, and name it as predict
death_test$predict <- rowSums(death_test[,c("Wserum_creatinine","Wtime")])

#Use confusion matrix to evaluate the model performance
perceptronpredicttable <- table(death_test$death == 1 , death_test$predict > 0) + table(death_test$death == -1 , death_test$predict < 0)
perceptronpredicttable


sum(diag(perceptronpredicttable))/sum(perceptronpredicttable)





# Perceptron 2 with 2 binary variables.
HeartFailure_3<- HeartFailure_1_[1:299, c("smoking", "high_blood_pressure", "DEATH_EVENT")]


Plot2<- ggplot(data = HeartFailure_3)+aes(x=high_blood_pressure, y=smoking) +geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT),size = 3)+scale_shape_binned()+ xlab("high_blood_pressure") + ylab("smoking") +ggtitle("Death Event vs high_blood_pressure and smoking") +theme(plot.title = element_text(hjust = 0.5))
Plot2


HeartFailure_3$death <- lapply(HeartFailure_3$DEATH_EVENT,function(x) {
  if(x == 0)
    HeartFailure_3$death <- -1
  else if(x == 1)
    HeartFailure_3$death <- 1
  else
    HeartFailure_3$death <- NULL
})  


death_index <- sample(nrow(HeartFailure_3), 0.7 * nrow(HeartFailure_3))
death_train <- HeartFailure_3[death_index, ]
death_test <- HeartFailure_3[-death_index, ] 



X <- death_train[, c("high_blood_pressure", "smoking")] # Input Matrix
y <- death_train$death # Output Vector


#Perceptron Learning Algorithm
perceptron <- function(X, Y, numEpochs) {
  results <- list()
  w <- runif(ncol(X), -10, 10) #Initalize weights
  
  # For loop - number of generations(epochs) - number of times dataset is ran through
  for(j in 1:numEpochs) {
    predictedResult <- numeric(length=100) # Initalize predictedResult vector
    numIncorrect = 0 # Keeps track of # of missclassified points
    
    # For loop - loop throught dataset
    for(i in 1:length(Y)) {
      xi = as.numeric(unlist(X[i,])) # Convert dataframe to vector
      predictedResult[i] = sign(w %*% xi) # Predict the point
      
      # If predicted point is incorrect - change weight
      if(predictedResult[i] != Y[i]) {
        numIncorrect = numIncorrect + 1 # Add one to # of missclassified points
        w <- w + as.numeric(Y[i]) * xi # Update the weight w <- w + WiXi
      }
    }
    # Print results of this generation(epoch)
    cat("\nEpoch #: ", j)
    cat("\nNumber Incorrect: ", numIncorrect)
    cat("\nFinal Weight: ", w)
  }
} 



Y<-as.numeric(unlist(y))

perceptron(X,Y,8)

weight = c(-0.9343224,0.6683246)
death_test$Whigh_blood_pressure <- death_test$high_blood_pressure*weight[1]
death_test$Wsmoking <- death_test$smoking*weight[2]

#Add all calculated weights, and name it as predict
death_test$predict <- rowSums(death_test[,c("Whigh_blood_pressure","Wsmoking")])

#Use confusion matrix to evaluate the model performance
perceptronpredicttable1 <- table(death_test$death == 1 , death_test$predict > 0) + table(death_test$death == -1 , death_test$predict < 0)
perceptronpredicttable1


sum(diag(perceptronpredicttable1))/sum(perceptronpredicttable1)










# Perceptron 3 with 3 continuous X variables.
HeartFailure_4<- HeartFailure_1_[1:299, c("age", "platelets", "ejection_fraction", "DEATH_EVENT")]

Plot3 <- ggplot(data = HeartFailure_4) +
  aes(x = age, y = platelets) +
  geom_point(aes(colour = DEATH_EVENT, shape = DEATH_EVENT), size = 3) +
  facet_wrap(~ ejection_fraction_cat, ncol = 1) +  # Use the binned ejection_fraction
  scale_shape_manual(values = c(1, 16)) +  # Define specific shapes for DEATH_EVENT categories
  labs(x = "Age", 
       y = "Platelets", 
       title = "Death Event by Age, Platelets, and Ejection Fraction Categories") +
  theme(plot.title = element_text(hjust = 0.5))

Plot3


HeartFailure_4$death <- lapply(HeartFailure_4$DEATH_EVENT,function(x) {
  if(x == 0)
    HeartFailure_4$death <- -1
  else if(x == 1)
    HeartFailure_4$death <- 1
  else
    HeartFailure_4$death <- NULL
})  


death_index <- sample(nrow(HeartFailure_4), 0.7 * nrow(HeartFailure_4))
death_train <- HeartFailure_4[death_index, ]
death_test <- HeartFailure_4[-death_index, ] 



X <- death_train[, c("age","ejection_fraction", "platelets")] # Input Matrix
y <- death_train$death # Output Vector


#Perceptron Learning Algorithm
perceptron <- function(X, Y, numEpochs) {
  results <- list()
  w <- runif(ncol(X), -10, 10) #Initalize weights
  
  # For loop - number of generations(epochs) - number of times dataset is ran through
  for(j in 1:numEpochs) {
    predictedResult <- numeric(length=100) # Initalize predictedResult vector
    numIncorrect = 0 # Keeps track of # of missclassified points
    
    # For loop - loop throught dataset
    for(i in 1:length(Y)) {
      xi = as.numeric(unlist(X[i,])) # Convert dataframe to vector
      predictedResult[i] = sign(w %*% xi) # Predict the point
      
      # If predicted point is incorrect - change weight
      if(predictedResult[i] != Y[i]) {
        numIncorrect = numIncorrect + 1 # Add one to # of missclassified points
        w <- w + as.numeric(Y[i]) * xi # Update the weight w <- w + WiXi
      }
    }
    # Print results of this generation(epoch)
    cat("\nEpoch #: ", j)
    cat("\nNumber Incorrect: ", numIncorrect)
    cat("\nFinal Weight: ", w)
  }
} 



Y<-as.numeric(unlist(y))

perceptron(X,Y,8)

weight = c(2868.999,-1379.35,51602.13)
death_test$Wage <- death_test$age*weight[1]
death_test$Wejection_fraction <- death_test$ejection_fraction*weight[2]
death_test$Wplatelets <- death_test$platelets*weight[3]

#Add all calculated weights, and name it as predict
death_test$predict <- rowSums(death_test[,c("Wage","Wejection_fraction", "Wplatelets")])

#Use confusion matrix to evaluate the model performance
perceptronpredicttable2 <- table(death_test$death == 1 , death_test$predict > 0) + table(death_test$death == -1 , death_test$predict < 0)
perceptronpredicttable2


sum(diag(perceptronpredicttable2))/sum(perceptronpredicttable2)








# Perceptron 4 with 4 continuous X variables.
HeartFailure_5<- HeartFailure_1_[1:299, c("age", "platelets", "ejection_fraction","time", "DEATH_EVENT")]

Plot4 <- ggplot(HeartFailure_4, aes(x = age, y = platelets, color = as.factor(DEATH_EVENT), size = time)) +
  geom_point(alpha = 0.6) +  # Use alpha for better visualization if points overlap
  facet_wrap(~ ejection_fraction_cat) +  # Facet by the binned ejection fraction categories
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Death Event") +  # Color code the death event
  scale_size(range = c(1, 6), name = "Follow-up Time") +  # Adjust point size for 'time', with a legend
  labs(x = "Age", y = "Platelets", title = "Relationship between Age, Platelets, and Death Event\nFaceted by Ejection Fraction Categories") +
  theme_minimal() +  # Use a minimal theme for a cleaner look
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right")


Plot4


HeartFailure_5$death <- lapply(HeartFailure_5$DEATH_EVENT,function(x) {
  if(x == 0)
    HeartFailure_5$death <- -1
  else if(x == 1)
    HeartFailure_5$death <- 1
  else
    HeartFailure_5$death <- NULL
})  


death_index <- sample(nrow(HeartFailure_5), 0.7 * nrow(HeartFailure_5))
death_train <- HeartFailure_5[death_index, ]
death_test <- HeartFailure_5[-death_index, ] 



X <- death_train[, c("age","ejection_fraction", "platelets", "time")] # Input Matrix
y <- death_train$death # Output Vector


#Perceptron Learning Algorithm
perceptron <- function(X, Y, numEpochs) {
  results <- list()
  w <- runif(ncol(X), -10, 10) #Initalize weights
  
  # For loop - number of generations(epochs) - number of times dataset is ran through
  for(j in 1:numEpochs) {
    predictedResult <- numeric(length=100) # Initalize predictedResult vector
    numIncorrect = 0 # Keeps track of # of missclassified points
    
    # For loop - loop throught dataset
    for(i in 1:length(Y)) {
      xi = as.numeric(unlist(X[i,])) # Convert dataframe to vector
      predictedResult[i] = sign(w %*% xi) # Predict the point
      
      # If predicted point is incorrect - change weight
      if(predictedResult[i] != Y[i]) {
        numIncorrect = numIncorrect + 1 # Add one to # of missclassified points
        w <- w + as.numeric(Y[i]) * xi # Update the weight w <- w + WiXi
      }
    }
    # Print results of this generation(epoch)
    cat("\nEpoch #: ", j)
    cat("\nNumber Incorrect: ", numIncorrect)
    cat("\nFinal Weight: ", w)
  }
} 



Y<-as.numeric(unlist(y))

perceptron(X,Y,8)

weight = c(3900.496,-1463.183,-388140, -33579.4)
death_test$Wage <- death_test$age*weight[1]
death_test$Wejection_fraction <- death_test$ejection_fraction*weight[2]
death_test$Wplatelets <- death_test$platelets*weight[3]
death_test$Wtime <- death_test$time*weight[4]

#Add all calculated weights, and name it as predict
death_test$predict <- rowSums(death_test[,c("Wage","Wejection_fraction", "Wplatelets","Wtime")])

#Use confusion matrix to evaluate the model performance
perceptronpredicttable3 <- table(death_test$death == 1 , death_test$predict > 0) + table(death_test$death == -1 , death_test$predict < 0)
perceptronpredicttable3


sum(diag(perceptronpredicttable3))/sum(perceptronpredicttable3)
