#-----------------------------------#
# QUESTION 1                        #
#-----------------------------------#
#-------------------#
#  Setting WD       #
#-------------------#
# Setting working directory
setwd("C:/Users/Enita/OneDrive - University of West Georgia/Desktop/UWG Course Work/Spring Semester 2025/Advanced Visual Analytics/data")


#--------------------#
#   Libraries        #
#--------------------#
library(ggplot2)
library(tidyverse)
library(readr)
library(NHANES)
library(caret)


#--------------------#
#   Data Import      #
#--------------------#
#--------------------------#
# Question 1A: Data Import #
#--------------------------#
# Importing the data
cps_data <- read_csv("cps5.csv")

# Estimate a multiple regression model on wage
model1 <- lm(wage ~ age + female + educ + married + metro + south + west + midwest, data = cps_data)
summary(model1)


#-------------------------#
# Question 1B: Model RMSE #
#-------------------------#
# Generate predicted values
cps_data$wage_hat <- predict(model1)

# Compute RMSE
rmse_model1 <- sqrt(mean((model1$residuals)^2))
cat("RMSE for the full dataset:", round(rmse_model1, 2), "\n")


#---------------------------------#
# Question 1C: Model Partitioning #
#---------------------------------#
# Partition the sample into training (80%) and validation (20%) sets
set.seed(1234)
selected <- sample(1:nrow(cps_data), 0.8 * nrow(cps_data), replace = FALSE)
train_data <- cps_data[selected, ]
validation_data <- cps_data[-selected, ]

# Reevaluating the model on the training set
model2 <- lm(wage~ age + female + educ + married + metro + south + west + midwest, data = train_data)
summary(model2)

# Generate predicted values for the training set
predicted_val <- predict(model2, newdata = validation_data)

# Compute RMSE for the validation set
rmse_model2 <- sqrt(mean((validation_data$wage - predicted_val)^2))
cat("RMSE for the validation set:", round(rmse_model2, 2), "\n")


#--------------------------------------#
# Question 1D: Stepwise Forward Model  #
#--------------------------------------#
# Stepwise forward selection
null_model <- lm(wage ~ 1, data = train_data)
full_model <- lm(wage ~ age + female + educ + married + metro + south + west + midwest, data = train_data)

# Forward selection
step_model <- stepAIC(null_model, 
                      scope = list(lower = null_model, upper = full_model), 
                      direction = "forward")
summary(step_model)

# Generate predicted values for the training set
predicted_step <- predict(step_model, newdata = validation_data)

# Compute RMSE for the validation set
rmse_step <- sqrt(mean((validation_data$wage - predicted_step)^2))
cat("RMSE for the stepwise model on the validation set:", round(rmse_step, 2), "\n")


#--------------------------------------#
# Question 2                           #
#--------------------------------------#
#-------------------------------#
# Question 2a: Data Preparation #
#-------------------------------#
# Check the structure of the data
str(NHANES)

# Filter data for ages 30 and above
health_data <- subset(NHANES, Age >= 30)

# Create binary indicator for depression
health_data$Depressed_Yes <- ifelse(health_data$Depressed %in% c("Several", "Most"), 1, 0)

# Select relevant columns and outcome
health_data <- health_data %>% 
  select("Depressed_Yes", "Gender", "Age", "Education", "Poverty", "PhysActive", "HomeOwn", "Work", "SmokeNow", "SleepTrouble")

# Drop observations with missing values
health_data1 <- na.omit(health_data)


#---------------------------------#
# Question 2b: Logistic Regression#
#---------------------------------#
# Estimate a logistic regression model
log_model <- glm(Depressed_Yes ~ Gender + Age + Education + Poverty + PhysActive + HomeOwn + Work + SmokeNow + SleepTrouble, 
                 data = health_data1, 
                 family = binomial)

# Generate predicted probabilities
predicted_probs <- predict(log_model, type = "response")

# Convert probabilities to binary outcome
predicted_outcome <- ifelse(predicted_probs > 0.5, 1, 0)

# Compute error rate
error_rate <- mean(predicted_outcome != health_data1$Depressed_Yes)
cat("Error rate for the logistic regression model:", round(error_rate, 2), "\n")


#--------------------------------------#
# Question 2c: Model Cross Validation  #
#--------------------------------------#
# Set seed for reproducibility
set.seed(1234)

# Convert outcome to factor 
health_data1$Depressed_Yes <- as.factor(health_data1$Depressed_Yes)

# Create a 10-fold cross-validation partition
train_control <- trainControl(method = "cv", number = 10)

# Train the logistic regression model using cross-validation
cv_model <- train(Depressed_Yes ~ Gender + Age + Education + Poverty + PhysActive + HomeOwn + Work + SmokeNow + SleepTrouble, 
                  data = health_data1, 
                  method = "glm", 
                  family = "binomial", 
                  trControl = train_control)

# Accuracy of the model
accuracy <- cv_model$results$Accuracy
cat("Cross-validated accuracy of the logistic regression model:", round(accuracy, 2), "\n")


#--------------------------------------#
# Question 2d: Model RFE               #
#--------------------------------------#
# Set seed for reproducibility
set.seed(4321)

# Define the control method for RFE
rfe_control <- rfeControl(functions = rfFuncs, 
                          method = "cv", 
                          number = 10)

# Perform RFE
rfe_model <- rfe(x= health_data1[, c("Gender", "Age", "Education", "Poverty", "HomeOwn", "Work", "PhysActive", "SmokeNow", "SleepTrouble")],, 
                 y= health_data1$Depressed_Yes, 
                 sizes = c(1:9), 
                 rfeControl = rfe_control)

# Print the results
rfe_model$results
cat("Best Optimal Model Size:", rfe_model$optVariables, "\n")


#--------------------------------------#
# Question 3: Classification Models    #
#--------------------------------------#
#-------------------------------#
# Question 3a: Data Preparation #
#-------------------------------#
# Check the structure of dataset
str(NHANES)

# Filter data for ages 40 and above
health_data2 <- subset(NHANES, Age >= 40)

# Create binary indicator for hypertension
health_data2$Hbp <- ifelse(health_data2$BPSysAve > 140 | health_data2$BPDiaAve > 90, 1, 0)

# Convert to factor
health_data2$Hbp <- as.factor(health_data2$Hbp)

# Select relevant columns and outcome
health_data3 <- health_data2 %>%
  select("Hbp", "Gender", "Age", "Education", "Poverty", "PhysActive", "HomeOwn", "Work", "Smoke100", "SleepTrouble", "Weight", "BMI", "Pulse", "HealthGen")

# Drop observations with missing values
health_data3 <- na.omit(health_data3)


#-------------------------------#
# Question 3b: Data Splitting   #
#-------------------------------#
# Set seed for reproducibility
set.seed(4321)

# Partition the dataset
train_index <- createDataPartition(health_data3$Hbp, p = 0.75, list = FALSE)
train_data <- health_data3[train_index, ]
valid_data <- health_data3[-train_index, ]


#-------------------------------#
# Question 3c: KNN Model        #
#-------------------------------#
# Define control method for KNN
knn_control <- trainControl(method = "cv", number = 10)

# Train the KNN model
knn_model <- train(Hbp ~ ., 
                   data = train_data, 
                   method = "knn", 
                   trControl = knn_control, 
                   tuneLength = 10,
                   preProcess = c("center", "scale"))
                   

# Display the model best neighbors
knn_model$bestTune
  

#-----------------------------------#
# Question 3d: KNN Model Evaluation #
#-----------------------------------#
# Generate predictions on the validation set
knn_pred <- predict(knn_model, newdata = valid_data)

# Compute accuracy
knn_accuracy <- mean(knn_pred == valid_data$Hbp)
cat("Accuracy of the KNN model on the validation set:", round(knn_accuracy, 2), "\n")


#-----------------------------------#
# Question 3e: Classification Model #
#-----------------------------------#
# Train the classification tree model
tree_model <- train(Hbp ~ ., 
                     data = train_data, 
                     method = "rpart", 
                     trControl = trainControl(method = "cv", number = 10),
                     tuneLength = 10)

# Display the model best tree model
print(tree_model)


#-----------------------------------------------#
# Question 3f: Classification on Validation Set #
#-----------------------------------------------#
# Generate predictions on the validation set
tree_pred <- predict(tree_model, newdata = valid_data)

# Compute accuracy
tree_accuracy <- mean(tree_pred == valid_data$Hbp)
cat("Accuracy of the classification tree model on the validation set:", round(tree_accuracy, 2), "\n")
