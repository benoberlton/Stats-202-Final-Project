# Data Loading ----
library(package)
library(dplyr)
library(readr)
library(caret)
library(skimr)
library(RANN) # required for knnInpute
library(caretEnsemble)
library(xgboost)

setwd("C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data")
Study_df <- list.files(path = 'C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data') %>% 
  lapply(read_csv) %>% 
  bind_rows %>% 
  rowwise()%>% 
  mutate(Positive_Total = sum(c_across(P1:P7))) %>% 
  mutate(Negative_Total = sum(c_across(N1:N7))) %>% 
  mutate(General_Total = sum(c_across(G1:G16)))

# Initial Data Visualization
ggplot(Study_df, aes(x= Country, y = PANSS_Total)) + geom_point()

# Data Visualizations and Comprehension ----
Training_data <- Study_df %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) 

Training_data_A <- Study_df %>% 
  filter(Study == "A") 

Training_data_B <- Study_df %>% 
  filter(Study == "B") 

Training_data_C <- Study_df %>% 
  filter(Study == "C") 

Training_data_D <- Study_df %>% 
  filter(Study == "D")

Test_data <- Study_df %>% 
  filter(Study == "E") %>% 
  mutate(LeadStatus = replace(LeadStatus, LeadStatus == "Assign to CS", "Flagged") )%>% 
  rowwise() %>% 
  mutate(Positive_Total = sum(c_across(P1:P7))) %>% 
  mutate(Negative_Total = sum(c_across(N1:N7))) %>% 
  mutate(General_Total = sum(c_across(G1:G16))) %>% 
  select(c(2:6,8, 40:43)) 
  
  
  #distinct(PatientID, VisitDay, .keep_all = TRUE) 

# Initial Data Comparisons
colMeans(Training_data[sapply(Training_data, is.numeric)])
colMeans(Training_data_A[sapply(Training_data_A, is.numeric)])
colMeans(Training_data_A[sapply(Training_data_B, is.numeric)])
colMeans(Training_data_A[sapply(Training_data_C, is.numeric)])
colMeans(Training_data_A[sapply(Training_data_D, is.numeric)])
colMeans(Test_data[sapply(Test_data, is.numeric)])
ggplot(Training_data, aes(x= Country, y = PANSS_Total)) + geom_point()
ggplot(Training_data_A, aes(x= Country, y = PANSS_Total)) + geom_point()
ggplot(Training_data_B, aes(x= Country, y = PANSS_Total)) + geom_point()
ggplot(Training_data_C, aes(x= Country, y = PANSS_Total)) + geom_point()
ggplot(Training_data_D, aes(x= Country, y = PANSS_Total)) + geom_point()
ggplot(Test_data, aes(x= Country, y = PANSS_Total)) + geom_point()
apply(Training_data, 2, max)
apply(Test_data, 2, max)


# Splitting Training and Testing data ----
#decided that group E is unique and lower in all values
set.seed(1)

#data
Training_data <- Study_df %>% 
  filter(Study != c("E")) %>% 
  mutate(LeadStatus = replace(LeadStatus, LeadStatus == "Assign to CS", "Flagged") ) %>% 
  mutate(LeadStatus = replace(LeadStatus, LeadStatus == "Flagged", 1)) %>% 
  mutate(LeadStatus = replace(LeadStatus, LeadStatus == "Passed", 0)) %>% 
  select(c(3:6,8, 40:43)) 


#splitting 80% of data into training and test set
train_row_nums <- createDataPartition(Training_data$LeadStatus, p=0.8, list=FALSE)
Training_data <- Training_data[train_row_nums,]
Test_data <- Training_data[-train_row_nums,]

x <- Training_data %>% select(-LeadStatus)
y <- Training_data$LeadStatus

skimmed = skim(Training_data)
View(skimmed[, c(1:5, 9:11, 13, 15:16)])

# One-Hot Encoding ----
# here I will take all categorical variables and make numerical values for them
dummies_model <- dummyVars(LeadStatus ~ ., data = Training_data)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
Training_data_mat <- predict(dummies_model, newdata = Training_data)

# Convert to dataframe
Training_data <- data.frame(Training_data_mat)

# See the structure of the new dataset
# In above case, we had one categorical variable, Store7 with 2 categories. It was one-hot-encoded to produce two new columns - Store7.No and Store7.Yes.
str(Training_data)
# Preprocessing data ----

#scale the data between 0 and 1 
preProcessed_range_model <- preProcess(Training_data, method = 'range')
Training_data <-  predict(preProcessed_range_model, newdata = Training_data)
Training_data$LeadStatus <- y
View(Training_data)
# Feature Importance using recusive feature elimination (rfe)----
set.seed(100)
options(warn=-1)

#subset sizes of features to test
subsets <- c(1:3, 5, 8)


#rfe settings
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 3,
                   verbose = FALSE)

#running rfe
lmProfile <- rfe(x=Training_data[1:8], y=Training_data$LeadStatus,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

# Training Model ----
trainControl <- trainControl(method="repeatedcv", 
                             number=5, 
                             repeats=3,
                             savePredictions=TRUE)#classProbs =TRUE)

# using random forest, ridge regression, linear model, neural net, SVM, KNN, and stochastic gradient boosting
algorithmList <- c('rf', 'adaboost', 'glm', 'svmRadial', 'knn', 'gbm')


set.seed(100)
models <- caretList(LeadStatus ~ ., data=Training_data, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

# Stacking Models (unused) ----
set.seed(101)
# ensamble controller
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", trControl=stackControl)
print(stack.glm)
# Understanding rf model ----
set.seed(100)
model_rf = train(LeadStatus ~ ., data = Training_data, method = 'rf', importance = TRUE)
varimp_rf <- varImp(model_gbm)
plot(varimp_rf, main="Variable Importance with RF")


# Tuning rf model ----
# tuning settings
fitControl <- trainControl(
  method = 'cv',                  
  number = 5,                      
  savePredictions = 'final'       
) 
set.seed(100)

# Use cv to tune the model features
model_rf2 = train(LeadStatus ~ ., data=Training_data, method='rf', tuneLength = 5, trControl = fitControl)
ggplot(model_rf2)
# Prepare Test data ----
# create one-hot encodings 
Test_data2 <- predict(dummies_model, Test_data)

# Transform the features to range between 0 and 1
Test_data3 <- predict(preProcessed_range_model, Test_data2)

# View
head(Test_data3[, 1:10])
# Test Model ----

# test model
predicted <- predict(model_rf, Test_data3)


# print test statistics 
postResample(pred = predicted, obs = as.factor(Test_data$LeadStatus))

# produce confusion matrix
confusionMatrix(reference = as.factor(Test_data$LeadStatus), data = predicted, mode='everything', positive = 'Flagged')

# Create Data Prediction df ----
Prediction_data = Study_df %>% 
  filter(Study == c("E")) %>% 
  select(c(3:6,8, 40:43)) 


# Prepare Prediction data ----

# one-hot encoding prediction data
Prediction_data2 <- predict(dummies_model, Prediction_data)

# scaling predictino data
Prediction_data3 <- predict(preProcessed_range_model, Prediction_data2)

head(Prediction_data3[, 1:10])
# Produce Predictions ----

final_predictions <- predict(model_rf, Prediction_data3, type = "prob")
Final_df <- data.frame(Prediction_data$AssessmentID)
names(Final_df)[names(Final_df) == "Prediction_data.AssessmentID"] <- 'AssessmentID'
Final_df$LeadStatus <-  final_predictions
write.csv(Final_df, file = "C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data/Final_classes_gbm.csv", row.names = FALSE)

