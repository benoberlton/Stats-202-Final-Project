# Data Loading ----
library(dplyr)
library(readr)
library(caret)
library(skimr)
library(RANN) # required for knnInpute
library(caretEnsemble)

setwd("C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data")
Study_df <- list.files(path = 'C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data') %>% 
  lapply(read_csv) %>% 
  bind_rows

# Initial Data Visualization
ggplot(Study_df, aes(x= Country, y = PANSS_Total)) + geom_point()

# Data Visualizations and Comprehension ----
Training_data <- Study_df %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) 

Training_data_A <- Study_df %>% 
  filter(Study == "A") %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) 

Training_data_B <- Study_df %>% 
  filter(Study == "B") %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) 

Training_data_C <- Study_df %>% 
  filter(Study == "C") %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) 

Training_data_D <- Study_df %>% 
  filter(Study == "D") %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) 

Test_data <- Study_df %>% 
  filter(Study == "E") %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE) 

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

# 
Training_data <- Study_df%>% 
  filter(Study == "E") %>% 
  group_by(PatientID) %>% 
  top_n(1, VisitDay) %>% 
  select(c(2:8,39))

# splitting 80 % of my data into the training set the rest into the test set
train_row_nums <- createDataPartition(Training_data$PANSS_Total, p=0.8, list=FALSE)
Training_data <- Training_data[train_row_nums,]
Test_data <- Training_data[-train_row_nums,]

x <- Training_data[,1:6]
y <- Training_data$PANSS_Total

skimmed = skim(Training_data)
View(skimmed[, c(1:5, 9:11, 13, 15:16)])

# One-Hot Encoding ----
# here I will take all categorical variables and make numerical values for them
dummies_model <- dummyVars(PANSS_Total ~ ., data = Training_data)

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
Training_data$PANSS_Total <- y
View(Training_data)
# Feature Importance using recusive feature elimination (rfe)----
set.seed(100)
options(warn=-1)
 cl <- makeCluster(detectCores(), type='PSOCK')
 registerDoParallel(cl)
subsets <- c(1:7)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 1,
                   verbose = FALSE,
                   allowParallel = TRUE)

lmProfile <- rfe(x=Training_data[, 1:7], y=Training_data$PANSS_Total,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

# Training Model ----
# traininng settings
trainControl <- trainControl(method="repeatedcv", 
                             number=5, 
                             repeats=3,
                             savePredictions=TRUE)#classProbs =TRUE)

# using random forest, ridge regression, linear model, neural net, SVM, KNN, and stochastic gradient boosting
algorithmList <- c('rf', 'ridge', 'lm', 'svmRadial', 'knn', 'gbm')


set.seed(100)
models <- caretList(PANSS_Total ~ ., data=Training_data, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

# Stacking Models ----
set.seed(101)
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", trControl=stackControl)
print(stack.glm)
# Understanding rf model ----
set.seed(100)
model_rf = train(PANSS_Total ~ ., data = Training_data, method = 'rf', importance = TRUE)
varimp_rf <- varImp(model_rf)
plot(varimp_rf, main="Variable Importance with RF")



# Tuning rf model ----
fitControl <- trainControl(
  method = 'cv',                  
  number = 5,                      
  savePredictions = 'final'       
) 
set.seed(100)
model_rf2 = train(PANSS_Total ~ ., data=Training_data, method='rf', tuneLength = 5, trControl = fitControl)
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
predicted <- predict(model_rf2, Test_data3)

# print statistics 
postResample(pred = predicted, obs = Test_data$PANSS_Total)
Test_data$Predicted = predicted
ggplot() + geom_point(data = Test_data, aes(x = PatientID, y = PANSS_Total), color = "blue") +
  geom_point(data = , aes(x = PatientID, y = Predicted), color = "red") 
   

# Create Data Prediction df ----
Prediction_data = Study_df %>% 
  filter(Study == "E") %>% 
  select(c(2:8)) %>% 
  group_by(PatientID) %>% 
  top_n(1, VisitDay) %>% 
  distinct(PatientID, VisitDay, .keep_all = TRUE)

#adjust assessment ID
Prediction_data$AssessmentID = Prediction_data$AssessmentID + 1

# create index for late 18th week days
index = Prediction_data$VisitDay >= 126

# make next visit for late 18th week days 43 days after the current last day
Prediction_data$VisitDay[index] <- Prediction_data$VisitDay[index] + 43

# add a floor or 18th week for all patients
Prediction_data$VisitDay[Prediction_data$VisitDay < 126] <- 126
Prediction_data$PANSS_Total = NA

# Prepare Prediction data ----
Prediction_data2 <- predict(dummies_model, Prediction_data)

# Transform the features to range between 0 and 1
Prediction_data3 <- predict(preProcessed_range_model, Prediction_data2)

# View
head(Prediction_data3[, 1:10])
# Produce Predictions ----
final_predictions <- predict(model_rf2, Prediction_data3)
Final_df <- data.frame(Prediction_data$PatientID)
names(Final_df)[names(Final_df) == "Prediction_data.PatientID"] <- 'PatientID'
Final_df$PANSS_Total <-  final_predictions
write.csv(Final_df, file = "C:/Users/benja/Desktop/classes/Stats 202/Final Project/FinalProjectData/Study_data/Final_predictions_last_days.csv", row.names = FALSE)
