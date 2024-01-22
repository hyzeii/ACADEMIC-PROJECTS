library(rpart)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(readr)
library(Metrics)
library(dplyr) 
library(glmnet) 
library(ggplot2) 
library(caret)

### CLEANING DATA
# load training data set
df_origin_train <- read_csv("training_set.csv")
df_train <- df_origin_train

# load testing data set
df_origin_test <- read_csv("test_set.csv")
df_test <- df_origin_test

# cleaning training data set

# extract car make
df_train <- df_train %>% separate(col=name, into = "name", sep = " ")

# extract value of mileage, engine, max_power
df_train <- df_train %>% separate(col=mileage, into = "mileage", sep = " ")
df_train <- df_train %>% separate(col=engine, into = "engine", sep = " ")
df_train <- df_train %>% separate(col=max_power, into = "max_power", sep = " ")  

# convert new values of mileage, engine, max_power to numeric
df_train$mileage <- as.numeric(df_train$mileage)
df_train$engine <- as.numeric(df_train$engine)
df_train$max_power <- as.double(df_train$max_power)
str(df_train)

# clean torque
df_train$torque <- gsub(" ", "", df_train$torque)
df_train$torque <- gsub("nm", "Nm", df_train$torque)
df_train$torque <- gsub("NM", "Nm", df_train$torque)
df_train$torque <- gsub("Nm/", "Nm@", df_train$torque)
df_train$torque <- gsub("at", "@", df_train$torque)
df_train$torque <- gsub("~", "-", df_train$torque)

# remove any values with no reference to Nm or rpm
df_train = df_train %>%filter(grepl("Nm", df_train$torque)&grepl("rpm", df_train$torque))

# separate torque into Nm and rpm
df_train <- df_train %>% separate(col = torque, into = c("Nm", "rpm"), sep = "@")

# remove measurement unit and convert to numeric
df_train <- df_train %>% separate(col = Nm, into = "Nm", sep = "Nm")
df_train$Nm <- as.numeric(df_train$Nm)

# remove measurement unit, split into 2 columns and convert to numeric
df_train$rpm = gsub("rpm", "",df_train$rpm)
df_train$rpm <- gsub("\\+/-.*", "", df_train$rpm)
df_train <- separate(df_train, col = rpm, into = c('rpm1', 'rpm2'), sep = '-')
df_train$rpm1 <- as.numeric(df_train$rpm1)
df_train$rpm2 <- as.numeric(df_train$rpm2)

# take average rpm and remove rpm1 and rpm2
df_train <- df_train %>% 
  mutate(rpm = ifelse(is.na(rpm2), rpm1, (rpm1 + rpm2)/2))
df_train <- subset(df_train,select = -c(rpm1,rpm2))

# relocate column
df_train <- df_train %>% relocate(seats, .after = last_col())

# checking for na
sum(is.na(df_train))
sapply(df_train, function(x) sum(is.na(x)))

df_train <- df_train %>% drop_na()

# find duplicate data
unique(df_train)

df_train <- df_train %>% distinct()

view(df_train)

# cleaning test data set

# extract car make
df_test <- df_test %>% separate(col=name, into = "name", sep = " ")

# extract value of mileage, engine, max_power
df_test <- df_test %>% separate(col=mileage, into = "mileage", sep = " ")
df_test <- df_test %>% separate(col=engine, into = "engine", sep = " ")
df_test <- df_test %>% separate(col=max_power, into = "max_power", sep = " ")  

# convert new values of mileage, engine, max_power to numeric
df_test$mileage <- as.numeric(df_test$mileage)
df_test$engine <- as.numeric(df_test$engine)
df_test$max_power <- as.double(df_test$max_power)
str(df_test)

# clean torque
df_test$torque <- gsub(" ", "", df_test$torque)
df_test$torque <- gsub("nm", "Nm", df_test$torque)
df_test$torque <- gsub("NM", "Nm", df_test$torque)
df_test$torque <- gsub("Nm/", "Nm@", df_test$torque)
df_test$torque <- gsub("at", "@", df_test$torque)
df_test$torque <- gsub("~", "-", df_test$torque)

# remove any values with no reference to Nm or rpm
df_test = df_test %>%filter(grepl("Nm", df_test$torque)&grepl("rpm", df_test$torque))

# separate torque into Nm and rpm
df_test <- df_test %>% separate(col = torque, into = c("Nm", "rpm"), sep = "@")

# remove measurement unit and convert to numeric
df_test <- df_test %>% separate(col = Nm, into = "Nm", sep = "Nm")
df_test$Nm <- as.numeric(df_test$Nm)

# remove measurement unit, split into 2 columns and convert to numeric
df_test$rpm = gsub("rpm", "",df_test$rpm)
df_test$rpm <- gsub("\\+/-.*", "", df_test$rpm)
df_test <- separate(df_test, col = rpm, into = c('rpm1', 'rpm2'), sep = '-')
df_test$rpm1 <- as.numeric(df_test$rpm1)
df_test$rpm2 <- as.numeric(df_test$rpm2)

# take average rpm and remove rpm1 and rpm2
df_test <- df_test %>% 
  mutate(rpm = ifelse(is.na(rpm2), rpm1, (rpm1 + rpm2)/2))
df_test <- subset(df_test,select = -c(rpm1,rpm2))

# relocate column
df_test <- df_test %>% relocate(seats, .after = last_col())

# checking for na
sum(is.na(df_test))
sapply(df_test, function(x) sum(is.na(x)))

df_test <- df_test %>% drop_na()

# find duplicate data
unique(df_test)

df_test <- df_test %>% distinct()

view(df_test)

# export cleaning data file
write.csv(df_train, "clean_training_dataset.csv", row.names = FALSE)
write.csv(df_test, "clean_testing_dataset.csv", row.names = FALSE)

#1. A linear regression model
clean_training_dataset <- read_csv("clean_training_dataset.csv")
clean_testing_dataset_1_ <- read_csv("clean_testing_dataset.csv")
View(clean_training_dataset)
View(clean_testing_dataset_1_)

df_train<-clean_training_dataset
df_test<-clean_testing_dataset_1_

factor_type = c("name", "fuel", "seller_type", "transmission", "owner")
df_train[factor_type] = lapply(df_train[factor_type],as.factor)
df_test[factor_type] = lapply(df_test[factor_type],as.factor)
str(df_train)
str(df_test)

df_test = rbind(df_train[1,],df_test)
df_test = df_test[-1,]


#Running full model
set.seed(1234)
lm<- lm(selling_price~.,data=df_train)
summary(lm)

train_predictions <- predict(lm, newdata = df_train)
test_predictions <- predict(lm, newdata = df_test)

train_mse <- mse(df_train$selling_price, train_predictions)
test_mse <- mse(df_test$selling_price, test_predictions)

#MSE of training
train_mse
#MSE of testing
test_mse

#Running model exclude seats
set.seed(1234)
lm1<- lm(selling_price~.,data=df_train[,c(-14)])
summary(lm1)

#Running model exclude fuel
set.seed(1234)
lm1<- lm(selling_price~.,data=df_train[,c(-5)])
summary(lm1)

#Running model exclude fuel and seats
set.seed(1234)
lm1<- lm(selling_price~.,data=df_train[,c(-5,-14)])
summary(lm1)

train_predictions <- predict(lm1, newdata = df_train)
test_predictions <- predict(lm1, newdata = df_test)

train_mse <- mse(df_train$selling_price, train_predictions)
test_mse <- mse(df_test$selling_price, test_predictions)

#MSE of training
train_mse
#MSE of testing
test_mse


#3. Elastic Net Model 

set.seed(1234)
para.optimal = data.frame(alpha= vector(), lambda = vector(), cvm = vector())
mat=model.matrix(selling_price ~ ., df_train)[,-3]
alpha.list = seq(0,1, 0.05)
for(i in 1:length(alpha.list)){
  m.elastic = cv.glmnet(mat, df_train$selling_price, alpha = alpha.list[i],
                        type.measure = "mse",
                        nfolds = 5)
  para.optimal[i,1] = alpha.list[i]
  para.optimal[i,2] = m.elastic$lambda.min
  para.optimal[i,3] = min(m.elastic$cvm)
}

row = which.min(para.optimal[,3])
alpha = para.optimal[row,1]
alpha
lambda = para.optimal[row,2]
lambda

m.min = glmnet(mat, df_train$selling_price, 
               alpha = para.optimal[row,1],
               lambda = para.optimal[row,2])

X.test =model.matrix(selling_price ~ ., df_test)[,-3]
X.train =model.matrix(selling_price ~ ., df_train)[,-3]

m.min.train = predict(m.min, newx = X.train)
m.min.test = predict(m.min, newx = X.test)

#MSE of training test
RMSE(df_train$selling_price,m.min.train)^2

#MSE of testing test
RMSE(df_test$selling_price, m.min.test)^2

coef(m.min)

###4. Regression tree
# Data prepration
df_train <- read_csv("clean_training_dataset.csv")
View(df_train)
df_test <- read_csv("clean_testing_dataset.csv")
View(df_test)
str(df_train)
str(df_test)

factor_type = c("name", "fuel", "seller_type", "transmission", "owner")
df_train[factor_type] = lapply(df_train[factor_type],as.factor)
df_test[factor_type] = lapply(df_test[factor_type],as.factor)

# Build a regression tree using the "tree" package
library(tree)
# Fit a regression tree with target variable "selling_price", all predictors is included
tree_model <- tree(selling_price ~ ., data = df_train)
# Summary the tree
summary(tree_model)

# Plot the tree
plot(tree_model)
text(tree_model, pretty = 0.5)

pred_train_tree_model = predict(tree_model, df_train)
mean((pred_train_tree_model - df_train %>% pull(selling_price))^2)

#Calculate MSE, performance of the test dataset
pred_tree_model = predict(tree_model, df_test)
mean((pred_tree_model - df_test %>% pull(selling_price))^2)

# Perform cross-validation to select the optimal tree size
cv_tree <- cv.tree(tree_model, K = 5)
# Plot the size of the tree versus the deviation of the tree
plot(cv_tree$size, cv_tree$dev, type = "b")

# Identify the tree size with the lowest cross-validated error
best_tree_size <- which.min(cv_tree$dev)
best_tree_size <- cv_tree$size[which.min(cv_tree$dev)]
# Prune the tree to the optimal size
pruned_tree <- prune.tree(tree_model, best = 6)
# Summary the prunned tree
summary(pruned_tree)

# Plot the prunned tree
plot(pruned_tree)
text(pruned_tree, pretty = 0.5)

pred_train_pruned_tree = predict(pruned_tree, df_train)
mean((pred_train_pruned_tree - df_train %>% pull(selling_price))^2)

pred_pruned_tree = predict(pruned_tree, newdata = df_test)
mean((pred_pruned_tree - df_test$selling_price)^2)

###5.Random forest
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)

df_train = read.csv("clean_training_dataset.csv")
df_test = read.csv("clean_testing_dataset.csv")
str(df_train)
str(df_test)

factor_type = c("name", "fuel", "seller_type", "transmission", "owner")
df_train[factor_type] = lapply(df_train[factor_type],as.factor)
df_test[factor_type] = lapply(df_test[factor_type],as.factor)
str(df_train)
str(df_test)

df_test = rbind(df_train[1,],df_test)
df_test = df_test[-1,]

library(randomForest)
set.seed(257)
rf_model= randomForest(selling_price ~ ., data= df_train, ntree =10, mtry = 4)
rf_model

varImpPlot(rf_model)

pred_train_rf_model = predict(rf_model, df_train)
mean((pred_train_rf_model - df_train %>% pull(selling_price))^2)

pred_test_rf_model = predict(rf_model, newdata = df_test)
mean((pred_test_rf_model - df_test$selling_price)^2)

library(MASS)
library(caret)
control = trainControl(method ="cv", number = 5, search = "grid")
tuneGrid1 = expand.grid(.mtry = (1:12))
rf_best_mtry = train(selling_price ~ .,
                     data = df_train,
                     method = 'rf',
                     metric = 'mse',
                     tuneGrid = tuneGrid1)
rf_best_mtry
plot(rf_best_mtry)
# The final value used for the model was mtry = 12.

best_ntrees = list()
tuneGrid2 = expand.grid(.mtry = 12)
for (ntree in seq(50, 2000, 50)) {
  set.seed(257)
  rf_best_ntrees = train(selling_price~.,
                         data = df_train,
                         method = "rf",
                         metric = "mse",
                         tuneGrid = tuneGrid2,
                         trControl = control,
                         importance = TRUE,
                         ntree = ntree)
  key = toString(ntree)
  best_ntrees[[key]] = rf_best_ntrees
}
rf_tree_results = resamples(best_ntrees)
summary(rf_tree_results)
#250 is the best number of trees, r-squared = 0.9055390

tune_rf_model <- randomForest(selling_price ~ ., data = df_train, ntree = 250, mtry = 12)
tune_rf_model
varImpPlot(tune_rf_model)

pred_train_tune_rf_model = predict(tune_rf_model, df_train)
mean((pred_train_tune_rf_model - df_train %>% pull(selling_price))^2)

pred_tune_rf_model = predict(tune_rf_model, newdata = df_test)
mean((pred_tune_rf_model - df_test$selling_price)^2)
