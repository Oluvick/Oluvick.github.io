---
  title: "Statistical Analysis of Customer Credit Card Attrition"
author: "Victoria Agboola, Ilayda Bekircan"
format: pdf
editor: visual
---

## RESULTS

library(tidyverse)
library(ggplot2)

data.bank <- read.csv("/Users/ilaydabekircan/Documents/STAT/Project/BankChurners.csv")

summary(data.bank)

sum(is.na(data.bank))

# Dropping the CLIENTNUM column using Base R
data.bank <- data.bank[, -which(names(data.bank) == "CLIENTNUM")]
# Dropping the last two columns
data.bank <- data.bank[, -c((ncol(data.bank)-1), ncol(data.bank))]
summary(data.bank)

(correlation_matrix <- cor(data.bank[sapply(data.bank, is.numeric)]))

#correlation between Credit_Limit and Avg_Open_To_Buy
print(correlation_matrix[9, "Credit_Limit"])

data.bank <- data.bank[, -which(names(data.bank) == "Avg_Open_To_Buy")]

data.bank$Attrition_Flag <- as.factor(ifelse(data.bank$Attrition_Flag == "Existing Customer", 0, 1))
table(data.bank$Attrition_Flag)

data.bank$Gender <- as.factor(data.bank$Gender)
data.bank$Education_Level <- as.factor(data.bank$Education_Level)
data.bank$Marital_Status <- as.factor(data.bank$Marital_Status)
data.bank$Income_Category <- as.factor(data.bank$Income_Category)
data.bank$Card_Category <- as.factor(data.bank$Card_Category)


# Bar chart comparing the counts of Education Level within each Marital Status category
ggplot(data.bank, aes(x = Education_Level, fill = Marital_Status)) +
  geom_bar(position = "dodge") +
  labs(x = "Education Level", y = "Count", fill = "Marital Status") +
  theme_minimal()

# Boxplot comparing Customer Age across different Education Levels
ggplot(data.bank, aes(x = Education_Level, y = Customer_Age, fill = Education_Level)) +
  geom_boxplot() +
  labs(x = "Education Level", y = "Customer Age") +
  theme_minimal()

# Boxplot comparing Customer Age across different Income Category
ggplot(data.bank, aes(x = Income_Category, y = Customer_Age, fill = Income_Category)) +
  geom_boxplot() +
  labs(x = "Income Category", y = "Customer Age") +
  theme_minimal()

# Age distribution of bank customers
ggplot(data.bank, aes(x = Customer_Age)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(x = "Customer Age", y = "Frequency") +
  theme_minimal()

ggplot(data.bank, aes(x = Credit_Limit, fill = Gender)) +
  geom_density(alpha = 0.7) +
  labs(x = "Credit Limit", y = "Density", fill = "Gender") +
  theme_minimal()

ggplot(data.bank, aes(x = Attrition_Flag)) +
  geom_bar(aes(fill = Attrition_Flag)) +
  labs(x = "Attrition Flag", y = "Count", fill = "Attrition Status") +
  theme_minimal()

# setting seed before splitting train and test sets
set.seed(42)
ratio <- 0.80
train_set <- sort(sample(1:nrow(data.bank), ceiling(nrow(data.bank)*ratio)))

# creating train and test set
data.train <- data.bank[train_set, ]
data.test  <- data.bank[-train_set, ]

# 0 and 1 ratio of data, train and test
(summary(data.bank$Attrition_Flag) / nrow(data.bank))
(summary(data.train$Attrition_Flag) / nrow(data.train))
(summary(data.test$Attrition_Flag) / nrow(data.test))

### Model 1: Logistic Regression

logit.null <- glm(Attrition_Flag ~ 1, 
                  data = data.train, 
                  family = binomial(link = "logit"))
summary(logit.null)

library(caret)

pred.logit_null <- predict(logit.null, newdata = data.test, type="response")

condition.logit_null <- ifelse(pred.logit_null > 0.5, 1, 0)
(confusion_matrix.logit_null <- confusionMatrix(reference = as.factor(data.test$Attrition_Flag), 
                                                data = as.factor(condition.logit_null), 
                                                positive = "1",
                                                mode="everything"))

library(pROC)
auc(data.test$Attrition_Flag, pred.logit_null)

logit.full <- glm(Attrition_Flag ~ ., 
                  data = data.train, 
                  family = binomial(link = "logit"))
summary(logit.full)

pred.logit_full <- predict(logit.full, newdata = data.test, type="response")

condition.logit_full <- ifelse(pred.logit_full > 0.5, 1, 0)
(confusion_matrix.logit_full <- confusionMatrix(reference = as.factor(data.test$Attrition_Flag), 
                                                data = as.factor(condition.logit_full), 
                                                positive = "1",
                                                mode="everything"))

auc(data.test$Attrition_Flag, pred.logit_full)

car::vif(logit.full)

logit.both <- step(logit.null, list(lower=formula(logit.null),
                                    upper=formula(logit.full)),
                                    direction="both",
                                    trace=0, 
                                    data = data.train)
summary(logit.both)

pred.logit_both <- predict(logit.both, newdata = data.test, type="response")

condition.logit_both <- ifelse(pred.logit_both > 0.5, 1, 0)
(confusion_matrix.logit_both <- confusionMatrix(reference = as.factor(data.test$Attrition_Flag), 
                                                data = as.factor(condition.logit_both), 
                                                positive = "1",
                                                mode="everything"))

auc(data.test$Attrition_Flag, pred.logit_both)

par(mfrow=c(2,2))
plot(logit.full)

outliers <- which(abs(residuals(logit.full)) > 3*sd(residuals(logit.full)))

data.train.2 <- data.train[-outliers,]

n <- nrow(data.train.2)
p <- ncol(data.train.2)-1
chilev <- which(influence(logit.full)$hat > max(2*(p+1)/n, 0.5))
chilev

cooksD <- cooks.distance(logit.full)
highCooksD <- which(cooksD > (4/(n-p-1)))


data.train.3 <- data.train.2[-highCooksD,]
logit.full.2 <- glm(Attrition_Flag ~ ., 
data = data.train.3, 
family = binomial(link = "logit"))
summary(logit.full.2)
                                                                                                                                                                                                                                                                                                                                                                                                                               
par(mfrow=c(2,2))
plot(logit.full.2)

pred.logit_full.2 <- predict(logit.full.2, newdata = data.test, type="response")

condition.logit_full.2 <- ifelse(pred.logit_full.2 > 0.5, 1, 0)
(confusion_matrix.logit_full.2 <- confusionMatrix(reference = as.factor(data.test$Attrition_Flag), 
                                                data = as.factor(condition.logit_full.2), 
                                                positive = "1",
                                                mode="everything"))



### Model 2: Decision Trees
                                                                                                                                                                                                                                                                                                                                                                                                                               

library(rpart)

fit.allpred <- rpart(Attrition_Flag ~., method = "class", data = data.train,
                      control = rpart.control(minsplit = 1, cp = 0.001))
printcp(fit.allpred) 

(cp= fit.allpred$cptable[which.min(fit.allpred$cptable[, "xerror"]), "CP"])
(xerr = fit.allpred$cptable[which.min(fit.allpred$cptable[, "xerror"]), "xerror"])

plotcp(fit.allpred)

library(rpart.plot)

rpart.plot(fit.allpred, extra = "auto")

test_df <- data.frame(actual = data.test$Attrition_Flag, pred = NA)
test_df$pred <- predict(fit.allpred, newdata = data.test, type = "class")
# Create the confusion matrix using caret
conf_matrix <- confusionMatrix(as.factor(test_df$pred), as.factor(test_df$actual), positive = "1")
conf_matrix

prunefit.allp <- prune(fit.allpred, cp =
                      fit.allpred$cptable[which.min(fit.allpred$cptable[, "xerror"]), "CP"])

rpart.plot(prunefit.allp, extra = "auto")

#summary(prunefit.allp)

rootnode_err <- sum(data.train$Attrition_Flag==1)/nrow(data.train)
prelerr = prunefit.allp$cptable[which.min(prunefit.allp$cptable[, "rel error"]), "rel error"]
(presub.err_rate <- rootnode_err*prelerr) 

rootnode_err <- sum(data.train$Attrition_Flag==1)/nrow(data.train)
pxerr = prunefit.allp$cptable[which.min(prunefit.allp$cptable[, "xerror"]), "xerror"]
(pcv.err_rate <- rootnode_err*pxerr)

test_df <- data.frame(actual = data.test$Attrition_Flag, pred = NA)
test_df$prediction <- predict(prunefit.allp, newdata = data.test, type = "class")
conf_matrix_pruned_tree <- confusionMatrix(as.factor(test_df$prediction), as.factor(test_df$actual), positive = "1")
conf_matrix_pruned_tree

# Missclassification error rate:
misclassification_error <- sum(conf_matrix_pruned_tree$table[2,1], conf_matrix_pruned_tree$table[1,2]) /
                          sum(conf_matrix_pruned_tree$table)
misclassification_error

### Model 3: Random Forest
                                                                                                                                                                                                                                                                                                                                                                                                                               
library(ranger)

fit.rf <- ranger(Attrition_Flag ~ ., data = data.train, 
importance = 'impurity', mtry = 3)
print(fit.rf)

library(vip)

(v1 <- vi(fit.rf))

vip(v1)

pred <- predict(fit.rf, data = data.test)
test_df <- data.frame(actual = data.test$Attrition_Flag, pred = NA)
test_df$predictions <- pred$predictions
conf_matrix_rf <- confusionMatrix(as.factor(test_df$predictions), as.factor(test_df$actual), positive = "1")
conf_matrix_rf

library(caret)

recall <- sensitivity(conf_matrix_rf$table, positive = "1")
precision <- precision(conf_matrix_rf$table, positive = "1")
f1_score <- F_meas(conf_matrix_rf$table, positive = "1")

# Print the metrics
print(paste("Recall:", recall))
print(paste("Precision:", precision))
print(paste("F1 Score:", f1_score))
  
### Model 4: Gradient Boosting

library(xgboost)
library(Matrix)

# Transform the predictor matrix using dummy (or indicator or one-hot) encoding 
matrix_predictors.train <- 
as.matrix(sparse.model.matrix(Attrition_Flag ~., data = data.train))[, -1]
matrix_predictors.test <- 
 as.matrix(sparse.model.matrix(Attrition_Flag ~., data = data.test))[, -1]

# Train dataset
pred.train.gbm <- data.matrix(matrix_predictors.train) # predictors only
#convert factor to numeric
data.train.gbm <- as.numeric(as.character(data.train$Attrition_Flag)) 
dtrain <- xgb.DMatrix(data = pred.train.gbm, label = data.train.gbm)
# Test dataset
pred.test.gbm <- data.matrix(matrix_predictors.test) # predictors only
#convert factor to numeric
data.test.gbm <- as.numeric(as.character(data.test$Attrition_Flag))
dtest <- xgb.DMatrix(data = pred.test.gbm, label = data.test.gbm)

watchlist <- list(train = dtrain, test = dtest)
param <- list(max_depth = 2, eta = 1, nthread = 2,
objective = "binary:logistic", eval_metric = "auc")

model.xgb <- xgb.train(param, dtrain, nrounds = 7, watchlist)
pred.y.train <- predict(model.xgb, pred.train.gbm)
prediction.train <- as.numeric(pred.y.train > 0.5)
# Measure prediction accuracy on train data
(tab<-table(data.train.gbm, prediction.train))

threshold <- 0.5 
pred.y = predict(model.xgb, pred.test.gbm)
prediction <- as.numeric(pred.y > threshold) # Convert probabilities to binary predictions
                                                                                                                                                                                                                                                                                                                                                                                                                               
# Now, create the test data frame
test_df <- data.frame(actual = data.test$Attrition_Flag, prediction = prediction)
                                                                                                                                                                                                                                                                                                                                                                                                                               
# Convert both actual and predicted to factors assuming '1' is the positive class
test_df$actual <- as.factor(test_df$actual)
test_df$prediction <- as.factor(test_df$prediction)
                                                                                                                                                                                                                                                                                                                                                                                                                               
# Recreate the confusion matrix
xg.conf_matrix <- confusionMatrix(test_df$prediction, test_df$actual, positive = "1")
print(xg.conf_matrix)
                                                                                                                                                                                                                                                                                                                                                                                                                               

recall <- sensitivity(xg.conf_matrix$table, positive = "1")
                                                                                                                                                                                                                                                                                                                                                                                                                               
# Calculate precision (positive predictive value)
precision <- posPredValue(xg.conf_matrix$table, positive = "1")
                                                                                                                                                                                                                                                                                                                                                                                                                               
# Calculate F1 score
f1_score <- (2 * precision * recall) / (precision + recall)
                                                                                                                                                                                                                                                                                                                                                                                                                               
# Print the metrics
print(paste("Recall:", recall))
print(paste("Precision:", precision))
print(paste("F1 Score:", f1_score))

