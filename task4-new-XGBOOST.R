install.packages("xgboost")
install.packages("data.table")
install.packages("Matrix")
install.packages("pROC")
install.packages("ROSE")

library(ROSE)
library(xgboost)
library(data.table)
library(Matrix)
library(pROC)
# load data
data <- read.csv("bank-full.csv")

# data check
str(data)

# transform y to binary, 1 for yes, 0 for no
data$y_binary <- ifelse(data$y == "yes", 1, 0)

# check transformation result
table(data$y_binary)
# check the previous column
summary(data$previous)

# check the distribution of y_binary
table(data$y_binary)

# plot the historical subscriptions vs. current subscription
plot(data$previous, data$y_binary,
     main="Historical Subscriptions vs. Current Subscription",
     xlab="Historical Subscriptions (previous)",
     ylab="Current Subscription (y_binary)")

# add a regression line
abline(lm(data$y_binary ~ data$previous), col="blue")

# compute the average historical subscriptions for each current subscription status and plot bar chart
library(dplyr)
avg_subscriptions <- data %>%
  group_by(y_binary) %>%
  summarise(mean_previous = mean(previous))

barplot(avg_subscriptions$mean_previous,
        names.arg = c("No Subscription", "Subscription"),
        col = c("orange", "lightblue"),
        main="Average Historical Subscriptions by Current Subscription Status",
        xlab="Current Subscription (y_binary)",
        ylab="Average Historical Subscriptions (previous)")

# divide the data into training and testing
set.seed(123)
trainIndex <- createDataPartition(data$y_binary, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# check the distribution of y_binary in training and testing data
table(train_data$y_binary)
table(test_data$y_binary)

# get the number of rows in the training data and use it to oversample
original_size <- nrow(train_data)

train_data_rose <- ovun.sample(y_binary ~ ., data = train_data, method = "over", N = original_size * 1.75, seed = 1)$data

# check the distribution of y_binary in the oversampled training data
table(train_data_rose$y_binary)

str(train_data_rose)
str(test_data)

# check for missing values
sum(is.na(train_data_rose))
sum(is.na(test_data))

# extract the numeric columns from the training and testing data to ensure consistent features input to XGBoost
train_numeric_columns <- colnames(train_data_rose)[sapply(train_data_rose, is.numeric)]
test_numeric_columns <- colnames(test_data)[sapply(test_data, is.numeric)]

# check if the numeric columns are consistent
identical(train_numeric_columns, test_numeric_columns)

# delete the target variable y_binary from the numeric data to standardize and PCA the features later
numeric_train_data <- train_data_rose[, train_numeric_columns]
numeric_test_data <- test_data[, test_numeric_columns]

# ensure that the target variable y_binary is used only as a label and not for feature standardization
train_label <- numeric_train_data$y_binary
test_label <- numeric_test_data$y_binary

# remove the target variable column from the data
numeric_train_data <- numeric_train_data[, -which(names(numeric_train_data) == "y_binary")]
numeric_test_data <- numeric_test_data[, -which(names(numeric_test_data) == "y_binary")]

# standardize the training and testing data using the mean and standard deviation of the training set
numeric_train_data <- scale(numeric_train_data)
numeric_test_data <- scale(numeric_test_data,
                           center = attr(numeric_train_data, "scaled:center"),
                           scale = attr(numeric_train_data, "scaled:scale"))


# use the training data to fit the PCA model and reduce the dimensions of the training and testing data
pca <- prcomp(numeric_train_data, center = TRUE, scale. = TRUE)
train_data_pca <- as.data.frame(predict(pca, numeric_train_data))

# add the PCA-transformed principal components to the testing set
test_data_pca <- as.data.frame(predict(pca, numeric_test_data))

# add the target variable y_binary to the reduced training and testing data as the model label
train_data_pca$y_binary <- train_label
test_data_pca$y_binary <- test_label

# apart from the target variable, the rest of the columns are the features
train_matrix <- as.matrix(train_data_pca[, -ncol(train_data_pca)])  # remove the last column, which is the target variable
train_labels <- train_data_pca$y_binary

test_matrix <- as.matrix(test_data_pca[, -ncol(test_data_pca)])    # remove the last column, which is the target variable
test_labels <- test_data_pca$y_binary

# transform the data into the XGBoost DMatrix format
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# set the parameters for the XGBoost model
params <- list(
  booster = "gbtree",       # use tree-based models
  objective = "binary:logistic", # binary classification problem
  eval_metric = "auc",      # evaluation metric: AUC
  eta = 0.1,                # learning rate
  max_depth = 6,            # max depth of each tree
  subsample = 0.8,          # data sampling ratio
  colsample_bytree = 0.8    # feature sampling ratio
)


# train the XGBoost model
# watchlist using for early stopping
watchlist <- list(train = dtrain, test = dtest)

# train the model and monitor the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,            # maximum number of iterations
  watchlist = watchlist,
  early_stopping_rounds = 10 # if the AUC does not improve for 10 rounds, stop training
)

# evaluate the model on the test set
# predict probabilities
pred_probs <- predict(xgb_model, newdata = dtest)

# generate predicted labels (0 or 1 based on a threshold of 0.5
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# install.packages("caret")
library(caret)
# install.packages("pROC")
library(pROC)

# compute the confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(test_labels))
print(conf_matrix)

# compute and visualize the ROC curve
roc_curve <- roc(test_labels, pred_probs)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))


# cross-validation to find the best number of iterations
xgb_cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 100,
  nfold = 5,                # 5-fold cross-validation
  showsd = TRUE,
  stratified = TRUE,
  early_stopping_rounds = 10,
  maximize = TRUE,
  metrics = "auc"
)

# output the best AUC and the corresponding number of iterations
best_nrounds <- xgb_cv$best_iteration
best_auc <- max(xgb_cv$evaluation_log$test_auc_mean)
print(paste("Best AUC:", best_auc, "at round:", best_nrounds))



# using the best number of iterations to train the final model
xgb_model_best <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = watchlist,
  early_stopping_rounds = 10
)

# evaluate the final model on the test set
pred_probs_best <- predict(xgb_model_best, newdata = dtest)
pred_labels_best <- ifelse(pred_probs_best > 0.5, 1, 0)

# generate the confusion matrix
library(caret)
confusion_matrix_best <- confusionMatrix(as.factor(pred_labels_best), as.factor(test_data$y_binary))
print(confusion_matrix_best)

# visualize the confusion matrix
# transform the confusion matrix into a data frame
cm_table_best <- as.table(confusion_matrix_best$table)
df_cm_best <- as.data.frame(cm_table_best)
colnames(df_cm_best) <- c("Predicted", "Actual", "Freq")

# draw the heatmap
library(ggplot2)

ggplot(data = df_cm_best, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1, size = 10) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20)
  ) +
  labs(title = "Confusion Matrix Heatmap")


#compute the ROC curve and AUC
library(pROC)
roc_curve_best <- roc(test_data$y_binary, pred_probs_best)   # compute ROC curve
auc_value_best <- auc(roc_curve_best)                        # compute AUC
print(paste("AUC:", auc_value_best))

# draw the ROC curve
plot(roc_curve_best,ylim=c(0, 1), col = "blue", lwd = 2, main = paste("ROC Curve - AUC:", round(auc_value_best, 2)))

# draw the ROC curve and add the AUC value
ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  #add a diagonal dashed line
  labs(title = "ROC Curve",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  ylim(0, 1) +
  theme_minimal() +
  annotate("text", x = 0.6, y = 0.2, label = paste("AUC =", round(auc_value_best, 3)), size = 5, color = "red")  # add the AUC value


# extract the confusion matrix statistics
confusion_stats_best <- confusion_matrix_best$byClass
precision_best <- confusion_stats_best["Pos Pred Value"]
recall_best <- confusion_stats_best["Sensitivity"]
f1_score_best <- 2 * ((precision_best * recall_best) / (precision_best + recall_best))
print(paste("Precision:", round(precision_best, 2)))
print(paste("Recall:", round(recall_best, 2)))
print(paste("F1 Score:", round(f1_score_best, 2)))

