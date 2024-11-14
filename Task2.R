# load propressing source
source("Data preprocessing.R")
library(randomForest)
library(caret)
library(corrplot)
library(ROSE)
library(ggplot2)



# create a new data form for task2
Data2 <- df %>%
  select(default,balance, housing, loan, y)

# Create derived financial variables and enrich eigenvalues
Data2 <- Data2 %>%
mutate(
  ## balance_level
  balance_level = case_when(
    balance < quantile(balance, 0.33) ~ "low",
    balance >= quantile(balance, 0.33) & balance < quantile(balance, 0.66) ~ "medium",
    balance >= quantile(balance, 0.66) ~ "high"
  ),
  ## financial_stress_index
  financial_stress_index = default + housing + loan + ifelse(balance_level == "low", 1, 0)
  )

# split training data and test data
set.seed(123)
train_index <- sample(1:nrow(Data2), 0.7 * nrow(Data2))
train_data <- Data2[train_index, ]
test_data <- Data2[-train_index, ]

# Because data is unblanced, we need to oversample the data

train_balanced <- ovun.sample(
  y ~ .,
  data = train_data,
  method = "over",
  p = 0.5,
  seed = 123
)$data


# 添加一个标记列，区分原始和过采样数据
train_data$Sample_Type <- "Original"
train_balanced$Sample_Type <- "Balanced"

# Merge two datasets
combined_data <- rbind(train_balanced, train_data)
combined_data$Sample_Type <- factor(combined_data$Sample_Type, levels = c("Original", "Balanced"))
custom_labels <- as_labeller(c(`0` = "y=0", `1` = "y=1"))

# Plotting the distribution
ggplot(combined_data, aes(x = Sample_Type, fill = Sample_Type)) +
  geom_bar(aes(y = ..count..), position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4, color = "black") +
  facet_wrap(~ factor(y), labeller = custom_labels, scales = "free_y") +
  labs(title = "Sample Distribution of y=0 and y=1 Before and After Oversampling",
       x = "Sample Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme(legend.position = "none",  # 隐藏图例
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Calculate the correlation matrix
cor_matrix <- cor(train_balanced[, sapply(train_balanced, is.numeric)])
corrplot(cor_matrix, method = "square", type = "full",
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("lightblue", "white", "red"))(200),
         addCoef.col = "black")

# creat random forest model
set.seed(123)
rf_model <- randomForest(
  y ~ default  + financial_stress_index + balance,
  data = train_balanced,
  ntree = 500,
  mtry = 2,
  importance = TRUE,
)

# Check the importance of features
importance(rf_model)

# predict the test data
rf_pred_class <- predict(rf_model, test_data, type = "class")

# Check the distribution of predicted and actual classes
print(table(rf_pred_class))
print(table(test_data$y))

rf_pred_prob <- predict(rf_model, test_data, type = "prob")[, 1]
rf_pred_class <- ifelse(rf_pred_prob > 0.5, 0, 1)

# Compute confusion matrix and evaluation metrics
confusion_result <- confusionMatrix(as.factor(rf_pred_class), as.factor(test_data$y), positive = "0")
print(confusion_result)


