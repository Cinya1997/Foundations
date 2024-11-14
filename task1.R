# install packages
#install.packages("dplyr")
#install.packages("caret")
#install.packages("DataExplorer")


# load package
library(dplyr)
library(caret)
library(DataExplorer)

# load data
df <- read.csv("bank-full.csv")

#data check
str(df)
summary(df)
colSums(is.na(df))
colSums(df == "")
colSums(df == "na")

# convert variable types to factor

# for target "y"
df <- df %>%
  dplyr::mutate(y = ifelse(y == "yes", 1, 0))

# for TASK1
df <- df %>%
  dplyr::mutate(age_group = case_when(
    age < 30 ~ "18~29",
    age >=30 & age <40 ~ "30~39",
    age >=40 & age < 50 ~"40~50",
    age >=50 & age < 60 ~"50~59",
    age >=60 ~ "60+"
  ))
df$age_group <- as.factor(df$age_group)
df$job <- as.factor(df$job)
df$marital <- as.factor(df$marital)
df$education <- as.factor(df$education)

df$y <- as.factor(df$y)



library(ggplot2)

# Data Preparation: Calculate the Number of Subscriptions and Non-Subscriptions
subscription_data <- df %>%
  group_by(y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Draw a Pie Chart
ggplot(subscription_data, aes(x = "", y = percentage, fill = y)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Overall Subscription Rate for Term Deposits", fill = "Subscription Status") +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Calculate the Subscription Rate for Term Deposits in Each Customer Segment
age_group_summary <- df %>%
  group_by(age_group) %>%
  summarize(
    subscription_rate = mean(as.numeric(as.character(y))) * 100,
    count = n()
  )

job_summary <- df %>%
  group_by(job) %>%
  summarize(
    subscription_rate = mean(as.numeric(as.character(y))) * 100,
    count = n()
  )

education_summary <- df %>%
  group_by(education) %>%
  summarize(
    subscription_rate = mean(as.numeric(as.character(y))) * 100,
    count = n()
  )

marital_summary <- df %>%
  group_by(marital) %>%
  summarize(
    subscription_rate = mean(as.numeric(as.character(y))) * 100,
    count = n()
  )

library(ggplot2)

# Age Grouping and Subscription Rate
ggplot(age_group_summary, aes(x = reorder(age_group, -subscription_rate), y = subscription_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", width=0.5) +
  labs(title = "Subscription Rate by Age Group", x = "Age Group", y = "Subscription Rate (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(subscription_rate, 1), "% (", count, ")")), 
            vjust = -0.5, size = 3.5, color = "black")

# Occupation and Subscription Rate
ggplot(job_summary, aes(x = reorder(job, subscription_rate), y = subscription_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", width=0.5) +
  labs(title = "Subscription Rate by Job", x = "Job", y = "Subscription Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(subscription_rate, 1), "% (", count, ")")), 
            vjust = -0.5, size = 3.5, color = "black")+coord_flip()

# Education Level and Subscription Rate
ggplot(education_summary, aes(x = reorder(education, -subscription_rate), y = subscription_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", width=0.3) +
  labs(title = "Subscription Rate by Education Level", x = "Education Level", y = "Subscription Rate (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(subscription_rate, 1), "% (", count, ")")), 
            vjust = -0.5, size = 3.5, color = "black")

# Marital Status and Subscription Rate
ggplot(marital_summary, aes(x = reorder(marital, -subscription_rate), y = subscription_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", width=0.3) +
  labs(title = "Subscription Rate by Marital Status", x = "Marital Status", y = "Subscription Rate (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(subscription_rate, 1), "% (", count, ")")), 
            vjust = -0.5, size = 3.5, color = "black")

# Cross-Analysis of Customer Characteristics


# Combined Cross-Analysis of Age Groups and Occupations
age_job_summary <- df %>%
  group_by(age_group, job) %>%
  summarize(subscription_rate = mean(as.numeric(as.character(y))) * 100, .groups = 'drop') %>%
  arrange(desc(subscription_rate))

ggplot(age_job_summary, aes(x = job, y = subscription_rate, fill = age_group)) +
  geom_bar(stat = "identity", position = "dodge", width = 1.0) +  
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "lightyellow", "lightpink")) +  
  geom_text(aes(label = paste0(round(subscription_rate, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 2, color = "black") +  
  labs(title = "Subscription Rate by Job and Age Group", 
       x = "Job", 
       y = "Subscription Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Education Level and Marital Status

education_marital_summary <- df %>%
  group_by(education, marital) %>%
  summarize(subscription_rate = mean(as.numeric(as.character(y))) * 100, 
            count = n(), 
            .groups = "drop") %>%
  arrange(education, marital)


ggplot(education_marital_summary, aes(x = marital, y = subscription_rate, fill = education)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Subscription Rate by Education Level and Marital Status",
       x = "Marital Status", 
       y = "Subscription Rate (%)") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink", "lightyellow")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  geom_text(aes(label = paste0(round(subscription_rate, 1), "% (", count, ")")), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5, color = "black")  







