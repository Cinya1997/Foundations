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


# for TASK2
df <- df %>%
  # Convert binary variables to numeric type
  mutate(
    default = ifelse(default == "yes", 1, 0),
    housing = ifelse(housing == "yes", 1, 0),
    loan = ifelse(loan == "yes", 1, 0)
  ) %>%

# scale balance variable
  mutate(balance = scale(balance))

# for TASK3
# It is known that this data is from May 2008 to November 2010,
#but the original data is indeed in the year column, so it needs to be supplemented
#Process directly in csv, add one column "year"

#Combines year, month, and day into a complete date
#Change the month abbreviation to capital letter (in csv file)
df <- df %>%
  mutate(month = recode(month,
                        "Jan" = "1", "Feb" = "2", "Mar" = "3", "Apr" = "4",
                        "May" = "5", "Jun" = "6", "Jul" = "7",
                        "Aug" = "8", "Sep" = "9", "Oct" = "10",
                        "Nov" = "11", "Dec" = "12")) %>%
  mutate(date = paste(year, month, day, sep = "-")) %>%
  mutate(df = as.Date(date, format = "%Y-%b-%d"))

# FOR TASK4
# Replace -1 in `pdays` with NA, indicating that
# the customer has not participated in the previous marketing campaign

df <- df %>%
  mutate(pdays = ifelse(pdays == -1, NA, pdays))

# convert "contact" and "poutcome" to factor
df <- df %>%
  mutate(contact = factor(contact, levels = c("unknown", "telephone", "cellular")),
         poutcome = factor(poutcome, levels = c("unknown", "other", "failure", "success")))
# scale "duration""campaign"
# In particular, if you are using a tree-based model such as a decision tree or random forest,
# you do not need to standardize

df$duration <- scale(df$duration)
df$campaign <- scale(df$campaign)
df$pdays <- scale(df$pdays)

head(df)
# Delete the original columns that are no longer needed
df$df <- NULL
df$year <- NULL
df$month <- NULL
df$day <- NULL
df$age <- NULL
# move "y" to the first column
df <- df %>% select(y, everything())







