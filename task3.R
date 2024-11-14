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

# Convert the "month" column to a numeric value and calculate the subscription rate by year and month
df <- df %>%
  mutate(month_num = recode(month,
                            "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4,
                            "May" = 5, "Jun" = 6, "Jul" = 7,
                            "Aug" = 8, "Sep" = 9, "Oct" = 10,
                            "Nov" = 11, "Dec" = 12)) %>%
  group_by(year, month_num) %>%
  summarize(subscription_rate = mean(y) * 100) %>%
  arrange(year, month_num)


# Plot the time series chart
ggplot(df, aes(x = month_num, y = subscription_rate, color = as.factor(year), group = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(subscription_rate, 1)),  
            vjust = -0.5, size = 3, color="black") + 
  labs(title = "Subscription Rate by Month and Year",
       x = "Month",
       y = "Subscription Rate (%)",
       color = "Year") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal()
