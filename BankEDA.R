rm(list = ls())

library(tidyverse)
#loading data 
data <- read.csv('C:\\Users\\ELBOSTAN\\Rwork\\Bank.csv')

view(data)
#columns name
columnname <- colnames(data)
print(columnname)
#types of customers
attrition_counts <- table(data$Attrition_Flag)
print(attrition_counts)
#ploting it
attrition_counts_df <- as.data.frame(attrition_counts)
colnames(attrition_counts_df) <- c("Attrition_Flag", "Count")
ggplot(attrition_counts_df, aes(x = Attrition_Flag, y = Count, fill = Attrition_Flag)) +
  geom_bar(stat = "identity") +
  labs(title = "Counts of Each Type of Client",
       x = "Customers Type",
       y = "Count") +
  theme_minimal()
#group by gender and income category
customer_counts <- data %>%
  group_by(Gender, Income_Category) %>%
  summarise(count = n())

#histogram for customers age
hist(data$Customer_Age, 
     main = "Histogram of Customer Ages", 
     xlab = "Age", 
     ylab = "Frequency",
     col = "skyblue",
     border = "white",
     breaks = 10)
#using ggplot is much pretty 
ggplot(data, aes(x = Customer_Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Customer Ages",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

#making a Boxplot for customer ages
ggplot(data, aes(y = Customer_Age)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Boxplot of Customer Ages",
       y = "Age") +
  theme_minimal()

#histogram for dependent number
ggplot(data, aes(x = Dependent_count)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "white") +
  labs(title = "Histogram of Dependent Number",
       x = "Number of Dependent",
       y = "Frequency") +
  theme_minimal()

# Create a summary table of education levels and their counts
education_counts <- table(data$Education_Level)

# Convert the table to a dataframe
education_counts_df <- as.data.frame(education_counts)
colnames(education_counts_df) <- c("Education_Level", "Count")
View(education_counts_df)

# Calculate percentages
education_counts_df$Percentage <- education_counts_df$Count / sum(education_counts_df$Count) * 100

# Create a pie chart
ggplot(education_counts_df, aes(x = "", y = Count, fill = Education_Level)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Each Education Level",
       fill = "Education Level",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = "right")

#Filter only females
female_customers <- data %>%
  filter(Gender == "F")

#Group females by marital status and count the number of customers in each group
female_marital_counts <- female_customers %>%
  group_by(Marital_Status) %>%
  summarise(count = n())
print(female_marital_counts)

#Making Pie chart for female martial status
ggplot(female_marital_counts, aes(x = "", y = count, fill = Marital_Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 45) +
  geom_text(aes(label = paste0(Marital_Status, ": ", count)),
            position = position_stack(vjust = 0.5),
            hjust = 0.5) + 
  labs(title = "Proportion of Marital Status among Female Customers",
       fill = "Marital Status",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = "right")

# Calculate mean, max, and min for Credit_Limit
mean_credit_limit <- mean(data$Credit_Limit)
max_credit_limit <- max(data$Credit_Limit)
min_credit_limit <- min(data$Credit_Limit)

# Print the results
cat("Mean Credit Limit:", mean_credit_limit, "\n")
cat("Max Credit Limit:", max_credit_limit, "\n")
cat("Min Credit Limit:", min_credit_limit, "\n")

#boxplot for credit card limit
ggplot(data, aes(y = Credit_Limit)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Boxplot of Credit_Limit",
       y = "limit") +
  theme_minimal()

#ploting a bar chart of avg credit limit to income category
ggplot(data, aes(x = Income_Category, y = Credit_Limit)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black") +
  labs(title = "Average Credit Limit by Income Category",
       x = "Income Category",
       y = "Average Credit Limit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
