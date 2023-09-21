# 1. Branch Performance Analysis:
#    How does the sales performance of each branch (Branch A, Branch B, Branch C) compare, and are there specific strategies that the top-performing branch can share with the others?

# 2. Customer Loyalty and Sales:
#    Does the data show that members, who use member cards, exhibit higher loyalty and contribute significantly more to total sales compared to normal customers?

# 3. Gender-Based Marketing Strategy:
#    Are there gender-specific marketing strategies that can be developed based on the sales data? For example, should the supermarket tailor promotions or product offerings differently for male and female customers?

# 4. Product Line Optimization:
#    Which product categories are driving the highest sales, and conversely, are there underperforming categories that may require adjustments or discontinuation?

# 5. Payment Method and Sales Conversion:
#    Does the choice of payment method affect the likelihood of customers making larger purchases? Are there insights into how different payment methods influence customer spending habits?

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

# Loading the dataset
data <- read.csv("C:/Users/agarw/Downloads/supermarket_sales - Sheet1.csv")

# Getting information about the dataset
head(data)
str(data)
summary(data)

# Checking for missing/null values in the dataset
null_values <- sapply(data, function(x) sum(is.na(x)))

# Display columns with missing values and their count
null_values[null_values > 0]

# Detect Outliers
# Define a function to detect outliers using the IQR method
detect_outliers <- function(data, columns) {
  outliers <- list()
  for (col in columns) {
    q1 <- quantile(data[[col]], 0.25)
    q3 <- quantile(data[[col]], 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    outliers[[col]] <- data[[col]][data[[col]] < lower_bound | data[[col]] > upper_bound]
  }
  return(outliers)
}

# Specify the columns you want to check for outliers (e.g., numeric columns)
numeric_columns <- c("Unit.price", "Quantity", "Tax.5.", "Total", "cogs", "gross.income", "Rating")

# Detect outliers in the specified columns
outliers_before_removal <- detect_outliers(data, numeric_columns)

# Display the number of outliers in each column
for (col in numeric_columns) {
  cat("Number of outliers in", col, ":", length(outliers_before_removal[[col]]), "\n")
}

# Remove Outliers Using IQR
# Define a function to remove outliers from the dataset
remove_outliers <- function(data, columns) {
  for (col in columns) {
    q1 <- quantile(data[[col]], 0.25)
    q3 <- quantile(data[[col]], 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    data <- data[!(data[[col]] < lower_bound | data[[col]] > upper_bound), ]
  }
  return(data)
}

# Remove outliers from the specified columns
data_cleaned <- remove_outliers(data, numeric_columns)

# Recheck for Outliers
# Detect outliers in the cleaned dataset
outliers_after_removal <- detect_outliers(data_cleaned, numeric_columns)

# Display the number of outliers in each column after removal
for (col in numeric_columns) {
  cat("Number of outliers in", col, "after removal:", length(outliers_after_removal[[col]]), "\n")
}

# Histograms to visualize the distribution of numeric variables
numeric_columns <- c("Unit.price", "Quantity", "Tax.5.", "Total", "cogs", "gross.income", "Rating")

par(mfrow=c(2, 4))
for (col in numeric_columns) {
  hist(data_cleaned[[col]], main=col, xlab=col, col="lightblue", border="black")
}

# Box plots to compare numeric variables by branch
par(mfrow=c(2, 4))
for (col in numeric_columns) {
  boxplot(data_cleaned[[col]] ~ data_cleaned$Branch, data=data_cleaned, 
          main=paste(col, "by Branch"), xlab="Branch", ylab=col, col="lightblue")
}

# Bar plot to show sales by gender
gender_sales <- aggregate(Total ~ Gender, data_cleaned, sum)
barplot(gender_sales$Total, names.arg=gender_sales$Gender, 
        main="Total Sales by Gender", xlab="Gender", ylab="Total Sales")

# Pie chart to visualize payment methods
payment_counts <- table(data_cleaned$Payment)
pie(payment_counts, labels=names(payment_counts), 
    main="Payment Method Distribution", col=rainbow(length(payment_counts)))

# Time series plot for sales over time
data_cleaned$Date <- as.Date(data_cleaned$Date, format="%m/%d/%Y")
time_series <- aggregate(Total ~ Date, data_cleaned, sum)
plot(time_series$Date, time_series$Total, type="l", 
     main="Total Sales Over Time", xlab="Date", ylab="Total Sales")

# Correlation heatmap for numeric variables
correlation_matrix <- cor(data_cleaned[, numeric_columns])
library(corrplot)
corrplot(correlation_matrix, method="color", type="upper", 
         main="Correlation Heatmap for Numeric Variables")


# Create a bar plot to compare sales performance by branch
ggplot(data, aes(x = Branch, y = Total, fill = Branch)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Branch Sales Performance Comparison",
       x = "Branch", y = "Average Total Sales")

# Perform ANOVA test to compare branch performance
anova_result <- aov(Total ~ Branch, data = data)
summary(anova_result)


# Create a bar plot to compare average total sales for Member and Normal customers
ggplot(data, aes(x = Customer.type, y = Total, fill = Customer.type)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Total Sales by Customer Type",
       x = "Customer Type", y = "Average Total Sales")

# Separate data for Member and Normal customers
member_data <- subset(data, Customer.type == "Member")
normal_data <- subset(data, Customer.type == "Normal")

# Perform t-test to compare average total sales between Member and Normal customers
t_test_result <- t.test(member_data$Total, normal_data$Total)
t_test_result

# Create a bar plot to compare total sales for Male and Female customers
ggplot(data, aes(x = Gender, y = Total, fill = Gender)) +
  geom_bar(stat = "sum", position = "dodge") +
  labs(title = "Total Sales by Gender",
       x = "Gender", y = "Total Sales")

# Separate data for Male and Female customers
male_data <- subset(data, Gender == "Male")
female_data <- subset(data, Gender == "Female")

# Perform t-test to compare total sales between Male and Female customers
t_test_result <- t.test(male_data$Total, female_data$Total)
t_test_result

# Create a bar plot to compare total sales by product line
ggplot(data, aes(x = Product.line, y = Total, fill = Product.line)) +
  geom_bar(stat = "sum") +
  labs(title = "Total Sales by Product Line",
       x = "Product Line", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform one-way ANOVA to compare total sales among product categories
anova_result <- aov(Total ~ Product.line, data = data)
summary(anova_result)


# Create a box plot to compare total sales by payment method
ggplot(data, aes(x = Payment, y = Total, fill = Payment)) +
  geom_boxplot() +
  labs(title = "Total Sales by Payment Method",
       x = "Payment Method", y = "Total Sales")

# Separate data for different payment methods
ewallet_data <- subset(data, Payment == "Ewallet")
cash_data <- subset(data, Payment == "Cash")
credit_card_data <- subset(data, Payment == "Credit card")

# Perform ANOVA test to compare total sales among payment methods
anova_result <- aov(Total ~ Payment, data = data)
summary(anova_result)