# Load necessary libraries
library(tidyverse)
library(lubridate)
library(zoo)

# Load the dataset
df <- read.csv("apple_stock_prices.csv")

# Inspect the first few rows of the Date column
head(df$Date)

# Convert the 'Date' column to Date format using multiple formats
df$Date <- parse_date_time(df$Date, orders = c("ymd", "mdy", "dmy"))

# Check for any NA values introduced during conversion
sum(is.na(df$Date))

# Inspect rows with NA dates
df %>% filter(is.na(Date))

# Drop rows with NA dates
df <- df %>% drop_na(Date)

# Create the 'Month' column
df$Month <- month(df$Date, label = TRUE)

# Check the first few rows of the dataset after conversion
head(df)

# Check the data type of the 'Close' column
str(df$Close)

# Convert the 'Close' column to numeric
df$Close <- as.numeric(df$Close)

# Calculate moving averages for the 'Close' prices
df_company <- df

# Calculate moving averages for the 'Close' prices
df_company <- df_company %>%
  arrange(Date) %>%
  mutate(
    MA5 = rollmean(Close, k = 5, fill = NA, align = "right"),
    MA10 = rollmean(Close, k = 10, fill = NA, align = "right"),
    MA20 = rollmean(Close, k = 20, fill = NA, align = "right"),
    Quarterly_MA = rollmean(Close, k = 63, fill = NA, align = "right"),  # Approx. 63 trading days in a quarter
    Four_Quarter_MA = rollmean(Close, k = 252, fill = NA, align = "right")  # Approx. 252 trading days in four quarters
  )
# Summary Statistics
# Summary statistics for the dataset
summary(df)

# Summary statistics for specific columns
summary(df$Close)
summary(df$Volume)

# Univariate Analysis
# Ensure the 'Close' column is numeric
# Identify non-numeric values in the 'Close' column
non_numeric_values <- df$Close[!grepl("^[0-9.]+$", df$Close)]
print(non_numeric_values)

# Check for any NA values introduced during conversion
sum(is.na(df$Close))

# Histogram of Closing Prices
ggplot(df, aes(x = Close)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Closing Prices", x = "Closing Price", y = "Frequency")

# Boxplot of Closing Prices
ggplot(df, aes(y = Close)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Closing Prices", y = "Closing Price")

# Bivariate Analysis
# Scatter plot of Opening vs Closing Prices
ggplot(df, aes(x = Open, y = Close)) +
  geom_point(color = "red") +
  labs(title = "Opening vs Closing Prices", x = "Opening Price", y = "Closing Price")

# Boxplot of Closing Prices by Month
df$Month <- month(df$Date)
ggplot(df, aes(x = factor(Month), y = Close)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Boxplot of Closing Prices by Month", x = "Month", y = "Closing Price")

# Correlation Analysis
# Correlation matrix
cor_matrix <- cor(df %>% select(Open, High, Low, Close, Volume), use = "complete.obs")

# Correlation heatmap
library(reshape2)
melted_cor_matrix <- melt(cor_matrix)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "")

# Time Series Analysis
# Time series plot of Closing Prices
ggplot(df, aes(x = Date, y = Close)) +
  geom_line(color = "purple") +
  labs(title = "Time Series Analysis of Closing Prices", x = "Date", y = "Closing Price")

# Time series plot of Closing Prices with Moving Averages
ggplot(df_company, aes(x = Date)) +
  geom_line(aes(y = Close), color = "blue") +
  geom_line(aes(y = MA5), color = "red") +
  geom_line(aes(y = MA10), color = "green") +
  geom_line(aes(y = MA20), color = "orange") +
  geom_line(aes(y = Quarterly_MA), color = "purple") +
  geom_line(aes(y = Four_Quarter_MA), color = "brown") +
  labs(title = "Closing Prices with Moving Averages", x = "Date", y = "Price")

# Save the cleaned dataset to a CSV file
write.csv(df, "/Users/shahlyfayeek/Desktop/DVC/DVC_CW/cleaned_apple_stock_prices.csv", row.names = FALSE)

