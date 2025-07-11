# Load necessary libraries
library(dplyr)

# Load the dataset
gold_data <- read.csv("Gold.csv")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(knitr)
colnames(gold_data)

# Convert the 'Date' column to Date format if needed
gold_data$Date <- as.Date(gold_data$Date, format="%m/%d/%Y")
summary_statistics <- summary(gold_data[, c("Close.Last", "Volume", "Open", "High", "Low")])
print(summary_statistics)

stddev_and_range_table <- data.frame(
  Variable = c("Close", "Volume", "Open", "High", "Low"),
  StdDev = c(sd(gold_data$Close.Last, na.rm = TRUE),
             sd(gold_data$Volume, na.rm = TRUE),
             sd(gold_data$Open, na.rm = TRUE),
             sd(gold_data$High, na.rm = TRUE),
             sd(gold_data$Low, na.rm = TRUE)),
  Range = c(diff(range(gold_data$Close.Last, na.rm = TRUE)),
            diff(range(gold_data$Volume, na.rm = TRUE)),
            diff(range(gold_data$Open, na.rm = TRUE)),
            diff(range(gold_data$High, na.rm = TRUE)),
            diff(range(gold_data$Low, na.rm = TRUE)))
)

# Print the table
print(stddev_and_range_table)


library(lubridate)
# Convert Date column to Date format
gold_data$Date <- as.Date(gold_data$Date, format = "%m/%d/%Y")


gold_data$Year <- format(gold_data$Date, "%Y")
yearly_counts <- data.frame(table(gold_data$Year))
yearly_counts
colnames(yearly_counts) <- c("Year", "Frequency")

# Plot the frequency distribution by year
ggplot(yearly_counts, aes(x = Year, y = Frequency)) +
  geom_bar(stat = "identity", fill = "maroon", color = "black") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 3.5) +
  labs(title = "Frequency of Observations by Year", x = "Year", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(lubridate)
library(tidyr)

#response plot 
### 1. Plot: Time Series of Closing Prices (Trends)
ggplot(gold_data, aes(x = Date, y = Close.Last)) +
  geom_line(color = "blue", linewidth = 1) +  # Replace 'size' with 'linewidth'
  labs(title = "Gold Closing Prices Over Time",
       x = "Date",
       y = "Closing Price") +
  theme_minimal()

# 2.Plot Opening and Closing Prices
ggplot(data = gold_data) +
  geom_line(aes(x = Date, y = Open, color = "Open Price"), linewidth = 1) +
  geom_line(aes(x = Date, y = Close.Last, color = "Close Price"), linewidth = 1) +
  scale_color_manual(values = c("Open Price" = "darkorange", "Close Price" = "steelblue")) + 
  labs(title = "Opening & Closing Prices of Gold",
       x = NULL,
       y = NULL,
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 3.Plot High and Low Prices
ggplot(data = gold_data) +
  geom_line(aes(x = Date, y = High, color = "High Price"), linewidth = 1) +
  geom_line(aes(x = Date, y = Low, color = "Low Price"), linewidth = 1) +
  scale_color_manual(values = c("High Price" = "red", "Low Price" = "blue")) +  # Custom colors
  labs(title = "High & Low Prices of The Gold",
       x = NULL,  # Remove x-axis label
       y = NULL,  # Remove y-axis label
       color = "Legend") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        legend.position = "bottom")  # Place legend at the bottom

#4. Plot all features together

gold_data_long <- gold_data %>%
  pivot_longer(cols = c(Close.Last, Open, High, Low),
               names_to = "Feature",
               values_to = "Value")
gold_data_long

ggplot(gold_data_long, aes(x = Date, y = Value, color = Feature)) +
  geom_line(linewidth = 1) +  # Replace 'size' with 'linewidth'
  labs(title = "Gold Prices: Close, Open, High, and Low Over Time",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(legend.position = "bottom")


### 5. Plot: Highlighting Volatility (Daily Price Range)
gold_data$DailyRange <- gold_data$High - gold_data$Low

ggplot(gold_data, aes(x = Date, y = DailyRange)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "Daily Price Range Over Time",
       x = "Date",
       y = "Price Range (High - Low)") +
  theme_minimal()

# 6. Plot the Sales Volume of Gold
gold_data_clean <- gold_data[!is.na(gold_data$Volume), ]
ggplot(data = gold_data_clean, aes(x = Date, y = Volume)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(title = "Volume of The Gold",
       x = NULL,  # Remove x-axis label
       y = "Volume") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# 6. Decomposition Plot

gold_data <- gold_data[order(gold_data$Date), ]
gold_ts <- ts(gold_data$Close.Last, frequency = 365, start = c(2014, 1))  # Assuming daily data
decomposed_gold <- decompose(gold_ts)
par(mfrow = c(4, 1), mar = c(3, 4, 2, 2), oma = c(1, 1, 4, 1))  # Adjust outer margin for the title
plot(decomposed_gold$x, type = "l", main = "", ylab = "Observed", xlab = "")
plot(decomposed_gold$trend, type = "l", main = "", ylab = "Trend", xlab = "")
plot(decomposed_gold$seasonal, type = "l", main = "", ylab = "Seasonal", xlab = "")
plot(decomposed_gold$random, type = "l", main = "", ylab = "Residual", xlab = "Time")

mtext("Time Series Decomposition of Gold Closing Prices", side = 3, line = 1, outer = TRUE, cex = 1.2, font = 2)


#Correlation Matrix

# Select numerical columns for correlation analysis
gold_numeric <- gold_data[, c("Close.Last", "Open", "High", "Low", "Volume")]

gold_numeric <- as.data.frame(lapply(gold_numeric, function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  return(x)
}))

# Replace NA values with the mean of each column
gold_numeric <- as.data.frame(lapply(gold_numeric, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)  # Replace NA with the column mean
  return(x)
}))
#correlation matrix
cor_matrix <- cor(gold_numeric, use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)
#correlation heatmap
cor_melt <- melt(cor_matrix)  # Melt the correlation matrix for ggplot

ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix Heatmap",
       x = "",
       y = "",
       fill = "Correlation") +
  theme_minimal()

#Adress missing values
unique(gold_data$Volume)
# Replace non-standard missing values with NA
gold_data$Volume[gold_data$Volume == ""] <- NA  # Empty strings
gold_data$Volume[gold_data$Volume == "NA"] <- NA  # Strings "NA"
gold_data$Volume[gold_data$Volume == "N/A"] <- NA  # Strings "N/A"
# Count missing values in the Volume column
sum(is.na(gold_data$Volume))
gold_data$Volume[is.na(gold_data$Volume)] <- median(gold_data$Volume, na.rm = TRUE)

# Recheck for missing values in all columns
missing_summary <- colSums(is.na(gold_data))
print("Missing Values per Column:")
print(missing_summary)

# Define a function to detect outliers using IQR

# Check the structure of the dataset
str(gold_data)

# Confirm that all columns are numeric
sapply(gold_data[, c("Close.Last", "Volume", "Open", "High", "Low")], is.numeric)
# Convert relevant columns to numeric
gold_data <- gold_data %>%
  mutate(
    Close.Last = as.numeric(as.character(Close.Last)),
    Volume = as.numeric(as.character(Volume)),
    Open = as.numeric(as.character(Open)),
    High = as.numeric(as.character(High)),
    Low = as.numeric(as.character(Low))
  )


detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(column < lower_bound | column > upper_bound)
}

# Apply the function to numerical columns
gold_data <- gold_data %>%
  mutate(
    Close_Last_Outlier = detect_outliers(Close.Last),
    Volume_Outlier = detect_outliers(Volume),
    Open_Outlier = detect_outliers(Open),
    High_Outlier = detect_outliers(High),
    Low_Outlier = detect_outliers(Low)
  )

# Count outliers in each column
outlier_counts <- colSums(select(gold_data, ends_with("_Outlier")))
print("Outlier Counts per Column:")
print(outlier_counts)

# Visualize outliers with boxplots
boxplot(gold_data$Close.Last, main = "Boxplot of Close/Last", ylab = "Close/Last")
boxplot(gold_data$Volume, main = "Boxplot of Volume", ylab = "Volume")
boxplot(gold_data$Open, main = "Boxplot of Open", ylab = "Open")
boxplot(gold_data$High, main = "Boxplot of High", ylab = "High")
boxplot(gold_data$Low, main = "Boxplot of Low", ylab = "Low")



#ARIMA MODEL

library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
str(gold_data$Date) 
gold_ts <- ts(gold_data$Close.Last, frequency = 365, start = c(2014, 1)) 

# Plot the time series with Date
ggplot(gold_data, aes(x = Date, y = Close.Last)) +
  geom_line(color = "grey34", linewidth = 1) +
  labs(title = "Gold Closing Prices Over Time",
       x = "Date",
       y = "Closing Price") +
  theme_minimal()
#stationarity test
adf_test <- adf.test(gold_data$Close.Last)
print(adf_test)
#p-value is larger then 0.05 , the series not stationary 

# ıt is not stationary Apply differencing
gold_data$Close_Last_Diff <- c(NA, diff(gold_data$Close.Last))

# Fark alınmış seriden eksik değerleri kaldırarak ADF testi uygulayın
adf_test_diff <- adf.test(na.omit(gold_data$Close_Last_Diff))
print(adf_test_diff)

library(forecast)

# Plot ACF and PACF for the differenced series
Acf(gold_data$Close_Last_Diff, na.action = na.pass, main = "ACF of Differenced Closing Prices ")
Pacf(gold_data$Close_Last_Diff, na.action = na.pass, main = "PACF of Differenced Closing Prices")

# Option 1: Manually specify ARIMA parameters
arima_model <- Arima(gold_data$Close.Last, order = c(1, 1, 1))  # Replace p and q with your values
summary(arima_model)

# Option 2: Automatically determine ARIMA parameters
auto_arima_model <- auto.arima(gold_data$Close.Last, seasonal = FALSE)  # Non-seasonal ARIMA
summary(auto_arima_model)


# Forecast future values (e.g., for the next 30 days)
forecast_values <- forecast(arima_model, h = 30)  
print(forecast_values)
forecast_df <- data.frame(
  Date = seq(max(gold_data$Date) + 1, by = "day", length.out = 30),
  Forecast = as.numeric(forecast_values$mean),
  Lower = as.numeric(forecast_values$lower[, 2]),
  Upper = as.numeric(forecast_values$upper[, 2])
)

ggplot() +
  geom_line(data = gold_data, aes(x = Date, y = Close.Last), color = "black") +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue") +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.5) +
  labs(title = "Gold Price Forecast",
       x = "Date",
       y = "Closing Price") +
  theme_minimal()
# ZOOMING for predicted values
ggplot() +
  geom_line(data = gold_data, aes(x = Date, y = Close.Last), color = "black", alpha = 0.6) +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", linewidth = 1.5) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.5) +
  scale_x_date(limits = c(min(forecast_df$Date) - 30, max(forecast_df$Date))) +  
  labs(title = "Gold Price Forecast (Zoomed on Prediction)",
       x = "Date",
       y = "Closing Price") +
  theme_minimal()

# Check residuals
checkresiduals(auto_arima_model)

# Perform Ljung-Box test
Box.test(residuals(arima_model), lag = 10, type = "Ljung-Box")

# Evaluate model performance
accuracy(arima_model)


# t-değerini hesaplama
t_ar1 <- -0.0335 / 0.3399
t_ma1 <- -0.0067 / 0.3393

# t-değerlerini yazdırma
print(t_ar1)
print(t_ma1)






