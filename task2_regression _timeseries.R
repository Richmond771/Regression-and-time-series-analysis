# ==========================================
# Level 2 - Task 1: Regression Analysis
# Boston Housing Price Prediction
# ==========================================

# Clear environment
rm(list = ls())

# ------------------------------------------
# Load Dataset (Space-Separated File)
# ------------------------------------------
tt <- read.table(file.choose(),
                 header = FALSE,
                 sep = "",
                 stringsAsFactors = FALSE)

# Assign Column Names
colnames(tt) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX",
                  "RM", "AGE", "DIS", "RAD", "TAX",
                  "PTRATIO", "B", "LSTAT", "MEDV")

# Check structure
str(tt)

# ------------------------------------------
# Data Quality Checks
# ------------------------------------------

# Missing values
colSums(is.na(tt))

# Duplicate rows
sum(duplicated(tt))

# ------------------------------------------
# Train-Test Split (80/20)
# ------------------------------------------
set.seed(123)

train_index <- sample(seq_len(nrow(tt)),
                      size = 0.8 * nrow(tt))

train_data <- tt[train_index, ]
test_data  <- tt[-train_index, ]

# ------------------------------------------
# Fit Linear Regression Model
# ------------------------------------------
model <- lm(MEDV ~ ., data = train_data)

summary(model)

# ------------------------------------------
# Model Evaluation
# ------------------------------------------

# Predictions
y_pred <- predict(model, newdata = test_data)

# Mean Squared Error
mse <- mean((test_data$MEDV - y_pred)^2)

# R-Squared
r_squared <- 1 - sum((test_data$MEDV - y_pred)^2) /
  sum((test_data$MEDV - mean(test_data$MEDV))^2)

# Print Results
mse
r_squared

# ==========================================
# Level 2 - Task 2: Time Series Analysis
# Stock Price Analysis
# ==========================================

# Load dataset
stock <- read.csv(file.choose())

# Convert date column
stock$date <- as.Date(stock$date)

# Select Apple stock
aapl <- stock[stock$symbol == "AAPL", ]

# Sort by date
aapl <- aapl[order(aapl$date), ]

# Plot closing price
plot(aapl$date, aapl$close,
     type = "l",
     main = "Apple Closing Stock Price",
     xlab = "Date",
     ylab = "Closing Price")

# Moving average (30-day)
aapl$ma_30 <- stats::filter(aapl$close, rep(1/30, 30))

plot(aapl$date, aapl$close,
     type = "l",
     main = "Apple Closing Price with 30-Day Moving Average",
     xlab = "Date",
     ylab = "Closing Price")

lines(aapl$date, aapl$ma_30, col = "red", lwd = 2)

# Decomposition
aapl_ts <- ts(aapl$close, frequency = 252)
decomp <- decompose(aapl_ts)

plot(decomp)


