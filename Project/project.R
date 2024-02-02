
# 1. Exploratory data analysis :

# Load dataset and relevant libraries =>
# Load relevant libraries
library(readxl) # for data reading
library(dplyr) # for data manipulation
library(ggplot2)
library(randomForest)
library(caret) # Add caret library for train/test splitting
library(reshape2) # Add reshape2 for melt function

# Load dataset
bike_data <- readxl::read_excel("C:/Users/garvit/Desktop/Intern/data.xlsx")
print(bike_data)

# Perform data type conversion on the attributes
bike_data$season <- as.factor(bike_data$season) # Used for classification purposes
bike_data$yr <- as.factor(bike_data$yr)
bike_data$mnth <- as.factor(bike_data$mnth)
bike_data$holiday <- as.factor(bike_data$holiday)
bike_data$weekday <- as.factor(bike_data$weekday)
bike_data$workingday <- as.factor(bike_data$workingday)
bike_data$weathersit <- as.factor(bike_data$weathersit)

# Carry out Missing value analysis
missing_values <- colSums(is.na(bike_data))
print(missing_values)




# 2. Attributes distribution and trends :

# Histograms :

# Histogram for Temperature Distribution
hist(bike_data$temp, col = "navyblue", xlab = "Temperature", ylab = "Frequency", main = "Temperature Distribution")

# Histogram for Humidity Distribution
hist(bike_data$hum, col = "blue", xlab = "Humidity", ylab = "Frequency", main = "Humidity Distribution")

# Histogram for Windspeed Distribution
hist(bike_data$windspeed, col = "dark green", xlab = "Windspeed", ylab = "Frequency", main = "Windspeed Distribution")





# Bar graphs :

# Plot monthly distribution of total number of bikes rented
ggplot(bike_data, aes(x = mnth, y = cnt)) +
  geom_bar(stat = "summary", fun = "sum", fill = "skyblue", color = "black") +
  labs(title = "Monthly Distribution of Total Bikes Rented",
       x = "Month",
       y = "Total Bikes Rented")

# Plot yearly distribution of total number of bikes rented
ggplot(bike_data, aes(x = yr, y = cnt)) +
  geom_bar(stat = "summary", fun = "sum", fill = "lightgreen", color = "black") +
  labs(title = "Yearly Distribution of Total Bikes Rented",
       x = "Year",
       y = "Total Bikes Rented")

# Plot weekday distribution of total number of bikes rented
ggplot(bike_data, aes(x = weekday, y = cnt)) +
  geom_bar(stat = "summary", fun = "sum", fill = "orange", color = "black") +
  labs(title = "Weekday Distribution of Total Bikes Rented",
       x = "Weekday",
       y = "Total Bikes Rented")





# Scatter plots :

# Count with respect to temperature and humidity together
ggplot(bike_data, aes(temp, cnt)) + 
  geom_point(aes(color = hum), alpha = 0.5) +
  labs(title = "Bikes count vs temperature and humidity", x = "Normalized temperature", y = "Count") +
  scale_color_gradientn(colors = c('blue', 'light blue', 'dark blue', 'light green', 'yellow', 'dark orange', 'black')) +
  theme_bw()

# Count with respect to windspeed and weather together
ggplot(bike_data, aes(x = windspeed, y = cnt)) +
  geom_point(aes(color = factor(weathersit)), alpha = 0.5) +  # Convert to factor to ensure it's treated as discrete
  labs(title = "Bikes count vs windspeed and weather", x = "Windspeed", y = "Count") +
  scale_color_manual(values = c('blue', 'light blue', 'dark blue', 'light green', 'yellow', 'dark orange', 'black')) +  # Use scale_color_manual for discrete scale
  theme_bw()

# Count with respect to temperature and season together
ggplot(bike_data, aes(x = temp, y = cnt)) +
  geom_point(aes(color = season), alpha = 0.5) +
  labs(title = "Bikes count vs temperature and season", x = "Normalized temperature", y = "Count") +
  scale_color_manual(values = c('blue', 'light blue', 'dark blue', 'light green', 'yellow', 'dark orange', 'black')) +
  theme_bw()



# Plot monthly distribution of total number of bikes rented =>
ggplot(bike_data, aes(x = mnth, y = cnt)) +
  geom_bar(stat = "summary", fun = "sum", fill = "skyblue", color = "black") +
  labs(title = "Monthly Distribution of Total Bikes Rented",
       x = "Month",
       y = "Total Bikes Rented")

# Plot yearly distribution of total number of bikes rented =>
ggplot(bike_data, aes(x = yr, y = cnt)) +
  geom_bar(stat = "summary", fun = "sum", fill = "lightgreen", color = "black") +
  labs(title = "Yearly Distribution of Total Bikes Rented",
       x = "Year",
       y = "Total Bikes Rented")


# Plot boxplot for outliers' analysis =>
# We will consider only numeric variables for boxplot
numeric_vars <- bike_data[, sapply(bike_data, is.numeric)]
# Remove 'instant' column as it is just an index
numeric_vars <- numeric_vars[, -1]

# Melt the data, specifying 'instant' as the id variable
melted_data <- melt(numeric_vars, id.vars = NULL)

ggplot(melted_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot for Outliers' Analysis",
       x = "Variable",
       y = "Value")





# 3. Split the dataset into training and testing sets :

# Split the dataset
set.seed(123) # for reproducibility
train_index <- createDataPartition(y = bike_data$cnt, p = 0.8, list = FALSE)
train_data <- bike_data[train_index, ]
test_data <- bike_data[-train_index, ]





# 4. Create a model using Random Forest Algorithm :

# Specify the formula for the model
# Here, we predict 'cnt' based on other variables
formula <- cnt ~ season + yr + mnth + holiday + weekday +
  workingday + weathersit + temp + atemp + hum + windspeed +
  casual + registered

# Random Forest

# Train the Random Forest model
rf_model <- randomForest(formula, data = train_data)

# Print the summary of the model
print(rf_model)




# 5.Predict the performance of the model on the test dataset
# Predict on the test dataset
predictions <- predict(rf_model, newdata = test_data)
print(predictions)

# Evaluate the model (e.g., using RMSE)
rmse <- sqrt(mean((predictions - test_data$cnt)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

