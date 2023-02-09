## Franciszek Ruszkowski
## k21197589

library(ggplot2)
library(tidyverse)
library(corrplot)
library(dplyr)
library(outliers)
library(psych)
library(graphics)
library(nortest)
library(stats)
library(car)

df <- read.csv(file = "~/Desktop/RSTUDIO/cars_used.csv")

# Making the explained variable at the end 
df <- df[,c(1,2,4,5,6,7,8,9,10,3)]

plot_histogram <- function(x) {
  hist(x, breaks = "FD", main = "Histogram of Continuous Variable", xlab = "Variable Values", ylab = "Frequency")
}

plot_histogram(df$price)

### Quick Check For null values 

check_for_null_values <- function(df) {
  # Get a logical vector indicating which elements are NA
  null_values <- is.na(df)
  
  # Return the sum of all null values in the data frame
  return(sum(null_values))
}

null_values <- check_for_null_values(df)
print(null_values)




### Drawing plots to see anything else 
# Loop through all variables in the data set
par(mfrow = c(2, 2))

plotDensity <- function(df, var, main = NULL) {
  # Extract the variable from the dataframe
  x <- df[, var]
  n <- length(x)
  if (n < 10) {
    # Skip creating a plot if the number of observations is less than 10
    return()
  }
  
  # Create the density plot
  if (is.null(main)) {
    plot(density(x), main = paste("Density Plot of", var), cex = 1.5)
  } else {
    plot(density(x), main = main, cex = 1.5)
  }
}


plotDensity(df, "price")
plotDensity(df, "mileage")
plotDensity(df, "mpg")

## Worth removing outliers for mileage, and for MPG ?    ## !!! 

plotDiscrete <- function(df, var, nbins = 50) {
  # Extract the variable from the dataframe
  x <- df[, var]
  
  # Create a histogram plot with a larger number of bins
  hist(x, breaks = nbins, main = paste("Histogram of", var))
}

plotDiscrete(df,"year")
plotDiscrete(df,"tax")
plotDiscrete(df,"engineSize")


## Categorical 

plotCategoryCounts <- function(df, var) {
  # Extract the variable from the dataframe
  x <- df[, var]
  
  # Count the number of occurrences of each category
  counts <- table(x)
  
  # Create a bar plot of the counts
  barplot(counts, main = paste("Counts of categories for", var))
}

plotCategoryCounts(df, "fuelType")
plotCategoryCounts(df, "transmission")
plotCategoryCounts(df, "model")
plotCategoryCounts(df, "Make")


plotCategoryDensity <- function(df, var) {
  # Extract the variable and the car price from the dataframe
  x <- df[, var]
  y <- df[, "price"]
  
  # Get the unique categories in the variable
  categories <- unique(x)
  
  # Create a density plot for each category
  for (category in categories) {
    category_df <- df[x == category, ]
    plotDensity(category_df, "price", main = paste("Density plot of price for", category))
  }
}


plotCategoryDensity(df, "fuelType")
plotCategoryDensity(df, "transmission")
plotCategoryDensity(df, "model")
plotCategoryDensity(df, "Make")


## Remove outliers for Price Mileage and MPG


outlierRatio <- function(df, var, cutoff) {
  # Extract the variable from the dataframe
  x <- df[, var]
  
  # Count the number of data points inside the cut-off point
  inside <- sum(x <= cutoff)
  
  # Count the number of data points outside the cut-off point
  outside <- sum(x > cutoff)
  
  # Calculate the ratio of the number of outside data points to the total number of data points
  ratio <- outside / (inside + outside)
  
  # Return the ratio
  return(ratio)
}

findOutlierCutoff <- function(df, var) {
  # Extract the variable from the dataframe
  x <- df[, var]
  
  # Calculate the mean and standard deviation of the variable
  mean <- mean(x)
  sd <- sd(x)
  
  # Calculate the cut-off point for the outliers
  cutoff <- mean + 3 * sd
  
  # Return the cut-off point
  return(cutoff)
}

## PRICE OUTLIERS 

cutoff_price <- findOutlierCutoff(df, "price")
print(cutoff_price)
ratio <- outlierRatio(df, "price", cutoff_price)
print(ratio)

# proceeed to cut off those outliers 
df <- df[df$price <= cutoff_price, ]

## MPG OUTLIERS 
cutoff_mpg <- findOutlierCutoff(df, "mpg")
print(cutoff_mpg)
ratio <- outlierRatio(df, "mpg", cutoff_mpg)
print(ratio)

# proceeed to cut off those outliers 
df <- df[df$mpg <= cutoff_mpg, ]

## mileage OUTLIERS 
cutoff_mileage <- findOutlierCutoff(df, "mileage")
print(cutoff_mileage)
ratio <- outlierRatio(df, "mileage", cutoff_mileage)
print(ratio)

# proceeed to cut off those outliers 
df <- df[df$mileage <= cutoff_mileage, ]

### Also engine sizes where engine size is 0 mistake?

df <- subset(df, engineSize > 0)

plotDensity(df, "price")
plotDensity(df, "mileage")
plotDensity(df, "mpg")

## QQ Plot 
x <- qnorm(ppoints(length(df$price)))

# Create a q-q plot for the "price" variable
qqplot(x, df$price, main = "Q-Q Plot of Price")

lillie_test <- lillie.test(df$price)

# Print the p-value of the test
print(lillie_test$p.value)

## P-value is smaller than 0.05. We continue therefore knowing that Price is normally distributed 

# TESTS functions

# T test
two_sample_t_test <- function(data, var1, var2, group1, group2) {
  
  # Check for independence
  # Independent
  
  # Check for normality
  # qq plots 
  
  # Check for equal variances
  # not all arguments have same legth 
  
  # Perform two-sample t-test
  t_test <- t.test(data[data[,var1] == group1, var2], data[data[,var1] == group2, var2])
  
  # Print the results
  print(t_test)
}

# One sided ANOVA 
# Function to check assumptions and perform one-way ANOVA test
check_assumptions_and_run_ANOVA <- function(data, group_var, y_var) {
  # Check assumptions
  
  # Independence
  ## checked via QQ plots 
  
  # Equal variances
  # not all arguments have the same length 
  
  # Equal sample sizes
  # Checked in count bar plots
  
  # Perform one-way ANOVA test
  print("Performing one-way ANOVA test")
  
  model <- aov(data[,y_var] ~ data[,group_var], data = data)
  
  # Perform the ANOVA test
  return(anova(model))
}

plotCategoryQQ <- function(df, cat_var) {
  
  # Extract the categorical variable and the price variable from the dataframe
  x <- df[, cat_var]
  
  # Create a plot for each category of the categorical variable
  for (level in levels(x)) {
    
    # Subset the data for the current category
    subset_df <- subset(df, x == level)
    
    # Create a QQ plot for the price variable in the current category
    #qqplot(subset_df$price, main = paste("QQ Plot of Price for", level))
    
    ## QQ Plot 
    qq <- qnorm(ppoints(length(subset_df$price)))
    
    # Create a q-q plot for the "price" variable
    qqplot(qq, subset_df$price, main = paste("Q-Q Plot of Price for", level))
  }
}

## Test 1 - does car make have signfiicant imapct on the price 
### PLOT AVG PRICES 
avg_prices <- df %>%
  group_by(Make) %>%
  summarise(mean_price = mean(price))

# Plot the average prices in a bar plot
ggplot(avg_prices, aes(x = Make, y = mean_price)) +
  geom_bar(stat = "identity") +
  labs(x = "Car Make", y = "Average Price")

plotCategoryQQ(df, "Make")

test1 <- check_assumptions_and_run_ANOVA(df, "Make", "price")
print(test1)

# Yes car make has a significant impact 


## Test 2 -Is there a significant difference in the price of cars with 
# different transmission types (automatic vs. manual)?

plotCategoryQQ(df, "transmission")

two_sample_t_test(df, "transmission", "price", "Manual", "Automatic")

## result : There is an impact 

# Test 3 

plotCategoryQQ(df, "fuelType")
two_sample_t_test(df, "fuelType", "price", "Petrol", "Diesel")

## There is impact 

# Test 4 
# mileage and price ?  pearson correlation 

cor(df$price,df$mileage)

# Check for correlation between the variables to avoid multicollinearity in our
# final model (multiple regression)

# test 5 , TAX and price? 
cor(df$price,df$tax)
# Decently Correlated 

# test 6 TAX and transmission  ## ANOVA 

test6 <- check_assumptions_and_run_ANOVA(df, "transmission", "tax")
print(test6)
# Significant impact 

# Test 7 TAX vs diesel and petrol 
two_sample_t_test(df, "fuelType", "tax", "Petrol", "Diesel")
# sign difference

# test 7 TAX and engine size 
model <- lm(tax ~ engineSize, data = df)
summary(model)

# Test 8 Year vs Price 
model <- lm(price ~ year, data = df)
summary(model)



## Does Make have an influence on price
### PLOT AVG PRICES 
avg_prices <- df %>%
  group_by(Make) %>%
  summarise(mean_price = mean(price))

# Plot the average prices in a bar plot
ggplot(avg_prices, aes(x = Make, y = mean_price)) +
  geom_bar(stat = "identity") +
  labs(x = "Car Make", y = "Average Price")

## Test whether the price has impact vs car category 
# YES
# Assumptions ?

# Fit an ANOVA model to the data
model <- aov(price ~ Make, data = df)

# Perform the ANOVA test
anova(model)
# Yes Significant imapct

## Test whether Car model has an impact seperately for AUDI or BMW 

### Auxiliary Function 
drop_column <- function(df, col) {
  # Use the select() function to keep all columns except the specified column
  df_modified <- select(df, -col)
  return(df_modified)
}

## There are lot of models lets see if taking subsets of care makes 
## gives us more information

## Test whether Car model has an impact seperately for AUDI or BMW 
audi <- subset(df,Make=="audi")
audi <- drop_column(audi,"Make")
bmw <- subset(df,Make=="BMW")
bmw <- drop_column(bmw,"Make")
vw <- subset(df,Make=="vw")
vw <- drop_column(vw,"Make")


plotDensity(audi,"price")
plotDensity(bmw,"price")
plotDensity(vw,"price")

# Test 8
# Fit an ANOVA model to the data # Audi 
model_audi <- aov(price ~ model, data = audi)
# Perform the ANOVA test # Audi 
anova(model_audi)
# Significant

# Test 9
# Fit an ANOVA model to the data # BMW 
model_bmw <- aov(price ~ model, data = bmw)
# Perform the ANOVA test # Audi 
anova(model_bmw)
# Significant

# Test 10 
# Fit an ANOVA model to the data # VW 
model_vw <- aov(price ~ model, data = vw)
# Perform the ANOVA test # Audi 
anova(model_vw)
# Significant



### Checking if there is a way to group car models,
## Or assign ordinal / nominal values to the categories

avg_prices <- aggregate(price ~ model, data = audi, mean)
# Create a barplot showing the average price for each car model
ggplot(avg_prices, aes(x = model, y = price)) +
  geom_bar(stat = "identity")

avg_prices <- aggregate(price ~ model, data = bmw, mean)
# Create a barplot showing the average price for each car model
ggplot(avg_prices, aes(x = model, y = price)) +
  geom_bar(stat = "identity")

avg_prices <- aggregate(price ~ model, data = vw, mean)
# Create a barplot showing the average price for each car model
ggplot(avg_prices, aes(x = model, y = price)) +
  geom_bar(stat = "identity")

## How many S4 ans S5 audis there are ? 
s4_rows <- audi %>% filter(model == " S4") %>% nrow()
s5_rows <- audi %>% filter(model == " S5") %>% nrow()

# Print the result
print(s4_rows) # only 6
print(s5_rows) # only 2 

## If there are only a few cars of a given model like only 5 s4's or only 3 s5s
## then the dummy variable has an insignificant impact on the regressions



# I will now write code that shows the count of how many cars of each model there are
## We will try to group cars into sub-grups of models (Like for Audi example it will be
## S or RS instead of seperating RS3 RS5 etc and if there are only like 5 examples of a2 then
## it shall be omitted. We will do a similair analysis for all car MAKES  )

## We will do the following for each care make, 
## as reducing the number of categories for MODELS will be the most crucial 
# step to our overall regression 


countmodel <- function(data, carmake) {
  
  data_new <- data[data$Make == carmake, ]
  model_counts <- table(data_new$model)
  print(model_counts)
  barplot(model_counts, xlab = "Model", ylab = "Count", main = "Number of Cars by Model")
}

### We will do a seperate regression for each car make to see which model dummy 
## variables are insignifcant helping us thereafter group them accordingly 

### PREPARE FOR REGRESSION 
# Auxiliary function
remove_duplicate_columns <- function(df) {
  # Find the column names that occur more than once
  duplicated_columns <- names(df)[duplicated(names(df))]
  
  # Return the number of duplicate columns
  num_duplicates <- length(duplicated_columns)
  
  # If there are no duplicate columns, return the original data frame
  if (num_duplicates == 0) {
    return(df)
  }
  
  # Otherwise, create a new data frame with one of the duplicate columns removed
  new_df <- df[, !names(df) %in% duplicated_columns[1]]
  
  return(new_df)
}


# Encoding function
encode_nominal <- function(data, var) {
  
  dummies <- model.matrix(~ var - 1, data)[, -1]
  data <- cbind(data, dummies)
  return(data)
}

regres_make  <- function(df, carmake) { 

  makedf <- subset(df,Make==carmake)
  makedf <- encode_nominal(makedf, var = makedf$model)
  makedf <- drop_column(makedf,"model")
  makedf <- encode_nominal(makedf, var = makedf$transmission)
  makedf <- drop_column(makedf,"transmission")
  makedf <- encode_nominal(makedf, var = makedf$fuelType)
  makedf <- remove_duplicate_columns(makedf)
  makedf <- drop_column(makedf,"fuelType")
  makedf <- drop_column(makedf,"Make")
  return(makedf)
}

## We will also do linear regression for all 

### AUDI 
countmodel(df,"audi")
regres_audi <- regres_make(df,"audi")
fit <- lm(price ~ ., data = regres_audi)
summary(fit)
anova(fit)

### BMW 
countmodel(df,"BMW")
regres_bmw <- regres_make(df,"BMW")
fit <- lm(price ~ ., data = regres_bmw)
summary(fit)

### Ford 
countmodel(df,"Ford")
regres_ford <- regres_make(df,"Ford")
fit <- lm(price ~ ., data = regres_ford)
summary(fit)

### Volkswagen
countmodel(df,"vw")
regres_vw <- regres_make(df,"vw")
fit <- lm(price ~ ., data = regres_vw)
summary(fit)

## Toyota
countmodel(df,"toyota")
regres_toyota <- regres_make(df,"toyota")
fit <- lm(price ~ ., data = regres_toyota)
summary(fit)


## Skoda 
countmodel(df,"skoda")
regres_skoda <- regres_make(df,"skoda")
fit <- lm(price ~ ., data = regres_skoda)
summary(fit)


## Hyundai 
countmodel(df,"Hyundai")
regres_Hyundai <- regres_make(df,"Hyundai")
fit <- lm(price ~ ., data = regres_Hyundai)
summary(fit)


# NO nominal values, encoding dummy variables
regres <- function(df) {
  
  df <- encode_nominal(df, var = df$Make)
  df <- remove_duplicate_columns(df)
  df <- drop_column(df,"Make")
  
  
  df <- encode_nominal(df, var = df$model)
  df <- remove_duplicate_columns(df)
  df <- drop_column(df,"model")
  
  df <- encode_nominal(df, var = df$transmission)
  df <- remove_duplicate_columns(df)
  df <- drop_column(df,"transmission")
  
  df <- encode_nominal(df, var = df$fuelType)
  df <- remove_duplicate_columns(df)
  df <- drop_column(df,"fuelType")
  
  return(df)
}

reg1 <- regres(df)
fit1 <- lm(price ~ ., data = reg1)
summary(fit1)
anova(fit1)
 
#(6 not defined because of singularities) # "Transit Tourneo", "TT", "Up", "Veloster", "Yaris", "Yeti Outdoor"
df$model <- gsub(" ", "", df$model)
df <- df[!df$model %in% c("TransitTourneo", "TT", "Up", "Veloster", "Yaris", "YetiOutdoor"), ]

## Tiding up the data 

# Replace "Semi-Auto" with "Automatic" in the "transmission" column 
df$transmission[df$transmission == "Semi-Auto"] <- "Automatic"
df$transmission <- ifelse(grepl("Other", df$transmission), "Petrol", df$transmission)

## Group "Other" with Petrol
df$fuelType <- ifelse(grepl("Other", df$fuelType), "Petrol", df$fuelType)

## Group All Tourneos together
df$model <- ifelse(grepl("Tourneo", df$model), "Tourneo", df$model)

## Group audi S's together 
df$model <- ifelse(df$model %in% c("S3", "S4", "S5", "S8"), "S", df$model)

## Group all Caddy  together 
df$model <- ifelse(grepl("Caddy", df$model), "Caddy", df$model)

## Hybrid as Electric 
## Other as Petrol as most popular 
df$fuelType <- ifelse(grepl("Electric", df$fuelType), "Hybrid", df$fuelType)


reg2<- regres(df)
fit2 <- lm(price ~ ., data = reg2)
summary(fit2)
anova(fit2)



### Regression without models at all 
df_without_model <- select(df, -model)



regres_nomodel <- function(df) {
  
  df <- encode_nominal(df, var = df$Make)
  df <- remove_duplicate_columns(df)
  df <- drop_column(df,"Make")
  
  df <- encode_nominal(df, var = df$transmission)
  df <- remove_duplicate_columns(df)
  df <- drop_column(df,"transmission")
  
  df <- encode_nominal(df, var = df$fuelType)
  df <- remove_duplicate_columns(df)
  df <- drop_column(df,"fuelType")
}

reg_without_model <- regres_nomodel(df_without_model)
fit_reg_without_model <- lm(price ~ ., data = reg_without_model)
summary(fit_reg_without_model)
anova(fit_reg_without_model)


rmse1 <- sqrt(mean((reg1$price - fit1$fitted.values)^2))
rmse2 <- sqrt(mean((reg2$price - fit2$fitted.values)^2))
rmse3 <- sqrt(mean((reg_without_model$price - fit_reg_without_model$fitted.values)^2))


if (rmse1 < rmse2 && rmse1 < rmse3) {
  print("Model 1 has the lowest RMSE")
} else if (rmse2 < rmse1 && rmse2 < rmse3) {
  print("Model 2 has the lowest RMSE")
} else {
  print("Model 3 has the lowest RMSE")
}
