##### Problems Thesis Adrian Kula #####
##### Flow ID: 6882035 #####

## Data Understanding ##
#Installing and Loading Packages
#install.packages("ggplot2")
#install.packages("readxl")
#update.packages("dplyr")

#Load Libaries
library("ggplot2")
library(DataExplorer)
library(plyr)
library(readxl)
library(dplyr)


## Initial data cleaning ##

#Load data
dataCars <- read_excel("C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/dataCars_raw.xlsx")

#Inspecting variables type
class(dataCars)
str(dataCars)


#Remove irrelevant columns
library(janitor)
head(dataCars)
dataCars<-clean_names(dataCars)
colnames(dataCars)

# move price to first column
dataCars = dataCars %>% dplyr::select("price", 
                                      everything())

# Visualize the missing values. Consider deleting them from the dataset.##
table(is.na(dataCars))
plot_intro(dataCars)
plot_missing(dataCars)


#Remove redundant columns
dataCars$vehicle_id <- NULL
dataCars$registration <- NULL
dataCars$transmission <- NULL
dataCars$transmission2 <- NULL
dataCars$type <- NULL
dataCars$ncap_adult_occupant_protection_percent <- NULL
dataCars$ncap_child_occupant_protection_percent <- NULL
dataCars$ncap_pedestrian_protection_percent <- NULL
dataCars$no_of_seats <- NULL


#Converting all char columns to factor
dataCars <- as.data.frame(unclass(dataCars), stringsAsFactors = TRUE)
dataCars$registration_year <- factor(dataCars$registration_year)
str(dataCars)
head(dataCars$registration_year)


#Subset with missing values "NA"
dataCars_VisMissing <- dataCars[rowSums(is.na(dataCars)) > 0,]

#Subset without missing columns more than 70%
dataCars.cleanedColums <- dataCars
dataCars.cleanedColums$engine_layout <- NULL
dataCars.cleanedColums$max_towing_weight_unbraked_kg <- NULL
dataCars.cleanedColums$nedc_electricity_consumption_k_wh_100_km <- NULL
dataCars.cleanedColums$max_towing_weight_braked_kg <- NULL

plot_intro(dataCars.cleanedColums)
plot_missing(dataCars.cleanedColums)

## Remove rows with more than 50% NA
dataCars.cleanedRows <- dataCars.cleanedColums[which(rowMeans(!is.na(dataCars.cleanedColums)) > 0.50), ]
plot_intro(dataCars.cleanedRows)
plot_missing(dataCars.cleanedRows)



#Replace continous
dataCars.cleanedMedian <- dataCars.cleanedRows %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

plot_intro(dataCars.cleanedMedian)
plot_missing(dataCars.cleanedMedian)
str(dataCars.cleanedMedian)

#Replace factors
df_filled <- dataCars.cleanedMedian

get_mode <- function(v) {
  unique_v <- unique(v)
  unique_v[which.max(tabulate(match(v, unique_v)))]
  
}
  
  # Iterate over each column and replace missing values if it's a factor
  for (col_name in names(df_filled)) {
    if (is.factor(df_filled[[col_name]])) {
      mode_value <- get_mode(df_filled[[col_name]][!is.na(df_filled[[col_name]])])
      df_filled[[col_name]][is.na(df_filled[[col_name]])] <- mode_value
    }
  }

  
  plot_intro(df_filled)
  plot_missing(df_filled)
  
  
  # Subset numeric columns with dplyr
  data_num <- select_if(df_filled, is.numeric)            
  # Subset only factor columns
  df_factors <- df_filled[, sapply(df_filled, is.factor)]
  
  
  # Modified function to handle only numeric variables and return the count of outliers
  find_outliers_count <- function(x) {
    if (is.numeric(x)) {
      Q1 <- quantile(x, 0.25)
      Q3 <- quantile(x, 0.75)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Return the count of outliers
      return(sum(x < lower_bound | x > upper_bound))
    } else {
      return(NULL)  # Return NULL for non-numeric variables
    }
  }
  
  outliers_count_list <- lapply(df_filled, find_outliers_count)
  
  # Remove NULL entries from the list (for non-numeric columns)
  outliers_count_list <- outliers_count_list[!sapply(outliers_count_list, is.null)]
  
  # Print the count of outliers per variable
  outliers_count_list

  

  # 2. Using BoxPlot to detect the presence of outliers in the numeric/continuous data columns.

  # Create a list of indices for chunks
  chunks <- split(seq_along(names(data_num)), ceiling(seq_along(names(data_num))/8))
  
  # Reduce margins and divide the graphical window
  par(mfrow=c(3,3), mar=c(3, 3, 2, 1))  # Adjust the numbers to fit your needs
  
  # Plot the boxplots for the first 8 variables
  for (i in 1:30) {
    boxplot(data_num[,i], main = names(data_num)[i], col = "lightblue", cex.axis=0.8)
  }
  
  
  # Set the graphical window to display multiple plots
  par(mfrow=c(3, 3), mar=c(4, 4, 2, 1))  # 3 rows and 3 columns, with smaller margins
  
  # Loop through each numeric variable in data_num and plot a histogram with density plot
  for (var in names(data_num)) {
    hist(data_num[[var]], probability = TRUE, main = paste("Histogram with Density for", var), col = "lightgray")
    lines(density(data_num[[var]], na.rm = TRUE), col = "blue", lwd = 2)
  }
  
  # Reset the graphical window to default settings
  par(mfrow=c(2, 2), mar=c(5, 3, 3, 2))
  
  
  #Standarize vs normalize, No outliers in our dataset
  model <- lm(price ~ ., data = data_num)  # Assuming 'price' is the dependent variable and you use all other columns as predictors
  residuals <- resid(model)
  
  #Sales seems to be a bit left skewed, but close to be normally distributed
  hist(residuals, breaks=50, col="lightblue", main="Histogram of Residuals")
  lines(density(residuals), col="red", lwd=2)
  
  #Q-Q Plot suggests that points are far away form the red line
  qqnorm(residuals)
  qqline(residuals, col="red", lwd=2)
  
  
  sample_data <- sample(data_num$price, 5000, replace = FALSE)
  shapiro_test_result <- shapiro.test(sample_data)
  print(shapiro_test_result)

  #We cannot just delete outliers if they show the truth, bet when is is caused by bad data, the we should consider to
  #delete the outliers (Hodge & Austin, 2004)
  
  
  #install.packages("openxlsx")
  #install.packages("readxl")
  library(readxl)
  library(openxlsx)
  ###Saving cleaned file###
  write.xlsx(df_filled, "C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/df_cleaned.xlsx", rowNames = FALSE)
  
  

  #Load data
  df_filled <- read_excel("C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/df_cleaned.xlsx")
  str(df_filled)
  
  #Converting all char columns to factor
  df_filled <- as.data.frame(unclass(df_filled), stringsAsFactors = TRUE)
  df_filled$battery_charging_scenario_3_power_supply_k_w <- as.numeric(df_filled$battery_charging_scenario_3_power_supply_k_w)
  str(df_filled)
  
  # 1. Store the 'price' variable in a separate object
  price_col <- df_filled$price
  
  # 2. Drop price column
  df_independent = subset(df_filled, select = -c(price) )
  
  
  library(dplyr)
  # Subset numeric columns with dplyr
  df_num <- select_if(df_independent, is.numeric)          
  # Subset only factor columns
  df_factors <- df_independent[, sapply(df_independent, is.factor)]

  
  # 4. Correlation plots for understanding relationships
  # Compute the correlation matrix
  M <- cor(df_num, use = "complete.obs")
  
  # Get the upper triangle of the matrix
  upper_tri <- M[upper.tri(M)]
  
  # Create a data frame of variable pairs and their correlations
  high_corr_pairs <- data.frame(
    var1 = rownames(M)[row(M)[upper.tri(M)]],
    var2 = rownames(M)[col(M)[upper.tri(M)]],
    correlation = as.vector(upper_tri)
  )
  
  # Filter only those pairs with correlation above 0.7 in magnitude
  high_corr_pairs <- subset(high_corr_pairs, abs(correlation) > 0.90)
  
  # Sort the pairs by absolute correlation value
  high_corr_pairs <- high_corr_pairs[order(-abs(high_corr_pairs$correlation)), ]
  
  # Print the resulting pairs
  print(high_corr_pairs)
  
  
  # Compute mean absolute correlation for each variable
  mean_corr <- apply(abs(M), 1, mean)
  
  # For each pair in high_corr_pairs, determine which variable 
  # has a higher mean correlation and mark it for removal
  to_remove <- sapply(1:nrow(high_corr_pairs), function(i) {
    var1_mean <- mean_corr[high_corr_pairs$var1[i]]
    var2_mean <- mean_corr[high_corr_pairs$var2[i]]
    
    if (var1_mean > var2_mean) {
      return(high_corr_pairs$var1[i])
    } else {
      return(high_corr_pairs$var2[i])
    }
  })
  
  # Get unique variables to remove
  to_remove <- unique(to_remove)
  
  # Print out the variables to be removed
  print(to_remove)
  
  #To be removed: [1] "engine_torque_lbs_ft", "gross_vehicle_weight_kg", "wheelbase_mm", "engine_power_bhp" 
  
  # To actually drop them from the data_num dataset:
  df_num <- df_num[ , !(names(df_num) %in% to_remove)]
  
  
  # Add the 'price' variable back
  df_num$price <- price_col
  
  # move price to first column
  df_num = df_num %>% dplyr::select("price", 
                                        everything())
  
  
  ##  cardinalities ##
  
  library(ggplot2)
  library(corrplot)
  
  dim(df_num)
  dim(df_factors)
  
  # install.packages("ggplot2")
  library(ggplot2)
  
  # 1. Calculate cardinalities
  cardinalities <- sapply(df_factors, function(col) length(unique(col)))
  
  # Convert to a data frame for ggplot
  cardinality_df <- data.frame(Feature = names(cardinalities), Cardinality = cardinalities)
  
  # Order the data frame by Cardinality and get the top 5
  top_5_cardinality_df <- head(cardinality_df[order(-cardinality_df$Cardinality),], 5)
  
  # Plot using ggplot2
  ggplot(top_5_cardinality_df, aes(x = reorder(Feature, Cardinality), y = Cardinality)) +
    geom_col() + # This creates the column chart
    geom_text(aes(label=Cardinality), vjust=-0.5) + # This adds the cardinality values
    ggtitle("Top 5 Features by Cardinality") +
    xlab("Features") +
    ylab("Number of Unique Values")
  
  str(df_factors)
  #Converting all char columns to factor
  df_factors <- as.data.frame(unclass(df_factors), stringsAsFactors = TRUE)
  str(df_factors)
  
  df_factors$variant <- NULL
  
  #Refactoring
  
  library(dplyr)
  
  class(df_factors$insurance_group_1_50_effective_january_07)
  
  df_factors$insurance_group_1_50_effective_january_07 <- as.character(df_factors$insurance_group_1_50_effective_january_07)
  
  df_factors <- df_factors %>% 
    mutate(insurance_group = case_when(
      as.numeric(substr(insurance_group_1_50_effective_january_07, 1, nchar(insurance_group_1_50_effective_january_07)-1)) <= 10 ~ "1-10",
      as.numeric(substr(insurance_group_1_50_effective_january_07, 1, nchar(insurance_group_1_50_effective_january_07)-1)) <= 20 ~ "11-20",
      as.numeric(substr(insurance_group_1_50_effective_january_07, 1, nchar(insurance_group_1_50_effective_january_07)-1)) <= 30 ~ "21-30",
      as.numeric(substr(insurance_group_1_50_effective_january_07, 1, nchar(insurance_group_1_50_effective_january_07)-1)) <= 40 ~ "31-40",
      as.numeric(substr(insurance_group_1_50_effective_january_07, 1, nchar(insurance_group_1_50_effective_january_07)-1)) <= 50 ~ "41-50",
      TRUE ~ "Other"
    ))
  
  df_factors$insurance_group_1_50_effective_january_07 <- NULL
  
  str(df_factors)
  
  head(df_factors)
  
  
  
  ### Data Exploration part 2 ###
  
  # 2. Histograms for continuous variables
  for (var in names(df_num)) {
    print(
      ggplot(df_num, aes_string(var)) + 
        geom_histogram(fill="lightblue", color="black") + 
        labs(title = paste("Histogram of", var))
    )
  }
    
  # Reset the graphical window to default settings
  par(mfrow=c(3, 3))
  # 3. Bar plots for categorical variables
  for (cat_var in names(df_factors)) {
    print(
      ggplot(df_factors, aes_string(cat_var)) + 
        geom_bar(fill="lightblue", color="black") + 
        labs(title = paste("Bar Plot of", cat_var))
    )
  }
  
  
  table(df_factors$coupler_connector_type)
  table(df_factors$gears)
  table(df_factors$make)
  
  # 4. Scatter plots between continuous variables
  library(ggplot2)
  
  # Reset the graphical window to default settings
  par(mfrow=c(1, 1))
  
  # Set up the plotting window to display 3x3 grid of plots
  par(mfrow = c(2, 3))
  
  # List of key independent variables based on the literature
  selected_vars <- c("mileage", "battery_capacity_in_k_wh", "top_speed_mph", "nedc_maximum_ev_range_miles", "x0_to_62_mph_secs")
  
  for (var in selected_vars) {
    plot(df_num[[var]], df_num$price, main = paste("Price vs", var), xlab = var, ylab = "Price")
    abline(lm(price ~ df_num[[var]], data = df_num), col = "red")  # Adds a regression line
  }
  
  head(df_num)
  
  
  #Final dateset
  df_final <- cbind(df_factors, df_num)
  # move price to first column
  df_final = df_final %>% dplyr::select("price", 
                                    everything())
  str(df_final)
  
  #Saving final data#
  library(readxl)
  library(openxlsx)
  
  write.xlsx(df_final, "C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/df_final.xlsx", rowNames = FALSE)
  

  
  
##### Modeling Phase #####
  
  #Loading final data#
  library(readxl)
  library(openxlsx)
  df_final <- read_excel("C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/df_final.xlsx")
  str(df_final)
  
  #Converting all char columns to factor
  df_final <- as.data.frame(unclass(df_final), stringsAsFactors = TRUE)
  str(df_final)
 
  
  # 1. Stratified Splitting seems to be legitimate since we have rare categories:
  library(caret)
  set.seed(222)  # Setting seed for reproducibility
  # Stratified sampling based on quantiles of 'price'
  trainIndexStratified <- createDataPartition(df_final$price, p=0.7, list=FALSE)

  trainDataStratified <- df_final[trainIndexStratified, ]
  testDataStratified <- df_final[-trainIndexStratified, ]
  
  
  ## Arguments for stratified split ##
  # Calculate the proportions
  car_model_proportions <- prop.table(table(df_final$make))
  
  # Sort the proportions in descending order
  sorted_car_model_proportions <- sort(car_model_proportions, decreasing = TRUE)
  
  # Convert to percentages
  sorted_car_model_percentages <- sorted_car_model_proportions * 100
  
  # Print the sorted percentages
  print(sorted_car_model_percentages)
  
  #Displaying results of categories distribution
  hist(df_final$price, main = "Histogram of Price", xlab = "make")
  table(df_final$make)
  sorted_car_model_table <- sort(table(testDataStratified$make), decreasing = TRUE)
  barplot(sorted_car_model_table, main = "Bar Chart of Car Models", las=2, ylim=c(0, max(sorted_car_model_table)))


  # # 2. Random Splitting:
  # set.seed(222)
  # trainIndexRandom <- sample(seq_len(nrow(df_final)), size = floor(0.7 * nrow(df_final)))
  # 
  # trainDataRandom <- df_final[trainIndexRandom, ]
  # testDataRandom <- df_final[-trainIndexRandom, ]
  # 
  # sum(is.na(trainDataRandom))  
  

  
  
  ## Decision Trees ##
  library(rpart)
  set.seed(222)
  # Building an initial decision tree with a very small cp
  fit.tree <- rpart(price ~ ., data=trainDataStratified, method="anova", cp=0.00001)
  
  # Display the tree
  print(fit.tree)
  
  #PRint tree
  plot(fit.tree)
  
  # Using cross-validation to find the best cp value
  fit.cptable <- fit.tree$cptable
  
  # The cptable contains the cross-validated error for different cp values
  # Let's find the cp with the smallest xerror (cross-validated error)
  best.cp <- fit.cptable[which.min(fit.cptable[,"xerror"]),"CP"]
  
  # Pruning the tree using the best cp value
  pruned.tree <- prune(fit.tree, cp=best.cp)
  
  # Displaying the pruned tree
  print(pruned.tree)
  
  #Print pruned tree
  plot(pruned.tree)
  

  # Predictions using the initial tree
  predictions.initial <- predict(fit.tree, newdata=testDataStratified)
  
  # Predictions using the pruned tree
  predictions.pruned <- predict(pruned.tree, newdata=testDataStratified)
  
  # Calculate RMSE for initial tree
  rmse.initial <- sqrt(mean((testDataStratified$price - predictions.initial)^2))
  
  # Calculate RMSE for pruned tree
  rmse.pruned <- sqrt(mean((testDataStratified$price - predictions.pruned)^2))
  
  # Print out the RMSE values
  cat("RMSE for the initial tree:", rmse.initial, "\n")
  cat("RMSE for the pruned tree:", rmse.pruned, "\n")
  #the cross-validation results suggests that the most complex tree is the best one since RMSE is the lowest.
  # RMSE for the initial tree: 4762.059 
  # >   cat("RMSE for the pruned tree:", rmse.pruned, "\n")
  # RMSE for the pruned tree: 4732.369 
  # 
  
  # Variable Importance for initial tree
  importance_dt <- fit.tree$variable.importance
  cat("\nVariable Importance for the initial tree:\n")
  print(importance_dt)
  
  # Extracting the top 10 important variables
  top_10_importance <- head(sort(importance_dt, decreasing = TRUE), 10)
  
  # Calculate total importance for percentage conversion
  total_importance <- sum(importance_dt)
  
  # Plotting the top 10 variable importance using barplot, but suppress y-axis for now
  bp <- barplot(top_10_importance / total_importance * 100, 
                las=2, # Make the variable names vertical for better readability
                main="Variable Importance for Initial Tree (Top 10) in %", 
                col="skyblue", 
                border="black",
                cex.names=0.8,
                ylab="Percentage (%)")
  
  
  
  #Mean Absolute Error (MAE)
  MAE <- mean(abs(testDataStratified$price - predictions.pruned))
  
  #Mean Absolute Percentage Error (MAPE)
  MAPE <- mean(abs((testDataStratified$price - predictions.pruned)/testDataStratified$price))*100
  
  #R-Squared (R2)
  SST <- sum((testDataStratified$price - mean(testDataStratified$price))^2)
  SSRes <- sum((testDataStratified$price - predictions.pruned)^2)
  R2 <- 1 - (SSRes/SST)
  
  # Print the metrics
  results_dt <- data.frame(Metric = c("Mean Absolute Error", "Mean Absolute Percentage Error", "R-Squared"),
                           Value = c(MAE, MAPE, R2))
  print(results_dt)
  
  #Results:
  # Metric        Value
  # 1            Mean Absolute Error 2779.5305221
  # 2 Mean Absolute Percentage Error    9.4048881
  # 3                      R-Squared    0.9200394
  
  #Overfiting
  
  library(caret)
  postResample(predictions.initial, testDataStratified$price)
  
  #Training
  trainPredictions <- predict(fit.tree, newdata = trainDataStratified)
  trainPerformance <- postResample(trainPredictions, trainDataStratified$price)
  
  #Testing
  testPredictions <- predict(fit.tree, newdata = testDataStratified)
  testPerformance <- postResample(testPredictions, testDataStratified$price)

  # Create a comparison dataframe
  comparison_df <- data.frame(
    Metric = names(trainPerformance),
    Training = round(as.numeric(trainPerformance), 4),
    Testing = round(as.numeric(testPerformance), 4)
  )
  
  # Print the dataframe
  print(comparison_df, row.names = FALSE)
  
  #Result
  # Metric  Training   Testing
  # RMSE 3438.7937 4730.8847
  # Rsquared    0.9602    0.9208
  # MAE 2095.1405 2752.5423
  
  #No overfitting in the data since test error is higher than on the training data
  
  
  
  
  

  
  ## Random Forest ##
  
  ### Import libraries
  #install.packages("ranger")
  library(ggplot2)
  library(caret)
  library(ranger)
  
  # Define cross-validation method
  control <- trainControl(method = "cv",  #cross-validations
                          number = 5,     # Number of folds
                          verboseIter = TRUE) 
  
  # Define the tuning grid
  tuneGrid <- data.frame(
    mtry = c(floor((ncol(trainDataStratified) - 1) / 3)), # mtry = p/3 is simply a RF model in regression.
    splitrule = "variance",
    min.node.size = 1
  )
  
  # Train the model with cross-validation
  rf.fit <- train(price ~ ., 
                  data = trainDataStratified, 
                  method = "ranger", 
                  num.trees = 1000, 
                  trControl = control,
                  tuneGrid = tuneGrid,   # provide the tuning grid
                  importance = 'impurity',
                  save.memory = TRUE)
  
  


  # Predict using the model on the test data
  predictions <- predict(rf.fit, newdata = testDataStratified)
  
  # Calculate Metrics:
  
  # Mean Absolute Error (MAE)
  MAE <- mean(abs(testDataStratified$price - predictions))
  
  # Mean Absolute Percentage Error (MAPE)
  MAPE <- mean(abs((testDataStratified$price - predictions)/testDataStratified$price))*100
  
  # R-Squared (R2)
  SST <- sum((testDataStratified$price - mean(testDataStratified$price))^2)
  SSRes <- sum((testDataStratified$price - predictions)^2)
  R2 <- 1 - (SSRes/SST)
  
  # Print the metrics
  results_rf <- data.frame(Metric = c("Mean Absolute Error", "Mean Absolute Percentage Error", "R-Squared"),
                        Value = c(MAE, MAPE, R2))
  print(results_rf)
  
  #Results
  # Metric        Value
  # 1            Mean Absolute Error 2724.3755298
  # 2 Mean Absolute Percentage Error    9.5388699
  # 3                      R-Squared    0.9341575
  
  
  library(ggplot2)
  
  # Extract feature importance and create dataframe
  importance_rf <- data.frame(
    variable = names(rf.fit$finalModel$variable.importance),
    importance = rf.fit$finalModel$variable.importance
  )
  
  # Sort, extract top 10, and calculate importance percentages
  importance_rf <- importance_rf[order(-importance_rf$importance), ][1:10, ]
  importance_rf$importance_percent <- (importance_rf$importance / sum(importance_rf$importance)) * 100
  
  # Plot the importance percentages using ggplot2
  ggplot(importance_rf, aes(x = reorder(variable, importance_percent), y = importance_percent)) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    ggtitle("Top 10 Important Features in RF") +
    xlab("Features") +
    ylab("Importance (%)") +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
  
  #Overfiting
  
  library(caret)
  postResample(predictions, testDataStratified$price)
  
  #Training
  trainPrediction_rf <- predict(rf.fit, newdata = trainDataStratified)
  trainPerformance_rf <- postResample(trainPrediction_rf, trainDataStratified$price)
  
  #Testing
  testPredictions_rf <- predict(rf.fit, newdata = testDataStratified)
  testPerformance_rf <- postResample(predictions, testDataStratified$price)
  
  # Create a comparison dataframe
  comparison_rf <- data.frame(
    Metric = names(trainPerformance_rf),
    Training = round(as.numeric(trainPerformance_rf), 4),
    Testing = round(as.numeric(testPerformance_rf), 4)
  )

  # Print the dataframe
  print(comparison_rf, row.names = FALSE)
  
  #No overfitting in the data since test error is higher than on the training data
  #Result to check overfitting
  # Metric Training   Testing
  # RMSE 3531.160 4294.3148
  # Rsquared    0.959    0.9345
  # MAE 2404.236 2724.3755
  
  
  
  
  
  ## Bagging ##
  
  # Assuming libraries and data splitting is done
  # Make sure you've loaded the 'ranger' library
  library(ggplot2)
  library(caret)
  library(ranger)
  
  # Define cross-validation method
  
  # Define cross-validation method
  control <- trainControl(method = "cv",  #cross-validations
                          number = 5,     # Number of folds
                          verboseIter = TRUE) 
  
  # Define the tuning grid for bagging
  # mtry is set to the total number of predictors
  tuneGrid <- data.frame(
    mtry = c(ncol(trainDataStratified) - 1), # mtry = p is simply a bagging model
    splitrule = "variance",
    min.node.size = 1
  )
  
  # Train the model with cross-validation
  bagged.fit <- train(price ~ ., 
                        data = trainDataStratified, 
                        method = "ranger", 
                        num.trees = 1000, 
                        trControl = control,
                        tuneGrid = tuneGrid,   # provide the tuning grid
                        importance = 'impurity',
                        save.memory = TRUE)
  
  
  
  # Predict using the model on the test data
  predictions_bagging <- predict(bagged.fit, newdata = testDataStratified)
  
  # 3. Performance Metrics:
  # MAE
  MAE_bagging <- mean(abs(testDataStratified$price - predictions_bagging))
  
  # MAPE
  MAPE_bagging <- mean(abs((testDataStratified$price - predictions_bagging) / testDataStratified$price)) * 100
  
  # R-Squared
  SST_bagging <- sum((testDataStratified$price - mean(testDataStratified$price))^2)
  SSRes_bagging <- sum((testDataStratified$price - predictions_bagging)^2)
  R2_bagging <- 1 - (SSRes_bagging/SST_bagging)
  
  
  # Print the metrics
  results_bag <- data.frame(Metric = c("Mean Absolute Error", "Mean Absolute Percentage Error", "R-Squared"),
                        Value = c(MAE_bagging, MAPE_bagging, R2_bagging))
  print(results_bag)
  
  #Result
  # Metric        Value
  # 1            Mean Absolute Error 2352.6599612
  # 2 Mean Absolute Percentage Error    8.1830744
  # 3                      R-Squared    0.9434032

  
  library(ggplot2)
  
  # Extract feature importance and create dataframe
  importance_bag <- data.frame(
    variable = names(bagged.fit$finalModel$variable.importance),
    importance = bagged.fit$finalModel$variable.importance
  )
  
  # Sort, extract top 10, and calculate importance percentages
  importance_bag <- importance_bag[order(-importance_bag$importance), ][1:10, ]
  importance_bag$importance_percent <- (importance_bag$importance / sum(importance_bag$importance)) * 100
  
  # Plot the importance percentages using ggplot2
  ggplot(importance_bag, aes(x = reorder(variable, importance_percent), y = importance_percent)) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    ggtitle("Top 10 Important Features in Bagging") +
    xlab("Features") +
    ylab("Importance (%)") +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
  
  print(importance_bag)

  #Result
  # variable   importance importance_percent
  # battery_capacity_in_k_wh         battery_capacity_in_k_wh 202136773414          21.224857
  # minimum_kerbweight_kg               minimum_kerbweight_kg 159078432613          16.703626
  # length_mm                                       length_mm 144915412944          15.216474
  # x0_to_62_mph_secs                       x0_to_62_mph_secs  89134684769           9.359361
  # width_including_mirrors_mm     width_including_mirrors_mm  79086337052           8.304259
  # engine_torque_nm                         engine_torque_nm  67257395901           7.062192
  # top_speed_mph                               top_speed_mph  67062847043           7.041763
  # gears2 SPEED                                 gears2 SPEED  55829390213           5.862223
  # width_mm                                         width_mm  52828814930           5.547155
  # drive_trainFront Wheel Drive drive_trainFront Wheel Drive  35028608886           3.678090
  
  
  #Overfiting
  
  library(caret)
  postResample(predictions_bagging, testDataStratified$price)
  
  #Training
  trainPrediction_bag <- predict(bagged.fit, newdata = trainDataStratified)
  trainPerformance_bag <- postResample(trainPrediction_bag, trainDataStratified$price)
  
  #Testing
  testPredictions_bag <- predict(bagged.fit, newdata = testDataStratified)
  testPerformance_bag <- postResample(testPredictions_bag, testDataStratified$price)
  
  # Create a comparison dataframe
  comparison_bag <- data.frame(
    Metric = names(trainPerformance_bag),
    Training = round(as.numeric(trainPerformance_bag), 4),
    Testing = round(as.numeric(testPerformance_bag), 4)
  )
  
  # Print the dataframe
  print(comparison_bag, row.names = FALSE)
  
  
  #No overfitting in the data since test error is higher than on the training data
  #Result to check overfitting
  # Metric  Training   Testing
  # RMSE 2473.3988 3995.3727
  # Rsquared    0.9796    0.9431
  # MAE 1645.3137 2373.6554
  # 


  
  
 
  
  
   ## xgboost ##

  # Ensure the required libraries are loaded
  library(caret)
  library(xgboost)
  
  #Define the control parameters for training
  set.seed(222)
  train.param <- trainControl(method = "cv", number = 5)

  # Define the tuning grid
  tune.grid.xgboost <- expand.grid(
    max_depth = 3:10,
    gamma = c(0, 1, 2),
    eta = c(0.06, 0.1),
    nrounds = 500,
    subsample = 0.5,
    colsample_bytree = 0.1,
    min_child_weight = 1
  )

  # # Train the model
  # model.xgboost <- caret::train(
  #   price ~ ., data = trainDataStratified,
  #   method = "xgbTree",
  #   tuneGrid = tune.grid.xgboost,
  #   trControl = train.param
  # )


  # Save the trained model to disk
  #saveRDS(model.xgboost, file = "C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/model_xgboost_main.rds")

  
  # Load the saved model from disk
  model.xgboost <- readRDS(file = "C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/model_xgboost_main.rds")
  
  
  # Display the model
  print(model.xgboost)
  #Result
  # Tuning parameter 'nrounds' was held constant at a value of 500
  # Tuning parameter 'colsample_bytree' was held constant at a value of
  # 0.1
  # Tuning parameter 'min_child_weight' was held constant at a value of 1
  # Tuning parameter 'subsample' was held constant at a
  # value of 0.5
  # RMSE was used to select the optimal model using the smallest value.
  # The final values used for the model were nrounds = 500, max_depth = 5, eta = 0.06, gamma = 1, colsample_bytree = 0.1,
  # min_child_weight = 1 and subsample = 0.5.
  
  
  
  # Plot the tuning results
  plot(model.xgboost)

  # Print the variable importance
  importance_xgb <- varImp(model.xgboost, scale = FALSE)
  print(importance_xgb)
  
  # Extract variable importance
  importance_xgb <- xgb.importance(model = model.xgboost$finalModel)
  
  # Convert importance data to dataframe
  importance_xgb <- as.data.frame(importance_xgb)
  
  # Order by importance in descending order and take the importance_xgb 10 most important
  top_10_importance <- head(importance_xgb[order(-importance_xgb$Gain), ], 10)
  
  # Plot using ggplot2
  library(ggplot2)
  ggplot(top_10_importance, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Top 10 Most Important Variables", x = "Variables", y = "Importance") +
    theme_minimal()

  
  # Make predictions on the test set
  predictions <- predict(model.xgboost, newdata = testDataStratified)
  
  # Calculate evaluation metrics
  
  # Mean Absolute Error (MAE)
  MAE <- mean(abs(testDataStratified$price - predictions))
  
  # Mean Absolute Percentage Error (MAPE)
  MAPE <- mean(abs((testDataStratified$price - predictions) / testDataStratified$price)) * 100
  
  # R-Squared (R2)
  SST <- sum((testDataStratified$price - mean(testDataStratified$price))^2)
  SSRes <- sum((testDataStratified$price - predictions)^2)
  R2 <- 1 - (SSRes / SST)
  
  # Print the metrics
  results_xgb <- data.frame(Metric = c("Mean Absolute Error", "Mean Absolute Percentage Error", "R-Squared"),
                        Value = c(MAE, MAPE, R2))
  print(results_xgb)
  
  #Results
  # Metric        Value
  # 1            Mean Absolute Error 2284.4467154
  # 2 Mean Absolute Percentage Error    7.9686403
  # 3                      R-Squared    0.9461135
  
  library(caret)
  postResample(predictions, testDataStratified$price)
  
  #Training
  trainPrediction_xgb <- predict(model.xgboost, newdata = trainDataStratified)
  trainPerformance_xgb <- postResample(trainPrediction_xgb, trainDataStratified$price)
  
  #Testing
  testPredictions_xgb <- predict(model.xgboost, newdata = testDataStratified)
  testPerformance_xgb <- postResample(testPredictions_xgb, testDataStratified$price)
  
  # Create a comparison dataframe
  comparison_bag <- data.frame(
    Metric = names(trainPerformance_xgb),
    Training = round(as.numeric(trainPerformance_xgb), 4),
    Testing = round(as.numeric(testPerformance_xgb), 4)
  )
  
  # Print the dataframe
  print(comparison_bag, row.names = FALSE)
  
  #No overfitting in the data since test error is higher than on the training data
  #Result to check overfitting
  # Metric Training   Testing
  # RMSE 2314.495 3884.9053
  # Rsquared    0.982    0.9462
  # MAE 1632.377 2284.4467
  
  # load libraries
  library(xgboost)
  library(caret)
  library(dplyr)
  library(DiagrammeR)
  
  # plot the first tree
  xgb.plot.tree(model = model.xgboost$finalModel, trees = 1)
  

  
  
  
  
  
  
  
  #### Evaluation ###
  
  ## Model Evaluation ##
  # Adding a method column
  results_dt$Method <- "Decision Trees"
  results_rf$Method <- "Random Forest"
  results_bag$Method <- "Bagging"
  results_xgb$Method <- "XGBoost"
  
  
  # Combine results
  combined_results <- rbind(results_dt, results_rf, results_bag, results_xgb)
  print(combined_results)
  
  
  
  #install.packages("knitr")
  #install.packages("tidyverse")
  library(knitr)
  library(tidyverse)
  
  # Transposingg columns
  transposed_results <- combined_results %>%
    spread(key = Metric, value = Value) %>%
    arrange(desc(`R-Squared`))
  
  kable(transposed_results, caption = "Model Performance Comparison", align = 'c')
  
  
  ## Variable Evaluation ##
  
  # Importance
  importance_dt 
  importance_rf 
  importance_bag 
  importance_xgb
  
  is.data.frame(importance_dt)
  class(importance_dt)
  
  importance_dt <- data.frame(variable = names(importance_dt), importance = importance_dt)
  
  
  # Extract top 10 rows and first two columns from importance_dt
  top_10_dt <- importance_dt[order(importance_dt$importance, decreasing = TRUE), ][1:10, 1:2]
  
  # Extract top 10 rows and first two columns from importance_rf
  top_10_rf <- importance_rf[order(importance_rf$importance, decreasing = TRUE), ][1:10, 1:2]
  
  # Extract top 10 rows and first two columns from importance_bag
  top_10_bag <- importance_bag[order(importance_bag$importance, decreasing = TRUE), ][1:10, 1:2]
  
  # Extract top 10 rows and first two columns from importance_xgb
  top_10_xgb <- importance_xgb[order(importance_xgb$Gain, decreasing = TRUE), ][1:10, 1:2]
  
  
  
  # Load the knitr package
library(knitr)

# Create a list of data frames
  combined_results <- cbind(top_10_dt,top_10_rf, top_10_bag, top_10_xgb)
  
  kable(combined_results)
  
  

  
  
  
  
  
  
  
#### Deployment ###

#Loading final data#
library(readxl)
library(openxlsx)
library(caret)
library(xgboost)

df_final <- read_excel("C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/df_final.xlsx")
str(df_final)

#Converting all char columns to factor
df_final <- as.data.frame(unclass(df_final), stringsAsFactors = TRUE)
str(df_final)

# 2. Splitting:
library(caret)
set.seed(222)  # Setting seed for reproducibility
# Stratified sampling based on quantiles of 'price'
trainIndexStratified <- createDataPartition(df_final$price, p=0.7, list=FALSE)

trainDataStratified <- df_final[trainIndexStratified, ]
testDataStratified <- df_final[-trainIndexStratified, ]


#str(trainDataStratified)
#str(testDataStratified)

# # Extract the xgb.Booster model
# booster_model <- model.xgboost$finalModel


#Define the control parameters for training
train.param <- trainControl(method = "cv", number = 5)


# Define the tuning grid
tune.grid.xgboost <- expand.grid(
  max_depth = 3:10,
  gamma = c(0, 1, 2),
  eta = c(0.06, 0.1),
  nrounds = 500,
  subsample = 0.5,
  colsample_bytree = 0.1,
  min_child_weight = 1
)


# #Train the model on 10 top  filters used on motors.co.uk
# set.seed(222)
# model.xgboost.app <- caret::train(
#   price ~ make + model + registration_year + mileage + body_style + colour + x0_to_62_mph_secs + top_speed_mph + nedc_maximum_ev_range_miles + battery_capacity_in_k_wh,
#   data = trainDataStratified,
#   method = "xgbTree",
#   tuneGrid = tune.grid.xgboost,
#   trControl = train.param
# )


# Save the trained model to disk
#saveRDS(model.xgboost.app, file = "C:/Users/adrian182/OneDrive/Documents/Master's thesis/Data/App/EV_Price_Prediction/model.xgboost.app.rds")
# Save the model
#saveRDS(model.xgboost.app, file = "model.xgboost.app.rds")


# Load the saved model from disk
model.xgboost.app <- readRDS(file = "model.xgboost.app.rds")

# Display the model
print(model.xgboost.app)

# Plot the tuning results
plot(model.xgboost.app)

# Extract variable importance using xgboost
importance_xgb <- xgb.importance(model = model.xgboost.app$finalModel)

# Convert importance data to dataframe
importance_xgb <- as.data.frame(importance_xgb)

# Order by importance in descending order and take the importance_xgb 10 most important
top_10_importance <- head(importance_xgb[order(-importance_xgb$Gain), ], 10)

# Plot using ggplot2
library(ggplot2)
ggplot(top_10_importance, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Important Variables", x = "Variables", y = "Importance") +
  theme_minimal()

# Make predictions on the test set
predictions <- predict(model.xgboost.app, newdata = testDataStratified)

# Calculate evaluation metrics

# Mean Absolute Error (MAE)
MAE_app <- mean(abs(testDataStratified$price - predictions))

# Mean Absolute Percentage Error (MAPE)
MAPE_app <- mean(abs((testDataStratified$price - predictions) / testDataStratified$price)) * 100

# R-Squared (R2)
SST_app <- sum((testDataStratified$price - mean(testDataStratified$price))^2)
SSRes_app <- sum((testDataStratified$price - predictions)^2)
R2_app <- 1 - (SSRes_app / SST_app)

# Print the metrics
results_xgb_app <- data.frame(Metric = c("Mean Absolute Error", "Mean Absolute Percentage Error", "R-Squared"),
                          Value = c(MAE_app, MAPE_app, R2_app))
print(results_xgb_app)

##Results
# Metric        Value
# 1            Mean Absolute Error 2541.3525844
# 2 Mean Absolute Percentage Error    8.7294848
# 3                      R-Squared    0.9380136



## Model Evaluation main vs app XGBoost ##
# Adding a method column
results_xgb$Method <- "XGBoost Main" 
results_xgb_app$Method <- "XGBoost App"

# Combine results
combined_results_XGB <- rbind(results_xgb, results_xgb_app)
print(combined_results_XGB)



#install.packages("knitr")
#install.packages("tidyverse")
library(knitr)
library(tidyverse)

# Transposingg columns
transposed_results_XGB <- combined_results_XGB %>%
  spread(key = Metric, value = Value) %>%
  arrange(desc(`R-Squared`))

kable(transposed_results_XGB, caption = "Model Performance Comparison", align = 'c')



### Web Tool for EV Price Prediction ###
# Please check the app.R file

# 
# ## Publishing the app ##
#  #Check the app.R file