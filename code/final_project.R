# STAT 131 Final Project
# Time Series Modeling of Agricultural Market Data
# Jordan Turley

path = '~/Google Drive/School/Harvard University/Semester 1/STAT 131 - Time Series/Final Project/'
corn = read.csv(paste(path, 'corn-prices-historical-chart-data.csv', sep = ''), header = TRUE)
cotton = read.csv(paste(path, 'cotton-prices-historical-chart-data.csv', sep = ''), header = TRUE)
soybeans = read.csv(paste(path, 'soybean-prices-historical-chart-data.csv', sep = ''), header = TRUE)
wheat = read.csv(paste(path, 'wheat-prices-historical-chart-data.csv', sep = ''), header = TRUE)

backIdx = 2519

# Plot all of corn data
plot(corn, xlab = 'Date', ylab = 'Price per Bushel (dollars)', main = 'Corn Price per Bushel since 1959')
lines(corn)

# Plot corn data for last ten years
plot(corn[(nrow(corn) - backIdx):nrow(corn), ]$value, pch = 20, cex = 0.2, ylab = 'Price per Bushel (dollars)', main = 'Corn Price per Bushel since 2009')
lines(corn[(nrow(corn) - backIdx):nrow(corn), ]$value)

# Plot all of cotton data
plot(cotton, xlab = 'Date', ylab = 'Price per Pound (dollars)', main = 'Cotton Price per Pound since 1959')
lines(cotton)

# Plot cotton data for last ten years
plot(cotton[(nrow(cotton) - backIdx):nrow(cotton), ]$value, pch = 20, cex = 0.2, ylab = 'Price per Pound (dollars)', main = 'Cotton Price per Pound since 2009')
lines(cotton[(nrow(cotton) - backIdx):nrow(cotton), ]$value)

# Plot all soybean data
plot(soybeans, xlab = 'Date', ylab = 'Price per Bushel (dollars)', main = 'Soybean Price per Bushel since 1959')
lines(soybeans)

# Plot soybean data for last ten years
plot(soybeans[(nrow(cotton) - backIdx):nrow(soybeans), ]$value, pch = 20, cex = 0.2, ylab = 'Price per Bushel (dollars)', main = 'Soybean Price per Bushel since 2009')
lines(soybeans[(nrow(cotton) - backIdx):nrow(soybeans), ]$value)

# Plot all wheat data
plot(wheat, xlab = 'Date', ylab = 'Price per Bushel (dollars)', main = 'Wheat Price per Bushel since 1959')
lines(wheat)

# Plot wheat data for last ten years
plot(wheat[(nrow(wheat) - backIdx):nrow(wheat), ]$value, pch = 20, cex = 0.2, ylab = 'Price per Bushel (dollars)', main = 'Wheat Price per Bushel since 2009')
lines(wheat[(nrow(wheat) - backIdx):nrow(wheat), ]$value)

########## Recession ##########

plot(corn[12200:12575, ]$value, pch = 20, cex = 0.2, ylab = 'Price per Bushel (dollars)', main = 'Corn Price per Bushel during Great Recession')
lines(corn[12200:12575, ]$value)

plot(cotton[8839:9213, ]$value, pch = 20, cex = 0.2, ylab = 'Price per Pound (dollars)', main = 'Cotton Price per Pound during Great Recession')
lines(cotton[8839:9213, ]$value)

plot(soybeans[9819:10192, ]$value, pch = 20, cex = 0.2, ylab = 'Price per Bushel (dollars)', main = 'Soybean Price per Bushel during Great Recession')
lines(soybeans[9819:10192, ]$value)

plot(wheat[12200:12575, ]$value, pch = 20, cex = 0.2, ylab = 'Price per Bushel (dollars)', main = 'Wheat Price per Bushel during Great Recession')
lines(wheat[12200:12575, ]$value)

# Split data into training and test sets
train_split = function(data, data_size, test_size) {
  train = data[(nrow(data) - data_size):(nrow(data) - test_size), ]
  return(train)
}

test_split = function(data, data_size, test_size) {
  test = data[(nrow(data) - test_size + 1):nrow(data), ]
  return(test)
}

oneMonth = 22

cornTrain = train_split(corn, backIdx, oneMonth)
cornTest = test_split(corn, backIdx, oneMonth)

cottonTrain = train_split(cotton, backIdx, oneMonth)
cottonTest = test_split(cotton, backIdx, oneMonth)

soybeansTrain = train_split(soybeans, backIdx, oneMonth)
soybeansTest = test_split(soybeans, backIdx, oneMonth)

wheatTrain = train_split(wheat, backIdx, oneMonth)
wheatTest = test_split(wheat, backIdx, oneMonth)

# Calculate first differences and 
first_diffs = function(data) {
  diffs = integer(length(data$value) - 1)
  for (i1 in 1:length(data$value) - 1) {
    diffs[i1] = data$value[i1 + 1] - data$value[i1]
  }
  return(diffs)
}

cornDiffs = first_diffs(cornTrain)
plot(cornDiffs, ylab = 'Daily Change (First Difference)', main = 'Daily Change (First Difference) in Corn Prices')
lines(cornDiffs)

cottonDiffs = first_diffs(cottonTrain)
plot(cottonDiffs, ylab = 'Daily Change (First Difference)', main = 'Daily Change (First Difference) in Cotton Prices')
lines(cottonDiffs)

soybeansDiffs = first_diffs(soybeansTrain)
plot(soybeansDiffs, ylab = 'Daily Change (First Difference)', main = 'Daily Change (First Difference) in Soybean Prices')
lines(soybeansDiffs)

wheatDiffs = first_diffs(wheatTrain)
plot(wheatDiffs, ylab = 'Daily Change (First Difference)', main = 'Daily Change (First Difference) in Wheat Prices')
lines(wheatDiffs)

########## Model Selection ##########
aic_bic = function(data, maxAR, maxMA) {
  aicVals = matrix(list(), nrow = maxAR, ncol = maxMA)
  bicVals = matrix(list(), nrow = maxAR, ncol = maxMA)
  minAIC = 1e99
  minAICAR = 0
  minAICMA = 0
  minBIC = 1e99
  minBICAR = 0
  minBICMA = 0
  for (i1 in 1:maxAR) {
    for (i2 in 1:maxMA) {
      arma = arima(data$value, order = c(i1, 1, i2), optim.control = list(maxit = 300))
      aic = AIC(arma)
      bic = BIC(arma)
      aicVals[i1, i2] = aic
      bicVals[i1, i2] = bic
      
      if (aic < minAIC) {
        minAIC = aic
        minAICAR = i1
        minAICMA = i2
      }
      if (bic < minBIC) {
        minBIC = bic
        minBICAR = i1
        minBICMA = i2
      }
    }
  }
  
  print("AIC Values:")
  print(aicVals)
  print("")
  print("BIC Values:")
  print(bicVals)
  
  return(c(minAIC, minAICAR, minAICMA, minBIC, minBICAR, minBICMA))
}

## Corn
# ACF/PACF
acf(cornDiffs, main = 'ACF of Corn First Differences')
pacf(cornDiffs, main = 'Partial ACF of Corn First Differences')

corn_aic_bic_vals = aic_bic(cornTrain, 6, 6)
corn_aic_bic_vals

cornModel1 = arima(cornTrain$value, order = c(1, 1, 1))
cornModel2 = arima(cornTrain$value, order = c(3, 1, 6))

cornPredictions1 = predict(cornModel1, n.ahead = 22)
cornPredictions2 = predict(cornModel2, n.ahead = 22)

# Plot the predictions
plot(cornTest$value, ylim = c(2.8, 4.6), ylab = 'Corn Price', main = 'Corn Test Set Predictions')
lines(cornTest$value)
points(1:22, cornPredictions1$pred, col = 'red')
points(1:22, cornPredictions1$pred - 2 * cornPredictions1$se, col = 'red')
points(1:22, cornPredictions1$pred + 2 * cornPredictions1$se, col = 'red')
lines(1:22, cornPredictions1$pred, col = 'red')
lines(1:22, cornPredictions1$pred - 2 * cornPredictions1$se, col = 'red')
lines(1:22, cornPredictions1$pred + 2 * cornPredictions1$se, col = 'red')

points(1:22, cornPredictions2$pred, col = 'blue')
points(1:22, cornPredictions2$pred - 2 * cornPredictions2$se, col = 'blue')
points(1:22, cornPredictions2$pred + 2 * cornPredictions2$se, col = 'blue')
lines(1:22, cornPredictions2$pred, col = 'blue')
lines(1:22, cornPredictions2$pred - 2 * cornPredictions2$se, col = 'blue')
lines(1:22, cornPredictions2$pred + 2 * cornPredictions2$se, col = 'blue')

legend('bottomleft', legend = c('True Data', 'ARIMA(1, 1, 1) (ACF, PACF and BIC)', 'ARIMA(3, 1, 6) (AIC)'), col = c('black', 'red', 'blue'), lty = 1)

## Cotton
# ACF/PACF
acf(cottonDiffs, main = 'ACF of Cotton First Differences')
pacf(cottonDiffs, main = 'Partial ACF of Cotton First Differences')

cotton_aic_bic_vals = aic_bic(cottonTrain, 6, 6)
cotton_aic_bic_vals

cottonModel1 = arima(cottonTrain$value, order = c(2, 1, 1))
cottonModel2 = arima(cottonTrain$value, order = c(5, 1, 4))
cottonModel3 = arima(cottonTrain$value, order = c(3, 1, 2))

plot_predictions = function(trueData, ymin, ymax, title, model1, model1Name, model2, model2Name, model3, model3Name) {
  predictions1 = predict(model1, n.ahead = 22)
  predictions2 = predict(model2, n.ahead = 22)
  predictions3 = predict(model3, n.ahead = 22)
  
  plot(trueData$value, ylim = c(ymin, ymax), ylab = paste(title, 'Price'), main = paste(title, 'Test Set Predictions'))
  lines(trueData$value)
  
  # Predictions from model 1
  points(1:22, predictions1$pred, col = 'red')
  points(1:22, predictions1$pred + 2 * predictions1$se, col = 'red')
  points(1:22, predictions1$pred - 2 * predictions1$se, col = 'red')
  lines(1:22, predictions1$pred, col = 'red')
  lines(1:22, predictions1$pred + 2 * predictions1$se, col = 'red')
  lines(1:22, predictions1$pred - 2 * predictions1$se, col = 'red')
  
  # Predictions from model 2
  points(1:22, predictions2$pred, col = 'blue')
  points(1:22, predictions2$pred + 2 * predictions2$se, col = 'blue')
  points(1:22, predictions2$pred - 2 * predictions2$se, col = 'blue')
  lines(1:22, predictions2$pred, col = 'blue')
  lines(1:22, predictions2$pred + 2 * predictions2$se, col = 'blue')
  lines(1:22, predictions2$pred - 2 * predictions2$se, col = 'blue')
  
  # Predictions from model 3
  points(1:22, predictions3$pred, col = 'green')
  points(1:22, predictions3$pred + 2 * predictions3$se, col = 'green')
  points(1:22, predictions3$pred - 2 * predictions3$se, col = 'green')
  lines(1:22, predictions3$pred, col = 'green')
  lines(1:22, predictions3$pred + 2 * predictions3$se, col = 'green')
  lines(1:22, predictions3$pred - 2 * predictions3$se, col = 'green')
  
  legend('bottomleft', legend = c('True Data', model1Name, model2Name, model3Name), col = c('black', 'red', 'blue', 'green'), lty = 1)
}

plot_predictions(cottonTest, 0.35, 0.85, 'Cotton', cottonModel1, 'ARIMA(2, 1, 1) (ACF, PACF)', cottonModel2, 'ARIMA(5, 1, 4) (AIC)', cottonModel3, 'ARIMA(3, 1, 2) (BIC)')

## Soybeans
# ACF/PACF
acf(soybeansDiffs, main = 'ACF of Soybean First Differences')
pacf(soybeansDiffs, main = 'Partial ACF of Soybean First Differences')

soybeans_aic_bic_vals = aic_bic(soybeansTrain, 6, 6)
soybeans_aic_bic_vals

soybeansModel1 = arima(soybeansTrain$value, order = c(0, 1, 0))
soybeansModel2 = arima(soybeansTrain$value, order = c(4, 1, 5))
soybeansModel3 = arima(soybeansTrain$value, order = c(1, 1, 1))

plot_predictions(soybeansTest, 7.4, 10.5, 'Soybean', soybeansModel1, 'ARIMA(0, 1, 0) (ACF, PACF)', soybeansModel2, 'ARIMA(4, 1, 5) (AIC)', soybeansModel3, 'ARIMA(1, 1, 1) (BIC)')

## Wheat
# ACF/PACF
acf(wheatDiffs, main = 'ACF of Wheat First Differences')
pacf(wheatDiffs, main = 'Partial ACF of Wheat First Differences')

# AIC, BIC
wheat_aic_bic_vals = aic_bic(wheatTrain, 6, 6)
wheat_aic_bic_vals

# Models
wheatModel1 = arima(wheatTrain$value, order = c(0, 1, 0))
wheatModel2 = arima(wheatTrain$value, order = c(4, 1, 3))
wheatModel3 = arima(wheatTrain$value, order = c(1, 1, 1))

# Plot true data and predictions from each model
plot_predictions(wheatTest, 3.6, 6, 'Wheat', wheatModel1, 'ARIMA(0, 1, 0) (ACF, PACF)', wheatModel2, 'ARIMA(4, 1, 3) (AIC)', wheatModel3, 'ARIMA(1, 1, 1) (BIC)')

########## Recession Prediction ##########
cornRecessionTrain = corn[12200:12553, ]
cornRecessionTest = corn[12554:12575, ]
cottonRecessionTrain = cotton[8839:9191, ]
cottonRecessionTest = cotton[9192:9213, ]
soybeansRecessionTrain = soybeans[9819:10170, ]
soybeansRecessionTest = soybeans[10171:10192, ]
wheatRecessionTrain = wheat[12200:12553, ]
wheatRecessionTest = wheat[12554:12575, ]

cornRecessionDiffs = first_diffs(cornRecessionTrain)
cottonRecessionDiffs = first_diffs(cottonRecessionTrain)
soybeanRecessionDiffs = first_diffs(soybeansRecessionTrain)
wheatRecessionDiffs = first_diffs(wheatRecessionTrain)

acf(cornRecessionDiffs, main = 'ACF for Corn First Differences During Recession')
pacf(cornRecessionDiffs, main = 'Partial ACF for Corn First Differences During Recession')

corn_recession_aic_bic_vals = aic_bic(cornRecessionTrain, 6, 6)
corn_recession_aic_bic_vals

cornRecessionModel1 = arima(cornRecessionTrain$value, order = c(4, 1, 5))
cornRecessionModel2 = arima(cornRecessionTrain$value, order = c(1, 1, 1))

# Function to plot the recession data and the two models found using AIC and BIC
plot_recession = function(trueData, ymin, ymax, title, model1, model1Name, model2, model2Name) {
  predictions1 = predict(model1, n.ahead = 22)
  predictions2 = predict(model2, n.ahead = 22)
  
  plot(trueData$value, ylim = c(ymin, ymax), ylab = paste(title, 'Prices'), main = paste(title, 'Recession Test Set Predictions'))
  lines(trueData$value)
  
  # Plot predictions and confidence interval for model 1
  points(1:22, predictions1$pred, col = 'red')
  points(1:22, predictions1$pred + 2 * predictions1$se, col = 'red')
  points(1:22, predictions1$pred - 2 * predictions1$se, col = 'red')
  lines(1:22, predictions1$pred, col = 'red')
  lines(1:22, predictions1$pred + 2 * predictions1$se, col = 'red')
  lines(1:22, predictions1$pred - 2 * predictions1$se, col = 'red')
  
  # Plot predictions and confidence interval for model 2
  points(1:22, predictions2$pred, col = 'blue')
  points(1:22, predictions2$pred + 2 * predictions2$se, col = 'blue')
  points(1:22, predictions2$pred - 2 * predictions2$se, col = 'blue')
  lines(1:22, predictions2$pred, col = 'blue')
  lines(1:22, predictions2$pred + 2 * predictions2$se, col = 'blue')
  lines(1:22, predictions2$pred - 2 * predictions2$se, col = 'blue')
  
  legend('bottomleft', legend = c('True Data', model1Name, model2Name), col = c('black', 'red', 'blue'), lty = 1)
}
plot_recession(cornRecessionTest, 2.7, 5.3, 'Corn', cornRecessionModel1, 'ARIMA(4, 1, 5) (AIC)', cornRecessionModel2, 'ARIMA(1, 1, 1) (BIC)')

acf(cottonRecessionDiffs, main = 'ACF for Cotton First Differences During Recession')
pacf(cottonRecessionDiffs, main = 'Partial ACF for Cotton First Differences During Recession')

cotton_recession_aic_bic_vals = aic_bic(cottonRecessionTrain, 6, 6)
cotton_recession_aic_bic_vals

cottonRecessionModel1 = arima(cottonRecessionTrain$value, order = c(1, 1, 1))
cottonRecessionPredictions1 = predict(cottonRecessionModel1, n.ahead = 22)

plot(cottonRecessionTest$value, ylim = c(0.35, 0.7), ylab = 'Cotton Prices', main = 'Cotton Recession Test Set Predictions')
lines(cottonRecessionTest$value)

points(1:22, cottonRecessionPredictions1$pred, col = 'red')
points(1:22, cottonRecessionPredictions1$pred + 2 * cottonRecessionPredictions1$se, col = 'red')
points(1:22, cottonRecessionPredictions1$pred - 2 * cottonRecessionPredictions1$se, col = 'red')

lines(1:22, cottonRecessionPredictions1$pred, col = 'red')
lines(1:22, cottonRecessionPredictions1$pred + 2 * cottonRecessionPredictions1$se, col = 'red')
lines(1:22, cottonRecessionPredictions1$pred - 2 * cottonRecessionPredictions1$se, col = 'red')

legend('bottomleft', legend = c('True Data', 'ARIMA(1, 1, 1) (AIC and BIC)'), col = c('black', 'red'), lty = 1)

acf(soybeanRecessionDiffs, main = 'ACF for Soybeans First Differences During Recession')
pacf(soybeanRecessionDiffs, main = 'Partial ACF for Soybeans First Differences During Recession')

soybean_recession_aic_bic_vals = aic_bic(soybeansRecessionTrain, 6, 6)
soybean_recession_aic_bic_vals

soybeanRecessionModel1 = arima(soybeansRecessionTrain$value, order = c(2, 1, 4))
soybeanRecessionModel2 = arima(soybeansRecessionTrain$value, order = c(1, 1, 1))

plot_recession(soybeansRecessionTest, 7, 13.5, 'Soybean', soybeanRecessionModel1, 'ARIMA(2, 1, 4) (AIC)', soybeanRecessionModel2, 'ARIMA(1, 1, 1) (BIC)')

acf(wheatRecessionDiffs, main = 'ACF for Wheat First Differences During Recession')
pacf(wheatRecessionDiffs, main = 'Partial ACF for Wheat First Differences During Recession')

wheat_recession_aic_bic_vals = aic_bic(wheatRecessionTrain, 6, 6)
wheat_recession_aic_bic_vals

wheatRecessionModel1 = arima(wheatRecessionTrain$value, order = c(5, 1, 5))
wheatRecessionModel2 = arima(wheatRecessionTrain$value, order = c(3, 1, 1))

plot_recession(wheatRecessionTest, 3.2, 7.5, 'Wheat', wheatRecessionModel1, 'ARIMA(5, 1, 5) (AIC)', wheatRecessionModel2, 'ARIMA(3, 1, 1) (BIC)')


corn_cotton = lm(cornTrain$value ~ cottonTrain$value)
corn_soybeans = lm(cornTrain$value ~ soybeansTrain$value)
corn_wheat = lm(cornTrain$value ~ wheatTrain$value)
cotton_soybeans = lm(cottonTrain$value ~ soybeansTrain$value)
cotton_wheat = lm(cottonTrain$value ~ wheatTrain$value)
soybeans_wheat = lm(soybeansTrain$value ~ wheatTrain$value)
summary(corn_cotton)
summary(corn_soybeans)
summary(corn_wheat)
summary(cotton_soybeans)
summary(cotton_wheat)
summary(soybeans_wheat)



my_lag = function(data, n) {
  name = paste('y_minus_', n, sep = '')
  data[name] = NA
  for (i1 in (n + 1):length(data$value)) {
    data[name][i1, ] = data$value[i1 - n]
  }
  return(data)
}

for (i1 in 1:6) {
  cornTrain = my_lag(cornTrain, i1)
  cottonTrain = my_lag(cottonTrain, i1)
  soybeansTrain = my_lag(soybeansTrain, i1)
  wheatTrain = my_lag(wheatTrain, i1)
}

cornReg = lm(cornTrain$value ~ cornTrain$y_minus_1
             + cornTrain$y_minus_2
             + cornTrain$y_minus_3
             + cornTrain$y_minus_4
             + cornTrain$y_minus_5
             + cornTrain$y_minus_6
             + cottonTrain$value
             + cottonTrain$y_minus_1
             + cottonTrain$y_minus_2
             + cottonTrain$y_minus_3
             + cottonTrain$y_minus_4
             + cottonTrain$y_minus_5
             + cottonTrain$y_minus_6
             + soybeansTrain$value
             + soybeansTrain$y_minus_1
             + soybeansTrain$y_minus_2
             + soybeansTrain$y_minus_3
             + soybeansTrain$y_minus_4
             + soybeansTrain$y_minus_5
             + soybeansTrain$y_minus_6
             + wheatTrain$value
             + wheatTrain$y_minus_1
             + wheatTrain$y_minus_2
             + wheatTrain$y_minus_3
             + wheatTrain$y_minus_4
             + wheatTrain$y_minus_5
             + wheatTrain$y_minus_6)

cottonReg = lm(cottonTrain$value ~ cornTrain$value
               + cornTrain$y_minus_1
               + cornTrain$y_minus_2
               + cornTrain$y_minus_3
               + cornTrain$y_minus_4
               + cornTrain$y_minus_5
               + cornTrain$y_minus_6
               + cottonTrain$y_minus_1
               + cottonTrain$y_minus_2
               + cottonTrain$y_minus_3
               + cottonTrain$y_minus_4
               + cottonTrain$y_minus_5
               + cottonTrain$y_minus_6
               + soybeansTrain$value
               + soybeansTrain$y_minus_1
               + soybeansTrain$y_minus_2
               + soybeansTrain$y_minus_3
               + soybeansTrain$y_minus_4
               + soybeansTrain$y_minus_5
               + soybeansTrain$y_minus_6
               + wheatTrain$value
               + wheatTrain$y_minus_1
               + wheatTrain$y_minus_2
               + wheatTrain$y_minus_3
               + wheatTrain$y_minus_4
               + wheatTrain$y_minus_5
               + wheatTrain$y_minus_6)

wheatReg = lm(wheatTrain$value ~ cornTrain$value
              + cornTrain$y_minus_1
              + cornTrain$y_minus_2
              + cornTrain$y_minus_3
              + cornTrain$y_minus_4
              + cornTrain$y_minus_5
              + cornTrain$y_minus_6
              + cottonTrain$value
              + cottonTrain$y_minus_1
              + cottonTrain$y_minus_2
              + cottonTrain$y_minus_3
              + cottonTrain$y_minus_4
              + cottonTrain$y_minus_5
              + cottonTrain$y_minus_6
              + soybeansTrain$value
              + soybeansTrain$y_minus_1
              + soybeansTrain$y_minus_2
              + soybeansTrain$y_minus_3
              + soybeansTrain$y_minus_4
              + soybeansTrain$y_minus_5
              + soybeansTrain$y_minus_6
              + wheatTrain$y_minus_1
              + wheatTrain$y_minus_2
              + wheatTrain$y_minus_3
              + wheatTrain$y_minus_4
              + wheatTrain$y_minus_5
              + wheatTrain$y_minus_6)

soybeansReg = lm(soybeansTrain$value ~ cornTrain$value
                 + cornTrain$y_minus_1
                 + cornTrain$y_minus_2
                 + cornTrain$y_minus_3
                 + cornTrain$y_minus_4
                 + cornTrain$y_minus_5
                 + cornTrain$y_minus_6
                 + cottonTrain$value
                 + cottonTrain$y_minus_1
                 + cottonTrain$y_minus_2
                 + cottonTrain$y_minus_3
                 + cottonTrain$y_minus_4
                 + cottonTrain$y_minus_5
                 + cottonTrain$y_minus_6
                 + soybeansTrain$y_minus_1
                 + soybeansTrain$y_minus_2
                 + soybeansTrain$y_minus_3
                 + soybeansTrain$y_minus_4
                 + soybeansTrain$y_minus_5
                 + soybeansTrain$y_minus_6
                 + wheatTrain$value
                 + wheatTrain$y_minus_1
                 + wheatTrain$y_minus_2
                 + wheatTrain$y_minus_3
                 + wheatTrain$y_minus_4
                 + wheatTrain$y_minus_5
                 + wheatTrain$y_minus_6)







