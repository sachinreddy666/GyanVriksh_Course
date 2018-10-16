# Linear Regression : Revisit

head(cars)  # display the first 6 observations

# Graphical Analysis
## The aim of this exercise is to build a simple regression model that we can use to predict Distance (dist)
##by establishing a statistically significant linear relationship with Speed (speed). 

##1. Scatter plot: Visualize the linear relationship between the predictor and response
##2. Box plot: To spot any outlier observations in the variable.
##Having outliers in your predictor can drastically affect the predictions as they can easily 
##affect the direction/slope of the line of best fit.
##3. Density plot: To see the distribution of the predictor variable.
#Ideally, a close to normal distribution (a bell shaped curve), without being skewed
#to the left or right is preferred. Let us see how to make each one of them.

# Scatter Plot

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

## one of the underlying assumptions in linear regression is that the relationship between the 
## response and predictor variables is linear and additive.


# BoxPlot - Check for outliers

#datapoint that lies outside the 1.5 * interquartile-range (1.5 * IQR) is considered an outlier

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'


# Density plot - Check if the response variable is close to normality

#install.packages('e1071')
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")


# Correlation
## Correlation is a statistical measure that suggests the level of linear dependence
##between two variables, that occur in pair

cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 

# Build Linear Model: The lm() function

linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)


# => dist = ???17.579 + 3.932???speed

# Linear Regression Diagnostics : statistically significant?

summary(linearMod)  # model summary


modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

print(t_value)
print(p_value)


# R-Squared and Adj R-Squared

# The actual information in a data is the total variation it contains, remember?. 
# What R-Squared tells us is the proportion of variation in the dependent (response) #
# variable that has been explained by this model.


# R^2=1???[SSE/SST]

# What about adjusted R-Squared?

# R^2adj= 1???[MSE/MST]


##where, MSE is the mean squared error given by MSE=SSE(n???q) and MST=SST(n???1) is the mean 
##squared total, where n is the number of observations and q is the number of coefficients in the model.

# AIC and BIC

## The AIC is defined as:


# AIC=(???2)×ln(L)+(2×k)
# where, k is the number of model parameters and the BIC is defined as:

# BIC=(???2) × ln(L)+k × ln(n)

# where, n is the sample size.

# For model comparison, the model with the lowest AIC and BIC score is preferred.

AIC(linearMod)  # AIC => 419.1569
BIC(linearMod)  # BIC => 424.8929

# How to know if the model is best fit for your data? : Use the combination metrics discussed above

# Predicting with Linear Models

# Step 1: Create the training (development) and test (validation) data samples from original data.
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data
## Step 2: Develop the model on the training data and use it to predict the distance on test data
# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
# Step 3: Review diagnostic measures.
summary (lmMod)  # model summary


# Step 4: Calculate prediction accuracy and error rates

# A simple correlation between the actuals and predicted values

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

## accuracy test

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)


# k- Fold Cross validation

install.packages('DAAG')
library(DAAG)
attach(cars)
cvResults <- CVlm(data = cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals.");
# performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error

















































