# Bradley Gadd-Speakman (cqmn68)
# Assignment 1

# Sets current directory to be the working directory (requires pkg 'rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# QUESTION 1.1

# Gets the 43 students gender and height data from the statistical methods III 
# class of 1993 at Durham University
data = read.csv("Student.csv")

# Fits a linear regression model to the data with height as covariate
linmodel = lm(gender ~ height, data)

# Gets coefficients of linear regression
coeff = as.numeric(coef(linmodel))
# Intercept: 6.476, Height grad: -0.088

# Plots the data in a scatter plot
plot(data$height, data$gender, main = "Gender Plotted Against Height",
     xlab = "Height (inches)", ylab = "Gender (0-Male, 1-Female)")

# Appends the linear model regression line onto the plot in red
abline(linmodel, col = "red")

# The linear regression model is an inappropriate model to use for this data
# since the response variable gender is categorical data and can only take
# two discrete values. Regression models assume that the variables are
# continuous data as they try to estimate the expected value of the response
# in a continuous and linear way.

# QUESTION 1.2

# Fits a logistic regression model with logit link to the data with 
# height as covariate
logmodel = glm(gender ~ height, family = binomial(link = logit), data)

# QUESTION 1.3

# Get coefficients of logistic model
coeff2 = as.numeric(coef(logmodel))
# Intercept: 77.713, Height grad: -1.153
# The intercept is the intercept of the log odds of being female
# as a function of height
# The gradient is the gradient of the log odds of being female 
# as a function of height

# Get predictions of logistic model
predictions = data.frame(height = seq(min(data$height), max(data$height), len = 100))
predictions$gender_pred = predict(logmodel, predictions, type = "response")

# Plot prediction values from the logistic model on the plot
lines(predictions, col = "green")

# QUESTION 1.4

# Fits a logistic regression model with probit link to the data with
# height as covariate
logmodel2 = glm(gender ~ height, family = binomial(link = probit), data)

# Get coefficients of logistic model2
coeff3 = as.numeric(coef(logmodel2))
# Intercept: 45.427, Height grad: -0.674

# Get predictions of logistic model2
predictions2 = data.frame(height = seq(min(data$height), max(data$height), len = 100))
predictions2$gender_pred = predict(logmodel2, predictions2, type = "response")

# Plot prediction values from the logistic model2 on the plot
lines(predictions2, col = "blue", lty = "dotted")

# QUESTION 1.4a

# linmodel predicts 0.5 at h = (0.5-b0)/b1 = 67.858
(0.5 - coeff[1])/coeff[2]

# logmodel predicts 0.5 at h = -b0/b1 = 67.405
-coeff2[1]/coeff2[2]

# logmodel2 predicts 0.5 at h = -b0/b1 = 67.393
-coeff3[1]/coeff3[2]

#QUESTION 1.4b

# logmodel predicts probability of 0.00498 of being female given h = 72
predict(logmodel, data.frame(height = 72), type = "response")

# logmodel2 predicts probability of 0.000950 of being female given h = 72
predict(logmodel2, data.frame(height = 72), type = "response")

