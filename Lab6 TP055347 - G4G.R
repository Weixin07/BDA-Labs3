# Create the data frame (By converting each data values into a frame)
data <- data.frame(
  Years_Exp = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
  Salary = c(39343.00, 46205.00, 37731.00, 43525.00,
             39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00)
)

# Create the scatter plot
plot(data$Years_Exp, data$Salary,
     xlab = "Years Experienced",
     ylab = "Salary",
     main = "Scatter Plot of Years Experienced vs Salary")

# Conduct a Simple Linear Regression
install.packages('caTools')
library(caTools)

# To split data into a ratio of 7:3, where 70% is used for training, and 30% for testing
split = sample.split(data$Salary, SplitRatio = 0.7)
trainingset = subset(data, split == TRUE) # To set the training dataset to be the split-ted value from the prior code of retrieving only 70% of the code
testset = subset(data, split == FALSE) # To set the testing dataset to retrieve the leftovers from the splitting code

# Fitting Simple Linear Regression to the Training set
lm.r= lm(formula = Salary ~ Years_Exp,
         data = trainingset)
#Summary of the model
summary(lm.r)

# Predicting Values
# Create a data frame with new input values
new_data <- data.frame(Years_Exp = c(4.0, 4.5, 5.0))

# Predict using the linear regression model
predicted_salaries <- predict(lm.r, newdata = new_data)

# Display the predicted salaries
print(predicted_salaries)

# Visualising the Training set results
ggplot() + geom_point(aes(x = trainingset$Years_Ex, 
                          y = trainingset$Salary), colour = 'red') +
  geom_line(aes(x = trainingset$Years_Ex,
                y = predict(lm.r, newdata = trainingset)), colour = 'blue') +
  
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = testset$Years_Exp, y = testset$Salary),
             colour = 'red') +
  geom_line(aes(x = trainingset$Years_Exp,
                y = predict(lm.r, newdata = trainingset)), 
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

