
#----------------------------------- EXERCISE ---------------------------------------
#use the Naive Bayes Technique to classify such passengers 
# load libraries
library(e1071)
library(caTools)
library(caret)

# load the Titanic dataset
data("Titanic")
#Save into a data frame and view it
Titanic_df = as.data.frame(Titanic)

#We see that there are 32 observations which represent all possible combinations of Class, Sex, Age and Survived with their frequency. Since it is 
#summarised, this table is not suitable for modelling purposes. We need to expand
# the table into individual rows.

##Creating data from table
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq) #This will repeat each combination equal to the frequency of each combination

# Create the dataset by row repetition created
Titanic_dataset=Titanic_df[repeating_sequence,]

# We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL

# Convert columns to factors if they are not already
Titanic_dataset$Class <- as.factor(Titanic_dataset$Class)
Titanic_dataset$Sex <- as.factor(Titanic_dataset$Sex)
Titanic_dataset$Age <- as.factor(Titanic_dataset$Age)
Titanic_dataset$Survived <- as.factor(Titanic_dataset$Survived)

# Splitting data into train and test data
set.seed(123) # Setting seed for reproducibility
split <- sample.split(Titanic_dataset$Survived, SplitRatio = 0.7)
train_set <- subset(Titanic_dataset, split == TRUE)
test_set <- subset(Titanic_dataset, split == FALSE)

# Fitting Naive Bayes Model to the training dataset
# Note that we don't need to scale features for Naive Bayes, especially for categorical data
naive_bayes_model <- naiveBayes(Survived ~ ., data = train_set)

# Predicting the Test set results
predictions <- predict(naive_bayes_model, newdata = test_set)

# Confusion Matrix to evaluate predictions
confusion_matrix <- table(test_set$Survived, predictions)

# Print the confusion matrix
print(confusion_matrix)

# Model Evaluation
# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the accuracy
print(paste("Accuracy:", accuracy))
confusionMatrix(predictions, test_set$Survived)

