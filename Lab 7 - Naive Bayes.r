#Consider a sample space:   {HH, HT, TH, TT}
#where, H: Head T: Tail
#
#P(Second coin being head given  = P(A|B)
#  first coin is tail) = P(A|B)  
#= [P(B|A) * P(A)] / P(B)
#= [P(First coin is tail given second coin is head) * 
#     P(Second coin being Head)] / P(first coin being tail)
#= [(1/2) * (1/2)] / (1/2)
#= (1/2) 
#= 0.5


#Iris dataset consists of 50 samples from each of 3 species of 
#Iris(Iris setosa, Iris virginica, Iris versicolor)

# Loading data
data(iris)

# Structure 
str(iris)

# Installing Packages
#install.packages("e1071")
#install.packages("caTools")
#install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)

# Splitting data into train and test data
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Fitting Naive Bayes Model to training dataset
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(Species ~ ., data = train_cl)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$Species, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)
