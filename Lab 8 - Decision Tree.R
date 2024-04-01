install.packages("rpart")
install.packages("rpart.plot")
install.packages("caTools")

#Loading package
library(rpart)
library(rpart.plot)
library(caTools)

#Load the CSV dataset
data <- read.csv("D:/BDA Labs/framingham.xls")


#checking for the shape of the data 
dim(data) 

# checking for the column in the dataset 
str(data) 

# summary statistics of the dataset 
summary(data) 

#Splitting the dataset 
set.seed(123) 

#split the dataset into training and testing  
split = sample.split(data, SplitRatio = 0.75) 
training_set = subset(data, split == TRUE) 
test_set = subset(data, split == FALSE) 

#Applying decision tree algorithms 
tree <- rpart(TenYearCHD ~ ., data = training_set) 
rpart.plot(tree)
