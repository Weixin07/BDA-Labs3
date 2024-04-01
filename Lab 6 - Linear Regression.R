#Regression
#
#Linear Regression is one of the most widely used regression techniques to model the relationship between two variables. It uses a linear relationship to model the regression line. There are 2 variables used in the linear relationship equation i.e., the predictor variable and the response variable.
#
#Linear Regression Equation:
#  
#  y = a x + b
#
#where,
#
#x indicates predictor or independent variable
#y indicates response or dependent variable
#a and b are coefficients




# Height vector 
x <- c(153, 169, 140, 186, 128, 
       136, 178, 163, 152, 133) 

# Weight vector 
y <- c(64, 81, 58, 91, 47, 57, 
       75, 72, 62, 49) 

# Create a linear regression model 
model <- lm(y~x) 

# Print regression model 
print(model) 

# Find the weight of a person 
# With height 182 
df <- data.frame(x = 182) 
res <- predict(model, df) 
cat("\nPredicted value of a person 
			with height = 182") 
print(res) 

#Set directory to Drive D: as desktop (C:) and G.Drive (G:) is not writable from R Studio
setwd("D:")
# Output to be present as PNG file 
png(file = "linearRegGFG.png") 

# Plot 
plot(x, y, main = "Height vs Weight 
				Regression model") 
abline(lm(y~x)) 

# Save the file. 
dev.off() 


