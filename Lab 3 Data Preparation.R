library(readr)
library(dplyr)
library(VIM)
library(ggplot2)

#use the Starwars dataset from the dplyr package. The dataset provides 
#descriptions of 87 characters from the Starwars universe on 13 variables.

#SELECT VARIABLES
library(dplyr)

freshdata <- select(starwars, name:species)

# keep the variables name, height, and gender
newdata <- select(starwars, name, height, gender)

newdata

# keep the variables name and all variables 
# between mass and species inclusive
newdata <- select(starwars, name, mass:species)

newdata

# keep all variables except birth_year and gender
newdata <- select(starwars, -birth_year, -gender)
newdata

# keep all variables except sex, gender, name, eye_color
newdata <- select(starwars, -sex, -gender, -name, -eye_color)
newdata

#SELECT DATASETS REQUIRED
#The filter function allows you to limit your dataset to observations (rows) meeting a specific criteria. Multiple criteria can be combined with the & (AND) and | (OR) symbols.

newdata <- select(starwars, name, mass:species)

# select females
newdata <- filter(starwars, sex == "female")
newdata

# select females with blue eyes
newdata <- filter(starwars, sex == "female" & eye_color == "blue")
newdata

# select females with blue eyes that is taller than 150
newdata <- filter(starwars, sex == "female" & eye_color == "blue" & height>150)
newdata

# Creating/Recode variables
# The mutate function allows you to create new variables or transform existing ones

# convert height in centimeters to inches, and mass in kilograms to pounds
newdata <- mutate(starwars, 
                  height = height * 0.394,
                  mass   = mass   * 2.205)


# if height is greater than 180 then heightcat = "tall", 
# otherwise heightcat = "short"
newdata <- mutate(starwars, 
                  heightcat = ifelse(height > 180, 
                                     "tall", 
                                     "short"))
                  
                  # convert any eye color that is not black, blue or brown, to other.
                  newdata <- mutate(starwars, 
                                    eye_color = ifelse(eye_color %in% c("black", "blue", "brown"),
                                                       eye_color,
                                                       "other"))
newdata
                                                      
                                    # set heights greater than 200 or less than 75 to missing
                                    newdata <- mutate(starwars, 
                                                      height = ifelse(height < 75 | height > 200,
                                                                      NA,
                                                                      height))

newdata
                                    
#Summarize Data
#The summarize function can be used to reduce multiple values down to a single value (such as a mean). It is often used in conjunction with the by_group function, to calculate statistics by group. In the code below, the na.rm=TRUE option is used to drop missing values before calculating the means.
# calculate mean height and mass
newdata <- summarize(starwars, 
                     mean_ht = mean(height, na.rm=TRUE), 
                     mean_mass = mean(mass, na.rm=TRUE))
summary(newdata)

# calculate mean height and weight by gender
newdata <- group_by(starwars, sex)
newdata <- summarize(newdata, 
                     mean_ht = mean(height, na.rm=TRUE), 
                     mean_wt = mean(mass, na.rm=TRUE))
newdata

#Feature Selection
data(msleep, package="ggplot2")

# what is the proportion of missing data for each variable?
pctmiss <- colSums(is.na(msleep))/nrow(msleep)
round(pctmiss, 2)

# Impute missing values using the (5) k nearest neighbors
library(VIM)
newdata <- kNN(msleep, k=5)

