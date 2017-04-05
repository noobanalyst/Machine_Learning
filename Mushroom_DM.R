# Decision rules algorithm example

# Loading relevant libraries
library(RWeka) # For JRip RIPPER alogrithm

# Loading the data intoa a data frame
mush <- read.csv("mushrooms.csv")

# Checking the loaded data
str(mush)

# Removing veil type variable as it is a single level factor variable
mush$veil_type <- NULL

# Performing explnatory analysis
prop.table(table(mush$type))*100

# Building a simple 1R rule learner
model_1R <- OneR(type~.,data = mush)

# Evaluating this model performance
summary(model_1R)

# Improving the 1R rule learner with a RIPPER algorithm implementation
model <- JRip(type~.,data = mush)
model

# Evaluating the model performance
summary(model)
