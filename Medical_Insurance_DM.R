# Loading relevant libraries
library(ggplot2)

# Loading the data intoa a data frame
insurance <- read.csv("insurance.csv")

# Checking the loaded data
str(insurance)

# Data exploration 
summary(insurance$charges)
ggplot(insurance,aes(insurance$charges))+geom_histogram()
table(insurance$region)
cor(insurance[c("age","bmi","children","charges")])

# Visualising the correlation matrix
pairs(cor(insurance[c("age","bmi","children","charges")]))

# Adding new features to build a better model
# Adding a non-linear relationship for age since the progression of age results in increase in medical insurance
insurance$age2 <- insurance$age^2

# Obese people tend to have higher medical expenses, so creating a binary indicator to indicate obesity
insurance$bmi30 <- ifelse(insurance$bmi>30,1,0)

# Creating the data model. 
# Smokers tend to have higher expenses, obese smokers more so. So including an interaction.
insurance_model <- lm(charges~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)

# Summarising the model
summary(insurance_model)
