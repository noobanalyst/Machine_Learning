# Loading relevant libraries
library(ggplot2)
library(rpart) # For regression trees
library(rpart.plot) # For visualising regression trees
library(RWeka)

# Loading the data intoa a data frame
wine <- read.csv("whitewines.csv")

# Checking the loaded data
str(wine)

# Building a histogram of wine quality
ggplot(wine, aes(wine$quality))+geom_histogram()

# Creating training and test datasets based on a 75/25 split
wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

# Building data model
wine_model <- rpart(quality~.,data = wine_train)

# Viewing the detailed summary of the model
summary(wine_model)

# Visualising the regression tree
rpart.plot(wine_model,digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Evaluating model performance
wine_pred <- predict(wine_model,wine_test)
summary(wine_pred)
cor(wine_pred,wine_test$quality)

# Improving model performance by using M5P function from RWeka package
wine_m5 <- M5P(quality~., data = wine_train)

# Examining the updated regression tree
wine_m5
summary(wine_m5)

# Evaluating model performance
wine_pred_m5 <- predict(wine_m5,wine_test)
summary(wine_pred_m5)
cor(wine_pred_m5,wine_test$quality)
