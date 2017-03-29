library(ggplot2)
library(gmodels)
library(caret)
library(randomForest)


# Loading the data into a data frame
hr_data <- read.csv("HR_comma_sep.csv")

# Checking the loaded data
str(hr_data)

# Converting relevant fields to factors for convinience
hr_data$work_accident <- factor(hr_data$work_accident,labels = c("No","Yes"))
hr_data$left <- factor(hr_data$left,labels = c("No","Yes"))
hr_data$promotion_last_5years <- factor(hr_data$promotion_last_5years,labels = c("No","Yes"))

# Creating training and test datasets based on a 80/20 split
set.seed(123)
row_num <- sample(14999,12000)
hr_train <- hr_data[row_num,]
hr_test <- hr_data[-row_num,]

# Running basic exploratory analysis on the data
summary(hr_data$satisfaction_level)
summary(hr_data$last_evaluation)
summary(hr_data$number_project)

summary(hr_data$average_montly_hours)
# On an average, there are 174 working hours in a month. The summary here reflects that 
# almost 75% of people are working more than the average hours.

summary(hr_data$time_spend_company)
table(hr_data$sales)

prop.table(table(hr_data$salary))*100
# Almost 49% of people are paid low salaries.

by(hr_data$average_montly_hours,hr_data$salary,summary)
# The average monthly hours distribution seems to be uniform across various salary levels. 

prop.table(table(hr_data$left))*100
# Of total 14999 people, around 23% of people have left the organisation

hr_model <- randomForest(hr_train[-7],hr_train$left,importance = TRUE)
imp <-importance(hr_model,type=1)
fimp<-data.frame(Feature=row.names(imp), Importance=imp[,1])
ggplot(fimp, aes(x=reorder(Feature, Importance), y=Importance, fill=Importance)) +
  geom_bar(stat="identity") +
  coord_flip()+
  xlab("") +
  ylab("Importance")

hr_predict <- predict(hr_model,hr_test)
# CrossTable(hr_test$left, hr_predict, prop.r = FALSE,prop.c = FALSE,prop.chisq = FALSE,dnn = c("Actual","Predicted"))
confusionMatrix(hr_test$left, hr_predict, positive = 'Yes')
