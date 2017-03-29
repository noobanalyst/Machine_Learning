library(ggplot2) # For visualisation
library(gmodels) # For Cross tabulation function
library(dplyr) # For case_when function
library(vcd) # For calculating kappa statistic
library(randomForest) # For building the randomforest model

# Reading the data into a data frame. 
# Note: As the name of the first column of the data has the character "#" in it, hence we need to set the "comment.char" attribute of the read.table function as empty space instead of default #.

crm <- read.table("ModelingData.txt", header = TRUE, comment.char = "")

# Checking the loaded data
str(crm)

# Converting int variable to factors
crm[,-c(1,3,11,14,23,27,29)] <- as.data.frame(lapply(crm[,-c(1,3,11,14,23,27,29)],as.factor))

crm <- crm[-1] # Removing OBS number
levels(crm$RESPONSE) <- c("No","Yes") # Changing levels for RESPONSE

prop.table(table(crm$CHK_ACCT))*100
# Conclusion: Around 27% records have less than 0 DM in their checking account while around 39% records dont have a checking account.


prop.table(table(crm$HISTORY))*100
#Conclusion: There are around 29% records with critical accounts, almost 9% with delayed payments while 4% records havent taken any credits.

summary(crm$DURATION)
#Conclusion: The duration ranges from 4 months to 72 months with a median of 18 months

prop.table(table(crm$SAV_ACCT))*100
#Conclusion: Around 60% records have less than 100 DM on an average in their savings account while around 18% records have unknown/no savings account

summary(crm$AMOUNT)
#Conclusion: The loan amount ranges from 250 DM to 18420 DM with a median of 2320 DM

prop.table(table(crm$RESPONSE))*100
#Conclusion: We can see that 30% of the records have bad credit rating

# Creating a combined variable APPLICANT
crm$APPLICANT <-factor(case_when(
        crm$MALE_DIV==1 ~ "MALE_DIV",
        crm$MALE_MAR_or_WID==1 ~ "MALE_MAR_or_WD",
        crm$MALE_SINGLE==1 ~ "MALE_SINGLE",
        TRUE ~ "OTHER"),labels = c("MD","MWD","MS","OTH")
)

# Applicant v/s Response
addmargins(xtabs(~crm$APPLICANT+crm$RESPONSE))

# Creating a combined variable PURPOSE
crm$PURPOSE <-  factor(case_when(
        crm$NEW_CAR==1 ~ "NEW_CAR",
        crm$USED_CAR==1 ~ "USED_CAR",
        crm$FURNITURE==1 ~ "FURNITURE",
        crm$RADIO.TV==1 ~ "RADIO.TV",
        crm$EDUCATION==1 ~ "EDUCATION",
        crm$RETRAINING==1 ~ "RETRAINING",
        TRUE ~ "OTHER")
)

# Purpose v/s Response
addmargins(xtabs(~crm$PURPOSE+crm$RESPONSE))


# Plotting Applicant and Response while aggregating them by Purpose
ggplot(crm[1:1000,], aes(APPLICANT, fill=RESPONSE))+geom_bar()+facet_grid(.~PURPOSE)


# Plotting histogram for Age while aggregating them by Purpose
ggplot(crm[1:1000,], aes(AGE, fill=RESPONSE))+geom_histogram()+facet_grid(.~PURPOSE)
# Conclusion: Now with type of applicant. We can observe that the older married or widowed applicants have better creditworthiness.

# Creating training and test datasets
crm_train <- crm[1:900,]
crm_train <- crm_train[-c(4,5,6,7,8,9,14,15,16)]
crm_test <- crm[901:1000,]
crm_test <- crm_test[-c(4,5,6,7,8,9,14,15,16)]

# Creating the Data Model
set.seed(123)
crm_model <- randomForest(crm_train[-22],crm_train$RESPONSE,importance = TRUE)

# Visualising the relatively important variables
imp <- importance(crm_model,type=1)
fimp <- data.frame(Feature=row.names(imp), Importance=imp[,1])
ggplot(fimp, aes(x=reorder(Feature, Importance), y=Importance, fill=Importance)) +
        geom_bar(stat="identity") +
        coord_flip()+
        xlab("") +
        ylab("Importance")

# Evaluating Model Performance
crm_predict <- predict(crm_model,crm_test)

# Building the confusion matrix
CrossTable(crm_test$RESPONSE,crm_predict, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE,dnn = c("Actual","Predicted"))

# Evaluating the kappa statistic
Kappa(table(crm_test$RESPONSE,crm_predict))
