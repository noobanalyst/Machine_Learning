# Loading relevant libraries
library(tm) # Text mining package used to create data corpus
library(SnowballC) # Package used for stemming
library(wordcloud)
library(e1071) # Package containing the NaiveBayes function
library(caret)

# Loading the data intoa a data frame
sms_data <- read.csv("sms_spam.csv",stringsAsFactors = FALSE)

# Checking the loaded data
str(sms_data)

# Converting type variable to factor
sms_data$type <- factor(sms_data$type)
table(sms_data$type)
prop.table(table(sms_data$type))*100

# Visualising data by creating a wordcloud
spam <- subset(sms_data,type=="spam")
ham <- subset(sms_data,type=="ham")
par(mfrow=c(1,2))
wordcloud(spam$text,max.words = 50,scale = c(3,0.5))
wordcloud(ham$text,max.words = 50,scale = c(3,0.5))

# Data preparation and cleanising by creating a data corpus to run Naive Bayes' algorithm
sms_corpus <- VCorpus(VectorSource(sms_data$text))

# Checking the newly created corpus
print(sms_corpus)
inspect(sms_corpus)

# Creating a document term matrix and also cleansing the data by transforming upper case letters, 
# removing numbers, removing stopwords like "to,and,but etc." and removing punctuations. 
# Finally the words are reduced to their root form, a process called as stemming.
sms_dtm <- DocumentTermMatrix(sms_corpus, control = list(
                              tolower=TRUE,
                              removeNumbers = TRUE,
                              stopwords = TRUE,
                              removePunctuation = TRUE,
                              stemming = TRUE
))


# Creating training and test datasets based on a 75/25 split.
sms_dtm_train <- sms_dtm[1:4180,]
sms_dtm_test <- sms_dtm[4181:5574,]

# Including only those features/words that appear in at least 5 messages
sms_freq <- findFreqTerms(sms_dtm_train,5)
sms_dtm_freq <- sms_dtm[,sms_freq]

# Modifying the training and test datasets to include only the frequent words
sms_dtm_ftrain <- sms_dtm_train[,sms_freq]
sms_dtm_ftest <- sms_dtm_test[,sms_freq]

# Creating training and test data labels and confirming that they arent biased towards any particular type.
sms_train_labels <- sms_data[1:4180,]$type
sms_test_labels <- sms_data[4181:5574,]$type
prop.table(table(sms_train_labels))*100
prop.table(table(sms_test_labels))*100

# Creating a function to convert the word count to string (Yes/No)
convert_string <- function(x){
  x <- ifelse(x>0,"Yes","No")
}
sms_dtm_ftrain <- apply(sms_dtm_ftrain, MARGIN = 2, convert_string)
sms_dtm_ftest <- apply(sms_dtm_ftest, MARGIN = 2, convert_string)

# Training the model
sms_model <- naiveBayes(sms_dtm_ftrain,sms_train_labels,laplace=1)

# Evaluating model performance
sms_predict <- predict(sms_model,sms_dtm_ftest)

# Confusion matrix for various metrics
confusionMatrix(sms_predict,sms_test_labels,positive="spam")
