# Loading relevant libraries
library(xts) # For timeseries functions
library(ggplot2)
library(broom)
library(dygraphs) # For plotting interactive graphs


# Loading the data into a data frame
nyse <- read.csv("C:/Users/User/Desktop/nyse/prices.csv")

# Extracting specific symbol data
ibmdata <- nyse[nyse$symbol=="IBM",]

# Converting the data into a timeseries
ibmts <- xts(ibmdata[,c(-1,-2)],order.by = as.Date(ibmdata[,1]),frequency = 365)
head(ibmts)

# Plotting the timeseries
tidy(ibmts[,-5]) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line()

# The same graph with interactive feature
dygraph(ibmts[-5])%>%dyRangeSelector()
