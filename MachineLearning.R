# Loading Dataset to Data tables in CSV Format
# Data Scrapped from http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# Treating first row as header's in Data Tables
initialData <- read.csv2('Jan_2015_ontime.csv',sep=",",header=TRUE, stringsAsFactors = FALSE)


# That is a lot of rows to process so to speed thing up let's restrict data to only flight between certain large majorAirports
majorAirports <-c('ATL','LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')

# Query to Subset data from Main Data Table
initialData <- subset(initialData, DEST %in% majorAirports & ORIGIN %in% majorAirports)


# Taking a look at first two rows in Data Table
head(initialData,2)

# Taking a look at last two rows in Data Table
tail(initialData,2)


# Removing Null Values [Data Cleaning or Data Munging]
initialData$X <- NULL


# In general we want eliminate any columns that we do not need.  
# In particular, we want to eliminate columns that are duplicates or provide the same information
# We can do this by 
# 1. Visual inspect if we have columns that are really the same.  But visual inspection is error prone
# and does not deal with a second issue of correlation.
# 2. Often there are correlated columns such as an ID and the text value for the ID.  
#    And these highly correlated columns usually do not add information about the 
#    how the data causes changes in the results, but do cause the effect of a field to be overly 
#    amplified because some algorithm naively treat ever columns as being independant and just as important.
#
head(initialData,10)
# In looking at the data I see the possible correlations between ORIGIN_AIRPORT_SEQ_ID and ORIGIN_AIRPORT_ID
# and between DEST_AIRPORT_SEQ_ID and DEST_AIRPORT_ID.  I am not sure we will use these fields,
# but if they are correlated we need only one of each pair
#
#  Let's Check the values using corrilation function, cor().  Closer to 1 =>  more correlated
cor(initialData[c("ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_AIRPORT_ID")])
# Wow.  A perfect 1.  So ORIGIN_AIRPORT_SEQ_ID and ORIGIN_AIRPORT_ID are moving in lock step.
# Let's check DEST_AIRPORT_SEQ_ID, DEST_AIRPORT_ID
cor(initialData[c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")])
# Another perfect 1.  So DEST_AIRPORT_SEQ_ID and DEST_AIRPORT_SEQ_ID are also moving in lock step.
#
# Let's drop the columns ORIGIN_AIRPORT_SEQ_ID and DEST_AIRPORT_SEQ_ID since they are not providing
# any new data
initialData$ORIGIN_AIRPORT_SEQ_ID <- NULL
initialData$DEST_AIRPORT_SEQ_ID <- NULL

# UNIQUE_CARRIER and CARRIER also look related, actually they look like identical
# We can see if the are identical by filtering the rows to those we they are different.
# R makes this easy, no loops to write.  All iteration is done for us.
mismatched <- initialData[initialData$CARRIER != initialData$UNIQUE_CARRIER,]
nrow(mismatched)
# 0 mismatched, so UNIQUE_CARRIER and CARRIER identical.  So let's rid of the UNIQUE_CARRIER column
initialData$UNIQUE_CARRIER <- NULL
# let's see what initialData looks like
head(initialData,2)

# To do both these we filter rows as shown below.
onTimeData <- initialData[!is.na(initialData$ARR_DEL15) & initialData$ARR_DEL15!="" & !is.na(initialData$DEP_DEL15) & initialData$DEP_DEL15!="",]
# Let's compare the number of rows in the new and old dataframes
nrow(initialData)
nrow(onTimeData)

# Changing the format of a column and all of the data for the row in that column is hard in some languages 
# but simple in R.  
# We just type in a simple command
onTimeData$DISTANCE <- as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED <- as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED <- as.integer(onTimeData$DIVERTED)

#   Let's take the Arrival departure and delay fields.  Sometime algorithm perform better 
# when you change the fields into factors which are like enumeration values in other languages
# This allows the algorithm to use count of when a value is a discrete value. 
onTimeData$ARR_DEL15 <- as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15 <-as.factor(onTimeData$DEP_DEL15)

# Let also change some other columns factors
onTimeData$DEST_AIRPORT_ID <- as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID <- as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK <- as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST <- as.factor(onTimeData$DEST)
onTimeData$ORIGIN <- as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK <- as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$CARRIER <- as.factor(onTimeData$CARRIER)

# let see how many arrival delayed vs non delayed flights.  We use tapply to 
# see how many time ARR_DEL15 is TRUE, and how many times it is FALSE
tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15, length)
# We should check how many departure delayed vs non delayed flights
#tapply(onTimeData$DEP_DEL15, onTimeData$DEP_DEL15, length)

install.packages("caret")
library(caret)
set.seed(122515)
featureCols <- c("ARR_DEL15", "DAY_OF_WEEK", "CARRIER", "DEST","ORIGIN","DEP_TIME_BLK")

# created filtered version of onTimeData dataframe
onTimeDataFiltered <- onTimeData[,featureCols]
# create vector contain row indicies to put into the training data frames
inTrainRows <- createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.70, list=FALSE)
# check the row IDs
head(inTrainRows,10)
# Create the training data frame
trainDataFiltered <- onTimeDataFiltered[inTrainRows,]
# Create the testing data frame.  Notice the prefix "-" 
testDataFiltered <- onTimeDataFiltered[-inTrainRows,]

# Check split 
#   Should be 70%
nrow(trainDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))
#   Should be 30%
nrow(testDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))


# Create a train prediction model
install.packages('e1071')
library(e1071)

#  Logistic Regression
logisticRegModel <- train(ARR_DEL15 ~ ., data=trainDataFiltered, method="glm", family="binomial",trControl=trainControl(method="cv", number=10, repeats=10))
# Output model
logisticRegModel

logRegPrediction <- predict(logisticRegModel,testDataFiltered)
logRefConfMat <- confusionMatrix(logRegPrediction,testDataFiltered[,"ARR_DEL15"])
logRefConfMat

install.packages("randomForest")
library(randomForest)

rfModel <- randomForest(trainDataFiltered[-1], trainDataFiltered$ARR_DEL15,proximity = TRUE, importance = TRUE)
rfValidation <- predict(rfModel, testDataFiltered)
rfConfMat <- confusionMatrix(rfValidation, testDataFiltered[,"ARR_DEL15"])
rfConfMat
