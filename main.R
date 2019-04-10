#getting tiny data??
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time


# data rule 1
# closer the data is to what you are predicting, the better
# data rule 2
# data will never be in the format you need
# data rule 3
# accurately predicting rare events is difficult
# data rule 4
# track how you manipulate data



# orig <- read.csv2('data/774523976_T_ONTIME_REPORTING.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
#orig = read.csv2('/dev/repos/R/ml_pluralsightcourse/data/774523976_T_ONTIME_REPORTING.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
#complete data 
orig = read.csv2('/dev/repos/R/ml_pluralsightcourse/data/complete_data_w_wheater_column.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)

nrow(orig)

#filtering airports
airports <- c('ATL', 'LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')

airports

orig <- subset(orig, DEST %in% airports & ORIGIN %in% airports)

orig

orig$X = NULL 

tail(orig, 2)

#checking correlation
cor(orig[c('DEST_AIRPORT_SEQ_ID', 'DEST_AIRPORT_ID')])

#delete useless DATA  
orig$DEST_AIRPORT_SEQ_ID <- NULL
orig$ORIGIN_AIRPORT_SEQ_ID <-NULL

#checking correlation with strings.. 
# mismatched<- orig[orig$carrier!= orig$OP_UNIQUE_CARRIER,]

#molding data
# checking for data  if the is in the right format or NAN
onTimeData= orig[!is.na(orig$ARR_DEL15) & orig$ARR_DEL15!="" & !is.na(orig$DEP_DEL15) & orig$DEP_DEL15!="",]
onTimeData
# checking for differences on the original an the filtered
nrow(orig)
nrow(ontime)

# changing the format of the values
onTimeData$DISTANCE = as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED = as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED = as.integer(onTimeData$DIVERTED)

# changing values to factors
onTimeData$ARR_DEL15 = as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15 = as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID = as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID = as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK = as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$dest = as.factor(onTimeData$DEST)
onTimeData$DEST = as.factor(onTimeData$DEST)
onTimeData$ORIGIN = as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK = as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$OP_UNIQUE_CARRIER = as.factor(onTimeData$OP_UNIQUE_CARRIER)

onTimeData

# checking data how many times arrdelay is true and false
tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15, length)

# compute the 
(6460/(25664+6460))

# factor and decision criteria for algorithm selection could vary
# the course uses this factors:
#   learning type -> based on statemnt => prediction model => supervised machine learning
#   result - regresion (continuos values)  clasification(descrete values) => classification => delay or not
#   complexity  -> then keep it simple -> eliminate enemble algorithm
#   basic vs enhanced -> basic(since it is our first approach)-> choose between Naive Bayes, Logistic regression, decision tree

# logistic regression algorthm
# training process -> retrain if needed

#training process ->(select)  with the minimun features (columns) include the column to be predicted
#install caret package

#set the seed to able the same starting point each time
set.seed(122515)

#setting the feature columns
featureCols = c("ARR_DEL15","DAY_OF_WEEK", "OP_CARRIER_AIRLINE_ID", "DEST", "ORIGIN", "DEP_TIME_BLK")

#create a set only with those colums
onTimeDataFiltered = onTimeData[,featureCols]
onTimeDataFiltered

#spliting data => ensure that arr_del15(the column that we try to predict) ratio are the same on trining and testing
#list = false is one item per row
inTrainRows = createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.7, list = FALSE)
inTrainRows
#checks the rows
head(inTrainRows, 10)

#select the training data based on the intrarow vector -> select the 70% of the data
trainDataFiltered = onTimeDataFiltered[inTrainRows,]

#select the training data based on the intrarow vector -> select the 30% of the data (put "-")
testDataFiltered = onTimeDataFiltered[-inTrainRows,]

#before apply the algorith verify the proportion if it is accurate
nrow(trainDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))
#now for testing
nrow(testDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))

#NOW TRAIN THE MODEL
# in case of error run 
install.packages('e1071', dependencies=TRUE)
#ARR_DEL15 ~ . = all columns except the one on the left of ~ are used to predict the value
logisticRegModel = train(ARR_DEL15 ~ ., data = trainDataFiltered, method="glm", family="binomial")

#now predict using the test set
logRegPrediction = predict(logisticRegModel, testDataFiltered)
logRegPrediction
#using confusion matrix function to evaluate how well the model predicts flyght delays
logRegConfMat = confusionMatrix(logRegPrediction, testDataFiltered[,"ARR_DEL15"])

#see result
logRegConfMat

#need to improve performance
#options.. adding columns or adjust training setting or select better algorithm

#load random forest
#trainDataFiltered[-1] exclude the colum from the data, 
#trainDataFiltered$ARR_DEL15 name of the data we try to obtain
rfModel = randomForest(trainDataFiltered[-1], trainDataFiltered$ARR_DEL15, proximity = TRUE, importance = TRUE)

#now using the model to predict with test
rfValidation = predict(rfModel, testDataFiltered)

#using confision matrix function to evaluate how well the model predicts flyght delays
rfConfMat = confusionMatrix(rfValidation, testDataFiltered[,"ARR_DEL15"])

rfConfMat

#new performance.. adjust, better algorithm rethink the problem
#adding weather data ... and go to filtering and start all over again...




