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
orig = read.csv2('/dev/repos/R/ml_pluralsightcourse/data/774523976_T_ONTIME_REPORTING.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)

# nrow(orig)
airports <- c('ATL', 'LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')
orig <- subset(orig, DEST %in% airports & ORIGIN %in% airports)
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

# checking data how many times arrdelay is true and false
tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15, length)

# compute the 
(6460/(25664+6460))



