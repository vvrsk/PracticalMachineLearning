library(caret)
library(corrgram)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
lifts.train <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
lifts.test <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
print(dim(lifts.train))
print(dim(lifts.test))

# Exclude aggregated rows from the training set (rows with new_window = yes)
lifts.train = lifts.train[lifts.train$new_window == 'no', ]

# Columns to be removed: metadata columns ('X' refers to the first unnamed column)
md_columns = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2',
               'cvtd_timestamp', 'new_window', 'num_window')
lifts.train = lifts.train[ , !(names(lifts.train) %in% md_columns)]

# Columns to be removed: no values in the the testing data set
lifts.test.nona <- lifts.test[ , colSums(is.na(lifts.test)) < nrow(lifts.test) ]
names.nona.variables <- names(lifts.test.nona)
names.nona.variables <- names.nona.variables[(names.nona.variables %in% names(lifts.train))]
names.nona <- append(names.nona.variables, 'classe')

lifts.train = lifts.train[names.nona]

 lifts.train.sample = lifts.train[sample(nrow(lifts.train), 500), ]
 corrgram(lifts.train.sample, order = TRUE, lower.panel=panel.pie)

# Big multi-scatter plot into a PDF
pdf("multi-scatter", 50, 50)
pairs(lifts.train.sample, pch = 21, bg = c("red", "green3", "blue", "yellow", "green")[unclass(lifts.train.sample$classe)])
dev.off()

fitRfCv <- train(classe ~ ., data = lifts.train, ntree = 100, method = 'rf', trControl = trainControl(method = "cv", number = 5))
# Default threshold 95% variance
fitRfPcaCv <- train(classe ~ ., data = lifts.train, ntree = 100, method = 'rf', preProcess = "pca", trControl = trainControl(method = "cv", number = 5))

lifts.prediction = predict(fitRfCv, lifts.test)

print(lifts.prediction)