library(stringr) # str_split_fixed()


HealthyRideRentals.2015.Q3 <- read.csv("/Users/zhugejunwei/PittBikeShare/HealthyRideTripData2015Q3/HealthyRideRentals 2015 Q3.csv", stringsAsFactors = FALSE)
HealthyRideRentals.2015.Q4 <- read.csv("/Users/zhugejunwei/PittBikeShare/HealthyRideTripData2015Q4/HealthyRideRentals 2015 Q4.csv", stringsAsFactors = FALSE)

#' ------------------------------------------------
#'                  Preprocessing
#' ------------------------------------------------
# combine two dataframe into one
rental_total <- rbind(HealthyRideRentals.2015.Q3,HealthyRideRentals.2015.Q4)

# delete StationName attributes, only using StationID to do prediction
rental_total <- rental_total[, c(1,2,3,4,5,6,8,10)]

# delete rows with missing values
rental_total$UserType[which(rental_total$UserType == "")] <- NA
rental_total <- rental_total[complete.cases(rental_total), ]
str(rental_total)
rental_total$StartTime <- as.character(rental_total$StartTime)
rental_total$StopTime <- as.character(rental_total$StopTime)
rental_total$UserType <- as.character(rental_total$UserType)

# Split StartTime and StopTime into Day, Month, Year, Hour, and Minute 
##' StartTime
tag <- as.data.frame(str_split_fixed(rental_total$StartTime, "/", 3))
rental_total$StartMonth <- tag$V1
rental_total$StartDay <- tag$V2
tag2 <- as.data.frame(str_split_fixed(tag$V3, " ", 2))
rental_total$StartYear <- tag2$V1
tag3 <- as.data.frame(str_split_fixed(tag2$V2, ":", 2))
rental_total$StartHour <- tag3$V1
rental_total$StartMinute <- tag3$V2
rental_total <- rental_total[, - which(colnames(rental_total) == 'StartTime')]

##' StopTime
tag.stop <- as.data.frame(str_split_fixed(rental_total$StopTime, "/", 3))
rental_total$StopMonth <- tag.stop$V1
rental_total$StopDay <- tag.stop$V2
tag.stop2 <- as.data.frame(str_split_fixed(tag.stop$V3, " ", 2))
rental_total$StopYear <- tag.stop2$V1
tag.stop3 <- as.data.frame(str_split_fixed(tag.stop2$V2, ":", 2))
rental_total$StopHour <- tag.stop3$V1
rental_total$StopMinute <- tag.stop3$V2
rental_total <- rental_total[, - which(colnames(rental_total) == 'StopTime')]

# convert training$UserType from factor into numeric
rental_total$UserType <- as.factor(rental_total$UserType)
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}
rental_total$UserType <- as.numeric.factor(rental_total$UserType)

# Split into training and testing set
rental_total$StartDay <- as.numeric(levels(rental_total$StartDay))[rental_total$StartDay]
rental_total$StartMonth <- as.numeric(levels(rental_total$StartMonth))[rental_total$StartMonth]
rental_total$StartYear <- as.numeric(levels(rental_total$StartYear))[rental_total$StartYear]
rental_total$StartHour <- as.numeric(levels(rental_total$StartHour))[rental_total$StartHour]
rental_total$StartMinute <- as.numeric(levels(rental_total$StartMinute))[rental_total$StartMinute]

rental_total$StopDay <- as.numeric(levels(rental_total$StopDay))[rental_total$StopDay]
rental_total$StopMonth <- as.numeric(levels(rental_total$StopMonth))[rental_total$StopMonth]
rental_total$StopYear <- as.numeric(levels(rental_total$StopYear))[rental_total$StopYear]
rental_total$StopHour <- as.numeric(levels(rental_total$StopHour))[rental_total$StopHour]
rental_total$StopMinute <- as.numeric(levels(rental_total$StopMinute))[rental_total$StopMinute]

#' ------------------------------------------------
#'                  Progress
#' ------------------------------------------------
# density plot based on StartHour
d <- density(training$StartHour)
plot(d, main="Pitt Bike Trips by Hour of Day")
polygon(d, col="blue", border="red")

stn_data = read.csv("/Users/zhugejunwei/PittBikeShare/HealthyRideTripData2015Q4/HealthyRideStations2015.csv")

#' Extract Rental Stop Data
rental_total_stop <- rental_total[, c(5,6,12,13,15)]
head(rental_total_stop)
#' Merge above with Station Data - Left outer join on "ToStationID"
rentalstop_final <- merge(rental_total_stop, stn_data, by.x = "ToStationId", by.y = "StationNum", all.x = TRUE)
head(rentalstop_final)
rentalstop_final <- rentalstop_final[, c(1,2,3,4,5,7)]
head(rentalstop_final)
#' Create data frame with group and "Count" variable
rentalstop_counts <- data.frame(table(rentalstop_final$ToStationId,rentalstop_final$UserType, rentalstop_final$StopMonth,rentalstop_final$StopDay,rentalstop_final$StopHour, rentalstop_final$RackQnty))
head(rentalstop_counts)
colnames(rentalstop_counts) <- c("ToStationId","StopUserType","StopMonth","StopDay","StopHour","StopRackQnty","StopCount")
head(rentalstop_counts)

#' Maximum vehicles returned on a given month, day and hour at a particular station
rentalstop_counts[which.max(rentalstop_counts$StopCount),]

#' Extract Rental Start Data
rental_total_start <- rental_total[, c(1,2,3,4,6,7,8,10)]

#' Merge above with Station Data - Left outer join on "FromStationID"
rentalstart_final <- merge(rental_total_start, stn_data, by.x = "FromStationId", by.y = "StationNum", all.x = TRUE)

#' Create data frame with group and "Count" variable
rentalstart_counts <- data.frame(table(rentalstart_final$FromStationId,rentalstart_final$UserType, rentalstart_final$StartMonth,rentalstart_final$StartDay,rentalstart_final$StartHour,rentalstart_final$RackQnty ))
head(rentalstart_counts)
colnames(rentalstart_counts) <- c("FromStationId","StartUserType","StartMonth","StartDay","StartHour","StartRackQnty","StartCount")
head(rentalstart_counts)
#' Maximum vehicles returned on a given month, day and hour at a particular station
rentalstart_counts[which.max(rentalstart_counts$StartCount),]

#' merge two count data set into one
start_row_to_keep = which(rentalstart_counts$StartCount > 0) 
rentalstart_counts <- rentalstart_counts[start_row_to_keep,]
head(rentalstart_counts)
stop_row_to_keep = which(rentalstop_counts$StopCount > 0) 
rentalstop_counts <- rentalstop_counts[stop_row_to_keep,]
head(rentalstop_counts)
final_total <- merge(rentalstart_counts,rentalstop_counts, by.x = "FromStationId", by.y = "ToStationId")
str(final_total)
final_total$StartMonth <- as.numeric(final_total$StartMonth)
final_total$StopMonth <- as.numeric(final_total$StopMonth)
## Hour_row_to_keep <- which(final_total$StartHour == final_total$StopHour) 
Month_row_to_keep <- which(final_total$StartMonth == final_total$StopMonth) 
final_total.2 <- final_total[Month_row_to_keep,]
Day_row_to_keep <- which(final_total.2$StartDay == final_total.2$StopDay) 
final_total.2 <- final_total.2[Day_row_to_keep,]
## final_total.2 <- final_total[Hour_row_to_keep,]
head(final_total.2)
final_total.2 <- final_total.2[,c(1,2,3,4,5,6,7,8,11,13)]
head(final_total.2)
#' the count of in and out on the same day. "+":in; "-":out.
final_total.2$Count <- final_total.2$StopCount - final_total.2$StartCount
head(final_total.2)
final_total.2 <- final_total.2[,-c(7,10)]

final_total.2$StartDay <- as.numeric(final_total.2$StartDay)
final_total.2$FromStationId <- as.numeric(final_total.2$FromStationId)
final_total.2$StartUserType <- as.numeric(final_total.2$StartUserType)
final_total.2$StartHour <- as.numeric(final_total.2$StartHour)
final_total.2$StartRackQnty <- as.numeric(final_total.2$StartRackQnty)
final_total.2$StopUserType <- as.numeric(final_total.2$StopUserType)
final_total.2$StopHour <- as.numeric(final_total.2$StopHour)

y = final_total.2$Count
final_total.2 = cbind(y=y, final_total.2)
final_total.2$Count = NULL
str(final_total.2)

training <- final_total.2[which(final_total.2$StartDay < 20),]
testing <- final_total.2[-which(final_total.2$StartDay < 20),]

#' ------------------------------------------------
#'             Training Models
#' ------------------------------------------------
library(MASS) # for the example dataset
library(plyr) # for recoding data
library(ROCR) # for plotting roc
library(e1071) # for NB and SVM
library(rpart) # for decision tree
library(ada) # for adaboost
library(class) # for knn
# For knn function, we need to standardize values so that they have same influence on the result
for (i in 1:8)
  training[,i] = (training[,i]-min(training[,i]))/(max(training[,i])-min(training[,i]))
training[1:3,]

for (i in 1:8)
  testing[,i] = (testing[,i]-min(testing[,i]))/(max(testing[,i])-min(testing[,i]))
testing[1:3,]


set.seed(12345) 
do.classification <- function(train.set, test.set, 
                              cl.name, verbose=F) {
  ## note: to plot ROC later, we want the raw probabilities,
  ## not binary decisions
  switch(cl.name,
         knn1 = { # here we test k=1; you should evaluate different k's
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 1, prob=T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] #modified
           prob = attr(prob,"prob")
           prob
         },
         knn3 = { # here we test k=3; you should evaluate different k's
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 3, prob=T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] #modified
           prob = attr(prob,"prob")
           prob
         },
         knn5 = { # here we test k=5; you should evaluate different k's
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 5, prob=T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] #modified
           prob = attr(prob,"prob")
           prob
         },
         knn10 = { # here we test k=5; you should evaluate different k's
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 10, prob=T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] #modified
           prob = attr(prob,"prob")
           prob
         },
         lr = { # logistic regression
           model = glm(y~., family=binomial, data=train.set)
           if (verbose) {
             print(summary(model))             
           }
           prob = predict(model, newdata=test.set, type="response") 
           prob
         },
         nb = { # naiveBayes
           model = naiveBayes(y~., data=train.set)
           prob = predict(model, newdata=test.set, type="raw") 
           prob = prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         dtree = {
           model = rpart(y~., data=train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }           
           prob = predict(model, newdata=test.set)
           if (0) { # here we use the default tree, 
             ## you should evaluate different size of tree
             ## prune the tree 
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set)
             ## plot the pruned tree 
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)             
           }
           #print(cbind(prob,as.character(test.set$y)))
           
           prob = prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         dtreeprune = {
           model = rpart(y~., data=train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }           
           prob = predict(model, newdata=test.set)
           
           if (1) { # here we prune the tree
             ## prune the tree 
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set)
             ## plot the pruned tree 
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)             
           }
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         svm = {
           model = svm(y~., data=train.set, probability=T)
           if (0) { # fine-tune the model with different kernel and parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set, 
                               kernel="radial", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           prob = predict(model, newdata=test.set, probability=T)
           #print(prob)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svm1 = {
           model = svm(y~., data=train.set, probability=T)
           if (1) { # kernel = radial here we use parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set, 
                               kernel="radial", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svm2 = {
           model = svm(y~., data=train.set, probability=T)
           if (1) { # kernel = sigmoid   here we use parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set, 
                               kernel="sigmoid", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svm3 = {
           model = svm(y~., data=train.set, probability=T)
           if (1) {    #kernel = polynomial   here we use parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set, 
                               kernel="polynomial", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         }, 
         ada = {
           model = ada(y~., data = train.set)
           prob = predict(model, newdata=test.set, type='probs')
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob)
           prob
         }
  ) 
}

pre.test <- function(train.set, test.set, cl.name, r=0.6, prob.cutoff=0.5) {
  cat('pre-test',cl.name,':',
      '#training:',nrow(train.set),
      '#testing',nrow(test.set),'\n')
  prob = do.classification(train.set, test.set, cl.name)
  
  predicted = as.numeric(prob > prob.cutoff)
  actual = test.set$y
  confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
  error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
  cat('error rate:',error,'\n')
}

my.classifier <- function(train.set, test.set, cl.name, do.cv, get.performance=F) {
  if (do.cv) k.fold.cv(train.set, test.set, cl.name, get.performance=get.performance)
}

k.fold.cv <- function(train.set, test.set, cl.name, k.fold=10, prob.cutoff=0.5, get.performance=F) {
  errors = dim(k.fold)
  precisions = dim(k.fold)
  recalls = dim(k.fold)
  fscores = dim(k.fold)
  accuracies = dim(k.fold)
  probs = NULL
  actuals = NULL
  train.set = train.set
  test.set = test.set
  cat(k.fold,'-fold CV run',cl.name,':',
      '#training:',nrow(train.set),
      '#testing',nrow(test.set),'\n')
  prob = do.classification(train.set, test.set, cl.name)
  predicted = as.numeric(prob > prob.cutoff)
  actual = test.set$y
  confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
  confusion.matrix
  error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
  #    errors[k] = error
  cat('\t\terror=',error,'\n')
  precision = (confusion.matrix[1,1]/(confusion.matrix[1,1]+confusion.matrix[2,1]))
  #    precisions[k] = precision
  print(confusion.matrix)
  recall =(confusion.matrix[1,1]/(confusion.matrix[1,1]+confusion.matrix[1,2]))
  #    recalls[k] = recall
  fscore = 2*precision*recall/(precision+recall)
  #    fscores[k] = fscore
  probs = c(probs,prob)
  actuals = c(actuals,actual)
  ## you may compute other measures and store them in arrays
  #  }
  avg.error = error
  cat('avg error=',avg.error,'\n')
  avg.accuracy = 1 - avg.error
  cat('avg Accuracy=',avg.accuracy,'\n')
  avg.precision = precision
  cat('avg Precision=',avg.precision,'\n')
  avg.recall = recall
  cat('avg recall=',avg.recall,'\n')
  avg.fscore = fscore
  cat('avg fscore=',avg.fscore,'\n')
}

results <- cbind(my.classifier(training,testing, cl.name='lr',do.cv=T),
                 my.classifier(training,testing, cl.name='knn1',do.cv=T),
                 my.classifier(training,testing, cl.name='knn5',do.cv=T),
                 my.classifier(training,testing, cl.name='knn10',do.cv=T),
                 my.classifier(training,testing, cl.name='nb',do.cv=T),
                 #my.classifier(training,testing, cl.name='dtree',do.cv=T),
                 #my.classifier(training,testing, cl.name='dtreeprune',do.cv=T),
                 #my.classifier(training,testing, cl.name='svm1',do.cv=T),
                 #my.classifier(training,testing, cl.name='svm2',do.cv=T),
                 my.classifier(training,testing, cl.name='ada',do.cv=T)
)

colnames(results) <- c("Logistic","KNN1","KNN5","KNN10","Naive Bayes","sigmoid SVM","ada")
results

# Create bar charts
colours <- c("red", "orange", "yellow", "green", "blue", "purple","darkgreen")

fs <- as.matrix(results[5,])
auc <- as.matrix(results[6,])

barplot(fs,main="Bar Chart for F-score",
        names.arg=c("Logistic","KNN1","KNN5","KNN10","Naive Bayes","Decision Tree","Pruned Decision Tree","sigmoid SVM","ada"),
        cex.lab = 1.5, cex.main = 1.4,cex.names=0.6,
        beside=TRUE, col=colours)


barplot(auc,main="Bar Chart for AUC",
        names.arg=c("Logistic","KNN1","KNN5","KNN10","Naive Bayes","Decision Tree","Pruned Decision Tree","sigmoid SVM","ada"),
        cex.lab = 1.5, cex.main = 1.4,cex.names=0.6,
        beside=TRUE, col=colours)

