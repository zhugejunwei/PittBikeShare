library(stringr) # str_split_fixed()


HealthyRideRentals.2015.Q3 <- read.csv("/Users/zhugejunwei/PittBikeShare/HealthyRideTripData2015Q3/HealthyRideRentals 2015 Q3.csv", stringsAsFactors = FALSE)
HealthyRideRentals.2015.Q4 <- read.csv("/Users/zhugejunwei/PittBikeShare/HealthyRideTripData2015Q4/HealthyRideRentals 2015 Q4.csv", stringsAsFactors = FALSE)

#' ------------------------------------------------
#'                  Preprocessing - 1
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
#'                  Preprocessing - 2
#' ------------------------------------------------
# density plot based on StartHour
d.hour <- density(rental_total$StartHour)
plot(d.hour, main="Pitt Bike Trips by Hour of Day")
polygon(d.hour, col="blue", border="red")
##' according to the "Pitt Bike Trips by Hour of Day" plot, the fastigium of Pitt bike trips is between 8 - 20.

# density plot based on StartDay
d.day <- density(rental_total$StartDay)
plot(d.day, main="Pitt Bike Trips by Day of a Day")
polygon(d.day, col="blue", border="red")
##' According to the "Pitt Bike Trips by Day of a Month" plot, we will not take acount of "Day" element, instead,
##' we will calculate weekdays and weekends from Month and Day, and take weekdays and weekends as variables. 

# density plot based on StartMonth
d.month <- density(rental_total$StartMonth)
plot(d.month, main="Pitt Bike Trips by Month")
polygon(d.month, col="blue", border="red")
##' according to the "Pitt Bike Trips by Month" plot, July and August have the most bike trips,
##'  and the bike trip amount declines month by month till Decemenber. 

# density plot based on UserType
d.type <- density(rental_total$UserType)
plot(d.type, main="Pitt Bike Trips by UserType")
polygon(d.type, col="blue", border="red")
##' usertype 1 is the most, 3 second, 2 the least. 
##' 1 - Member (pay as-you-go customer) 
##' 2 - Subscriber ( deluxe and standard monthly member customer)
##' 3 - Daily (24-hour pass customer)

##' In our final table, the variables will be StationID, UserType, Month, Weekday,
##'  RackQnty, and the "y" will be the bike trip count of a specific station at a day, values with 
##'  "-" means out, "+" means in. So we can compute the final bike amount of a station at one day.
##'  So we can dicide the rebalancing problem based on the amount and the "density plot based on Hour".

stn_data = read.csv("/Users/zhugejunwei/PittBikeShare/HealthyRideTripData2015Q4/HealthyRideStations2015.csv")

#' ----------------------------------------------------------------------
#' Edit-Start: add a Weekday attribute, delete Day attribute
#' ----------------------------------------------------------------------
library(base)
week.Q3 <- HealthyRideRentals.2015.Q3[,c(1,2,3)]
week.Q4 <- HealthyRideRentals.2015.Q4[,c(1,2,3)]
week.merge <- rbind(week.Q3,week.Q4)
# StartWeekday
tag.week.start <- as.data.frame(str_split_fixed(week.merge$StartTime, " ", 2))
week.merge$Startdate <- gsub("/", "-", tag.week.start$V1)
tag.week.start2 <- as.data.frame(str_split_fixed(week.merge$Startdate, "-", 3))
library(stringr)
tag.week.start2$V1 <- str_pad(tag.week.start2$V1, 2, pad = "0")
week.merge$Startmonth <- tag.week.start2$V1
tag.week.start2$V2 <- str_pad(tag.week.start2$V1, 2, pad = "0")
week.merge$Startday <- tag.week.start2$V2
week.merge$Startyear <- tag.week.start2$V3
week.merge$Startdate <- paste(week.merge$Startmonth, "-", week.merge$Startday, "-", week.merge$Startyear)
week.merge$Startdate <- gsub(" ", "", week.merge$Startdate)
week.merge$Weekday <- weekdays(as.Date(week.merge$Startdate))
# StopWeekday
tag.week.Stop <- as.data.frame(str_split_fixed(week.merge$StopTime, " ", 2))
week.merge$Stopdate <- gsub("/", "-", tag.week.Stop$V1)
tag.week.Stop2 <- as.data.frame(str_split_fixed(week.merge$Stopdate, "-", 3))
library(stringr)
tag.week.Stop2$V1 <- str_pad(tag.week.Stop2$V1, 2, pad = "0")
week.merge$Stopmonth <- tag.week.Stop2$V1
tag.week.Stop2$V2 <- str_pad(tag.week.Stop2$V1, 2, pad = "0")
week.merge$Stopday <- tag.week.Stop2$V2
week.merge$Stopdate <- paste(week.merge$Stopmonth, "-", week.merge$Stopday, "-", week.merge$Startyear)
week.merge$Stopdate <- gsub(" ", "", week.merge$Stopdate)
week.merge$Weekday2 <- weekdays(as.Date(week.merge$Stopdate))
week.merge2 <- week.merge[, c(1,8,12)]
##' Weekday - StartWeek
##' Weekday2 - StopWeek
# merge week.merge2 and rental_total
rental_total <- merge(rental_total, week.merge2, by.x = "TripId", by.y = "TripId")
#' ----------------------------------------------------------------------
#' Edit-End: add a Weekday attribute, delete Day attribute
#' ----------------------------------------------------------------------

 
#' Extract Rental Stop Data
rental_total_stop <- rental_total[, c(5,6,12,13,15,18)]
head(rental_total_stop)
#' Merge above with Station Data - Left outer join on "ToStationID"
rentalstop_final <- merge(rental_total_stop, stn_data, by.x = "ToStationId", by.y = "StationNum", all.x = TRUE)
head(rentalstop_final)
rentalstop_final <- rentalstop_final[, c(1,2,3,4,5,6,8)]
head(rentalstop_final)
#' Create data frame with group and "Count" variable
rentalstop_counts <- data.frame(table(rentalstop_final$ToStationId,rentalstop_final$UserType, rentalstop_final$StopMonth,rentalstop_final$StopDay,rentalstop_final$StopHour, rentalstop_final$Weekday2 ,rentalstop_final$RackQnty))
head(rentalstop_counts)
colnames(rentalstop_counts) <- c("ToStationId","StopUserType","StopMonth","StopDay","StopHour","StopWeekday","StopRackQnty","StopCount")
head(rentalstop_counts)

#' Maximum vehicles returned on a given month, day and hour at a particular station
rentalstop_counts[which.max(rentalstop_counts$StopCount),]

#' Extract Rental Start Data
rental_total_start <- rental_total[, c(1,2,3,4,6,7,8,10,17)]

#' Merge above with Station Data - Left outer join on "FromStationID"
rentalstart_final <- merge(rental_total_start, stn_data, by.x = "FromStationId", by.y = "StationNum", all.x = TRUE)

#' Create data frame with group and "Count" variable
rentalstart_counts <- data.frame(table(rentalstart_final$FromStationId,rentalstart_final$UserType, rentalstart_final$StartMonth,rentalstart_final$StartDay,rentalstart_final$StartHour,rentalstart_final$Weekday,rentalstart_final$RackQnty ))
head(rentalstart_counts)
colnames(rentalstart_counts) <- c("FromStationId","StartUserType","StartMonth","StartDay","StartHour","StartWeekday","StartRackQnty","StartCount")
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
final_total.2 <- final_total.2[,c(1,2,3,4,5,6,7,8,9,12,13,15)] 
head(final_total.2)
#' the count of in and out on the same day. "+":in; "-":out.
final_total.2$Count <- final_total.2$StopCount - final_total.2$StartCount
head(final_total.2)
final_total.2 <- final_total.2[,-c(5,8,10,11,12)]

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
final_total.2$RackQnty <- as.numeric.factor(final_total.2$RackQnty)
#as.numeric.factor(final_total.2$StartDay)
#as.numeric.factor(final_total.2$FromStationId)
#as.numeric.factor(final_total.2$StartUserType)
#as.numeric.factor(final_total.2$StartWeekday.n)
#as.numeric.factor(final_total.2$StopUserType)
final_total.2$StartDay <- as.numeric(final_total.2$StartDay)
final_total.2$FromStationId <- as.numeric(final_total.2$FromStationId)
final_total.2$StartUserType <- as.numeric(final_total.2$StartUserType)
final_total.2$StartWeekday.n <- as.numeric(final_total.2$StartWeekday)
final_total.2$StopUserType <- as.numeric(final_total.2$StopUserType)


final_total.2$StartWeekday = NULL
final_total.2$StopUserType = NULL
##' Tuesday - 5
##' Thurday - 4
##' Friday - 1
##' Sunday - 3
y = final_total.2$Count
final_total.2 = cbind(y=y, final_total.2)
final_total.2$Count = NULL
str(final_total.2)

library(plyr)
names(final_total.2)[names(final_total.2)=="FromStationId"] <- "StationId"
names(final_total.2)[names(final_total.2)=="StartUserType"] <- "UserType"
names(final_total.2)[names(final_total.2)=="StartMonth"] <- "Month"
names(final_total.2)[names(final_total.2)=="StartRackQnty"] <- "RackQnty"
names(final_total.2)[names(final_total.2)=="StartWeekday.n"] <- "Weekdays"
head(final_total.2)
##' Because the StartHour and StopHour of every trip between training and testing set
##'  cannot be exactly the same, so I delete these two column, but I will use "Hour" attribute 
##'  as an important factor to decide what time (A time period in a day) is the best period 
##'  to retransfer the bikes, just as "density plot based on StartHour" showed.

# final_total.2$y <- as.factor(final_total.2$y)

training <- final_total.2[which(final_total.2$StartDay < 20),]
testing <- final_total.2[-which(final_total.2$StartDay < 20),]

training$StartDay <- NULL
testing$StartDay <- NULL
# centering with 'scale()'
training.scale <- training
testing.scale <- testing
for (i in 1:6)
  training.scale[,i] = (training[,i]-min(training[,i]))/(max(training[,i])-min(training[,i]))
training.scale[1:3,]

for (i in 1:6)
  testing.scale[,i] = (testing[,i]-min(testing[,i]))/(max(testing[,i])-min(testing[,i]))
testing.scale[1:3,]


#' --------------------------------------------------------
#'                   ridge regression
#' --------------------------------------------------------
library(caret)
library(elasticnet)
library(MASS)
library(glmnet)

fit1 <- lm.ridge(y ~ ., data = training, lambda = 0.1)
new.df <- data.frame(testing)
result <- as.matrix(new.df) %*% coef(fit1)
library(base)
result <- round(result)

library(Metrics)

actual <- testing$y

size <- length(actual)
true_count = 0
for (i in 1:size)
{
  if(isTRUE(((result[i] >= actual[i]-2) && (result[i]<= actual[i]+2))))
  {
    true_count = true_count + 1
  }
}
accuracy <- true_count/size
accuracy ## 0.7410808


#' --------------------------------------------------------
#'                   polynormial regression
#' --------------------------------------------------------



