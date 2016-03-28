library(stringr) # str_split_fixed()


HealthyRideRentals.2015.Q3 <- read.csv("HealthyRideRentals 2015 Q3.csv", stringsAsFactors = FALSE)
HealthyRideRentals.2015.Q4 <- read.csv("HealthyRideRentals 2015 Q4.csv", stringsAsFactors = FALSE)

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

training <- rental_total[which(rental_total$StartDay < 20),]
testing <- rental_total[-which(rental_total$StartDay < 20),]

#' ------------------------------------------------
#'                  Progress
#' ------------------------------------------------
# density plot based on StartHour
d <- density(training$StartHour)
plot(d, main="Pitt Bike Trips by Hour of Day")
polygon(d, col="blue", border="red")

stn_data = read.csv("HealthyRideStations2015.csv")

#' Extract Rental Stop Data
rental_total_stop <- rental_total[, c(1,2,3,4,5,6,12,13,15)]

#' Merge above with Station Data - Left outer join on "ToStationID"
rentalstop_final <- merge(rental_total_stop, stn_data, by.x = "ToStationId", by.y = "StationNum", all.x = TRUE)

#' Create data frame with group and "Count" variable
rentalstop_counts <- data.frame(table(rentalstop_final$ToStationId,rentalstop_final$UserType, rentalstop_final$StopMonth,rentalstop_final$StopDay,rentalstop_final$StopHour ))
head(rentalstop_counts)
colnames(rentalstop_counts) <- c("ToStationId","UserType","StopMonth","StopDay","StopHour","Count")

#' Maximum vehicles returned on a given month, day and hour at a particular station
rentalstop_counts[which.max(rentalstop_counts$Count),]

#' Extract Rental Start Data
rental_total_start <- rental_total[, c(1,2,3,4,5,6,7,8,10)]

#' Merge above with Station Data - Left outer join on "FromStationID"
rentalstart_final <- merge(rental_total_start, stn_data, by.x = "FromStationId", by.y = "StationNum", all.x = TRUE)

#' Create data frame with group and "Count" variable
rentalstart_counts <- data.frame(table(rentalstart_final$FromStationId,rentalstart_final$UserType, rentalstart_final$StartMonth,rentalstart_final$StartDay,rentalstart_final$StartHour ))
head(rentalstart_counts)
colnames(rentalstart_counts) <- c("FromStationId","UserType","StartMonth","StartDay","StartHour","Count")

#' Maximum vehicles returned on a given month, day and hour at a particular station
rentalstart_counts[which.max(rentalstart_counts$Count),]