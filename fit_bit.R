library(dplyr)
library(datasets)
library(ggplot2)
##################################################################
# Read in the data
##################################################################
#read the data
data <- read.csv("activity/activity.csv", 
                 colClasses=c("integer","Date","integer"))

#create a data frame with the na's removed and grouped by date
data_tbl <- tbl_df(data)
data_no_na_tbl <- filter(data_tbl, !is.na(steps)) #remove na

no_na_date <- group_by(data_no_na_tbl, date)        #group by date
steps_per_day <- summarize(no_na_date, sum(steps))  #calc sum
colnames(steps_per_day) <- c("date","total")


#histogram of total number of steps per day
hist(steps_per_day$total, xlab = "total number of steps per day", 
     labels = TRUE, ylim = c(0,40), main = "Number of Daily Steps",
     col = "BLUE")


#mean/median total number of steps taken per day.
results <- summarize(steps_per_day, mean = mean(total), median = median(total))


#average daily activity pattern
no_na_interval <- group_by(data_no_na_tbl, interval)
avg_steps_per_interval <- summarize(no_na_interval, mean = mean(steps))

qplot(interval,mean,data = avg_steps_per_interval,geom = "line",
      main = "Average Daily Activity Pattern", 
      ylab = "Mean number of Steps", 
      xlab = "Interval" )

#interval with max avg steps
max_index <- which.max(avg_steps_per_interval$mean)         
max_interval <- avg_steps_per_interval[max_index,]

############################################################################################
# Missing Values - lots of NA in steps column could skew data
############################################################################################


#copy original data set
calc_data <- data_tbl

# get a list of indices of NA rows 
missing_rows<- which(is.na(calc_data$steps))

#number of missing values in the dataset.
num_na_rows <- length(missing_rows)

#calculate the overall average number of steps per interval
total_avg <- summarize(avg_steps_per_interval, avg = mean(mean))



# loop through indices and replace the na value with the rounded mean
# for that interval
for(i in 1:num_na_rows) {
      
    #determine which row in avg_steps_per_interval corresponds to  
    #the na row
    index <- which( avg_steps_per_interval$interval == 
                    calc_data[ missing_rows[i],]$interval)
              
              
    #double check that an average exists for this interval and replace na 
    #with average for the interval
    if (index > 0)
    {
        calc_data[missing_rows[i],]$steps <- round(avg_steps_per_interval[index,]$mean)
    }
    #it is possible avg for interval doesn't exist (for na for that interval 
    #for all days - use overall mean
    else
    {
        calc_data[missing_rows[i],]$steps <- total_avg$avg
    }
}

#histogram of total number of steps per day
calc_data_tbl <- tbl_df(calc_data)
calc_date <- group_by(calc_data_tbl, date)          #group by date
steps_per_day <- summarize(calc_date, sum(steps))   #calc sum
colnames(steps_per_day) <- c("date","total")

results_calc <- summarize(steps_per_day, 
                          mean = mean(total), 
                          median = median(total))

hist(steps_per_day$total, 
     xlab = "total number of steps per day", 
     labels = TRUE, 
     ylim = c(0,40), 
     main = "Number of Daily Steps (missing values estimated)",
     col = "green")


##########################################################################
# Weekday vs Weekend
##########################################################################
calc_data_day <- mutate(calc_data_tbl, 
       day_type = ifelse ((weekdays(as.Date(date)) %in% c('Saturday','Sunday')),
                         'weekend','weekday'))

#average daily activity pattern - split by day type
calc_data_group <- group_by(calc_data_day, day_type, interval )
avg_steps <- summarize(calc_data_group, mean = mean(steps))

qplot(interval, mean, data = avg_steps, geom = "line", 
      ylab = "Number of Steps", xlab = "Interval", 
      main = "Average Number of Steps on the Weekend and Weekdays",
      facets = day_type ~.)

