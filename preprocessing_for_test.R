library(tidyverse)
library(nycflights13)
library(Hmisc)
library(lubridate)
library(imputeMissings)
library(dplyr)


preprocess_data <- function(filepath) {
  set.seed(42)
  original_data <- read_csv(filepath)
  DF <- original_data
  DF[sapply(DF, is.character)] <-
    lapply(DF[sapply(DF, is.character)],
           as.factor)
  DF$flight <- as.factor(DF$flight)
  DF$sched_arr_time_posix <-
    as.POSIXct(str_pad(as.character(DF$sched_arr_time), 4, pad = "0"), format =
                 "%H%M")
  DF$sched_arr_time_hour <- hour(DF$sched_arr_time_posix)
  DF$sched_arr_time_minute <- minute(DF$sched_arr_time_posix)
  
  #num minute is number of minutes since start of day for scheduled arrival time
  DF$sched_arr_time_num_minute <-
    60 * DF$sched_arr_time_hour + DF$sched_arr_time_minute
  DF$sched_dep_time_posix <-
    as.POSIXct(str_pad(as.character(DF$sched_dep_time), 4 , pad = "0"), format =
                 "%H%M")
  DF$sched_dep_time_hour <- hour(DF$sched_dep_time_posix)
  DF$sched_dep_time_minute <- minute(DF$sched_dep_time_posix)
  
  #num minute is number of minutes since start of day for scheduled depival time
  DF$sched_dep_time_num_minute <-
    60 * DF$sched_dep_time_hour + DF$sched_dep_time_minute
  DF$sched_air_time <-
    DF$sched_arr_time_posix - DF$sched_dep_time_posix
  drops <-
    c(
      'sched_arr_time_posix',
      'sched_arr_time_hour',
      'sched_dep_time_posix',
      'sched_dep_time_hour',
      'sched_dep_time',
      'sched_arr_time',
      'hour',
      'time',
      'minute',
      'time_hour',
      "dep_time",
      "arr_time",
      "air_time",
      "arr_delay",
      "year.x",
      'tailnum'
    )
  DF <- DF[, !(names(DF) %in% drops)]
  
  ## Remove columns with more than 50% NA
  DF <- DF[, -which(colMeans(is.na(DF)) > 0.5)]
  
  DF$sched_air_time <- as.numeric(DF$sched_air_time)
  
  # impute
  impute_model <- imputeMissings::compute(DF, method = "median/mode")
  DF <- impute(DF, object = impute_model, flag = TRUE)
  DF <-
    DF[!duplicated(as.list(DF))]  #remove all redundant flag columns that are identical to each other.
  
  
  # scale all but dep_delay
  dep_delay_vec <- DF$dep_delay
  DF$dep_delay <- NULL
  DF <- DF %>% mutate_if(is.numeric, scale)
  DF$dep_delay <- dep_delay_vec
  
  # exclude dep_delay >= 30
  DF <- DF[DF$dep_delay < 30,]
  DF$flight <- NULL
  
  return(DF)
}

final_test_df <- preprocess_data('fltest.csv.gz')
print(final_test_df)
