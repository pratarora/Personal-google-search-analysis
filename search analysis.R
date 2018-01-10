rm(list=ls())
#sets a working directory-----------
setwd(
  "E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches"
)

#libraries
library(jsonlite)
library(tidyjson)
library(tidyverse)
library(httr)
library(lubridate)



file_list <- list.files(pattern = ".json")

pages <- list()
for(i in seq_along(file_list)){
  
  data <- fromJSON(file_list[i],simplifyMatrix = TRUE, flatten = TRUE) %>% #read JSON file
    as.data.frame() %>% #convert it into data frame
    unnest(event.query.id) # unnest the filed to get time stamp
    pages[[i]] <- data #append pages list to add new data
}
#combine all files
data <- rbind_pages(pages)


#change time in microsecond to seconds
data_timechanged <- data %>% mutate(timestamp_msec = as.numeric(timestamp_usec)/1000000)

#convert epoch time to date and time
data_timechanged <- data_timechanged %>% mutate(searchtime = as_datetime(timestamp_msec))


