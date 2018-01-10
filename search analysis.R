rm(list=ls())
#sets a working directory-----------
setwd(
  "E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches"
)


library(jsonlite)
library(tidyjson)
library(tidyverse)
library(httr)
library(lubridate)
Folderadd <-
  "E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches"
filename <-
  "2017-10-01 October 2017 to December 2017"
format <- ".json"
fulladdress <- paste(filename, format, sep = "")

# data <- stream_in(file(fulladdress))
data <-   fromJSON(fulladdress,simplifyMatrix = TRUE, flatten = TRUE) %>% as.data.frame() %>% unnest(event.query.id)
data_timechanged <- data %>% mutate(timestamp_msec = as.numeric(timestamp_usec)/1000000)




colnames(data)
data_timechanged <- data_timechanged %>% mutate(searchtime = as_datetime(timestamp_msec))
