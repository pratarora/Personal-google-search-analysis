rm(list = ls())
#sets a working directory-----------
setwd("E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches")

#libraries
library(jsonlite)
library(tidyjson)
library(tidyverse)
library(httr)
library(lubridate)
library(scales)
set.seed(2008)



file_list <- list.files(pattern = ".json")

pages <- list()
for (i in seq_along(file_list)) {
  data <-
    fromJSON(file_list[i], simplifyMatrix = TRUE, flatten = TRUE) %>% #read JSON file
    as.data.frame() %>% #convert it into data frame
    unnest(event.query.id) # unnest the filed to get time stamp
  pages[[i]] <- data #append pages list to add new data
  
}
#combine all files
data <- rbind_pages(pages)


#change time in microsecond to seconds
data_timechanged <- data %>%
  mutate(timestamp_msec = as.numeric(timestamp_usec) / 1000000)

rm(data)
#convert epoch time to date and time----
data_timechanged <- data_timechanged %>%
  rename('search_query' = 'event.query.query_text') %>%
  mutate(
    fulldatetime = as_datetime(timestamp_msec, tz = "Asia/Calcutta"),
    time = format(fulldatetime, "%T", tz = "Asia/Calcutta"),
    Hour = as_datetime(cut(fulldatetime, breaks = "hour")),
    Day =  as.Date(cut(fulldatetime, breaks = "day")),
    Weekday = weekdays(as.Date(fulldatetime)),
    Week =  as.Date(cut(fulldatetime, breaks = "week")),
    Month =  as.Date(cut(fulldatetime, breaks = "month")),
    Quarter =  as.Date(cut(fulldatetime, breaks = "quarter")),
    Year =  as.Date(cut(fulldatetime, breaks = "year")),
    allmonths = format(fulldatetime, "%b", tz = "Asia/Calcutta"),
    alldates = format(fulldatetime, "%D", tz = "Asia/Calcutta"),
    allhour = format(fulldatetime, "%H", tz = "Asia/Calcutta")
   ) %>% select(-timestamp_usec,-timestamp_msec)

data_timechanged$Weekday <- factor(data_timechanged$Weekday, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

yearlystats <- data_timechanged %>%  group_by(Year) %>% summarise(yearlycount= n())
data_timechanged <-  merge(data_timechanged,yearlystats)

quarterlystats <- data_timechanged %>%  group_by(Quarter) %>% summarise(quarterlycount= n())
data_timechanged <-  merge(data_timechanged,quarterlystats)

monthlystats <- data_timechanged %>%  group_by(Month) %>% summarise(monthlycount= n())
data_timechanged <-  merge(data_timechanged,monthlystats)

weeklystats <- data_timechanged %>%  group_by(Week) %>% summarise(weeklycount= n())
data_timechanged <-  merge(data_timechanged,weeklystats)

weekdaystats <- data_timechanged %>%  group_by(Weekday) %>% summarise(weekdaycount= n())
data_timechanged <-  merge(data_timechanged,weekdaystats)

dailystats <- data_timechanged %>%  group_by(Day) %>% summarise(dailycount= n())
data_timechanged <-  merge(data_timechanged,dailystats)

hourlystats <- data_timechanged %>%  group_by(Hour) %>% summarise(hourlycount= n()) 
data_timechanged <-  merge(data_timechanged,hourlystats)

# rm(list=setdiff(ls(), "data_timechanged"))

data_filtered <- data_timechanged %>% filter( fulldatetime >= "2017-12-01 00:00:00" & Day <= "2017-12-31 00:00:00")

#pooled Hourly data
p <- ggplot(data_timechanged, aes(x=as.numeric(allhour),y= hourlycount))+
  stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.7)+
  stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 0.8, size= 1) +
  labs(title= "Hourly Searches",x= "Time(in Hours)", y= "Count")+
  theme(plot.title = element_text(hjust = 0.5))
  
  # geom_freqpoly(bins=12, alpha=0.7,colour="red",closed = c("right", "left"))+
  # geom_point(aes(y= hourlycount))
  # geom_histogram(bins=12, alpha=0.5, colour="black",fill="blue")
   # geom_line()+geom_point()#+scale_x_datetime()
print(p)

#daily searches
q <- ggplot(data = data_timechanged,
            aes(as.Date(Hour), dailycount)) +
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.5) + # or "line"
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 0.8) + # or "line"
  geom_smooth()+
  # geom_point(colour="red", alpha= 0.5, shape= 21)+
    scale_x_date(
    labels = date_format("%d-%b-'%y"),
    date_breaks = "1 year")+
  labs(title= "Daily Searches",x= "Time", y= "Count")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(q)

#weekly searches
v <- ggplot(data = data_timechanged,
            aes(as.Date(Week), weeklycount)) +
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.5) + # or "line"
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 0.8) + # or "line"
  geom_smooth()+
  # geom_point(colour="red", alpha= 0.5, shape= 21)+
  scale_x_date(
    labels = date_format("%d-%b-'%y"),
    date_breaks = "1 year")+
  labs(title= "Weekly Searches",x= "Time", y= "Count")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(v)


#monthly searches
r <- ggplot(data = data_timechanged,
            aes(as.Date(Month), monthlycount)) +
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.8) + # or "line"
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
  # geom_point(colour="red", alpha= 0.5, shape= 21)+
  geom_smooth()+
  scale_x_date(
<<<<<<< HEAD
    labels = date_format("%b-'%y"),
=======
    labels = date_format("%d-%b-'%y"),
>>>>>>> c3a53d6e572c20a68f66124412629e1bc21ba135
    date_breaks = "1 year")+
  labs(title= "Monthly Searches",x= "Time", y= "Count")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(r)

#quarterly searches
s <- ggplot(data = data_timechanged,
            aes(as.Date(Quarter), quarterlycount)) +
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.8) + # or "line"
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
  # geom_point(colour="red", alpha= 0.5, shape= 21)+
  geom_smooth()+
  scale_x_date(
<<<<<<< HEAD
    labels = date_format("%b-'%y"),
=======
    labels = date_format("%d-%b-'%y"),
>>>>>>> c3a53d6e572c20a68f66124412629e1bc21ba135
    date_breaks = "1 year")+
  labs(title= "Quarterly Searches",x= "Time", y= "Count")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(s)



#yearly searches
t <- ggplot(data = data_timechanged,
            aes(as.Date(Year), yearlycount)) +
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.8) + # or "line"
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
  # geom_point(colour="red", alpha= 0.5, shape= 21)+
  geom_smooth()+
  scale_x_date(
<<<<<<< HEAD
    labels = date_format("%Y"),
=======
    labels = date_format("%d-%b-'%y"),
>>>>>>> c3a53d6e572c20a68f66124412629e1bc21ba135
    date_breaks = "1 year")+
  labs(title= "Yearly Searches",x= "Time", y= "Count")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(t)

<<<<<<< HEAD
#weekday searches
=======
>>>>>>> c3a53d6e572c20a68f66124412629e1bc21ba135
u <- ggplot(data=data_timechanged,aes(x= sort(Weekday), weekdaycount))+
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.8) + # or "line"
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
  # geom_point(colour="red", alpha= 0.5, shape= 21)+
  geom_smooth()+
<<<<<<< HEAD
  labs(title= "Searches on various days of the Week",x= "Day", y= "Count")+
=======
  # scale_x_date(
  #   labels = date_format("%d-%b-'%y"),
  #   date_breaks = "1 year")+
  labs(title= "Yearly Searches",x= "Day", y= "Count")+
>>>>>>> c3a53d6e572c20a68f66124412629e1bc21ba135
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(u)
