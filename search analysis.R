rm(list = ls())
#sets a working directory-----------
setwd("E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches")

#libraries-----------
library(jsonlite)
library(tidyjson)
library(tidyverse)
library(httr)
library(lubridate)
library(scales)
library(stringr)
library(SnowballC)
library(tm)
library(ColorPalette)
library(RColorBrewer)
library(wordcloud)
set.seed(2008)


# read all json files----------
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


# #change time in microsecond to seconds
# data_timechanged <- data %>%
#   mutate()
#make main dataframe----

#convert epoch time to date and time
data_timechanged <- data %>%
  rename('search_query' = 'event.query.query_text') %>%
  mutate(
    timestamp_msec = as.numeric(timestamp_usec) / 1000000,
    fulldatetime = as_datetime(timestamp_msec, tz = "Asia/Calcutta"),
    time = format(fulldatetime, "%T", tz = "Asia/Calcutta"),
    Hour = as_datetime(cut(fulldatetime, breaks = "hour")),
    Day =  as.Date(cut(fulldatetime, breaks = "day")),
    Weekday = weekdays(as.Date(fulldatetime)),
    Week =  as.Date(cut(fulldatetime, breaks = "week")),
    Month =  as.Date(cut(fulldatetime, breaks = "month")),
    Quarter =  as.Date(cut(fulldatetime, breaks = "quarter")),
    Year =  as.Date(cut(fulldatetime, breaks = "year")),
    allmonths = format(fulldatetime, "%m", tz = "Asia/Calcutta"),
    alldates = format(fulldatetime, "%d", tz = "Asia/Calcutta"),
    allhour = format(fulldatetime, "%H", tz = "Asia/Calcutta")
   ) %>% select(-timestamp_usec,-timestamp_msec)

data_timechanged$Weekday <- factor(data_timechanged$Weekday, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
rm(data)

#allcounts and merging in main dataframe----
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




#various graphs for pooled data----------
#pooled Hourly data
p <- ggplot(data_timechanged, aes(x=allhour,y= hourlycount))+
  stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.7)+
  stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 0.8, size= 1) +
  labs(title= "Hourly Searches",x= "Time(in Hours)", y= "Count")+
  # scale_x_discrete(labels= (data_timechanged$allhour))+
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
    labels = date_format("%b-'%y"),
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
    labels = date_format("%b-'%y"),
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
    labels = date_format("%Y"),
    date_breaks = "1 year")+
  labs(title= "Yearly Searches",x= "Time", y= "Count")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(t)

#weekday searches
u <- ggplot(data=data_timechanged,aes(x= sort(Weekday), weekdaycount))+
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.8) + # or "line"
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
  # geom_point(colour="red", alpha= 0.5, shape= 21)+
  geom_smooth()+
  labs(title= "Searches on various days of the Week",x= "Day", y= "Count")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(u)

#pooled monthly
w <- ggplot(data_timechanged, aes(x=allmonths,y= monthlycount))+
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.7)+
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 0.8, size= 1) +
  labs(title= "Searches on various Months of the Year",x= "Months", y= "Count")+
  scale_x_discrete(labels= month.name)+
theme(axis.text.x = element_text(angle=45, hjust=1),
      plot.title = element_text(hjust = 0.5))
# geom_freqpoly(bins=12, alpha=0.7,colour="red",closed = c("right", "left"))+
# geom_point(aes(y= hourlycount))
# geom_histogram(bins=12, alpha=0.5, colour="black",fill="blue")
# geom_line()+geom_point()#+scale_x_datetime()
print(w)

#pooled datewise searches
y <- ggplot(data_timechanged, aes(x=alldates,y= dailycount))+
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "bar", colour= "dark blue", alpha= 0.7)+
  stat_summary(fun.y = length, # adds up all observations for the month
               geom = "line", colour= "red", alpha= 0.8, size= 1) +
  labs(title= "Searches on various Dates in a Month",x= "Months", y= "Count")+
  scale_x_discrete(breaks = c(seq(0,30,10)))+
    theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))
print(y)



#to create wordcloud--------
nameremove <- c("Current Location","Mumbai","India","Maharastra","Pune")
data_filtered <- data_timechanged %>% filter( fulldatetime >= "2017-01-01 00:00:00" & fulldatetime <= "2017-12-31 00:00:00")
data_locationremoved <- filter(data_filtered, !str_detect(search_query, paste(nameremove,collapse = '|')))


corpp <- Corpus(VectorSource(data_locationremoved$search_query)) %>%
  tm_map(removePunctuation) %>%
  # tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, c(stopwords("english"))) %>%
  tm_map(stripWhitespace) #%>%
# tm_map(PlainTextDocument)
tdm <- TermDocumentMatrix(corpp)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
dd<-subset(d,freq>10)
png(filename = "wordsin_2017.png",
    width = 10, height = 10, units = "cm", pointsize = 12,res=500,
    bg = "white",   type = c("cairo"))
par(mar=c(0.3,0.3,0.3,0.3))
wordcloud(d$word,d$freq,scale=c(4,0.5),max.words = 150,rot.per = 0.35,
          random.order = FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
