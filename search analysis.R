rm(list = ls())
#sets a working directory-----------
setwd("E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches")

#libraries-----------
library(jsonlite)
library(ggplot2)
library(tidyr)
library(dplyr)
# library(broom)
library(tibble)
library(purrr)
library(readr)
library(httr)
library(lubridate)
library(scales)
library(stringr)
library(SnowballC)
library(tm)
library(ColorPalette)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(topicmodels)

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
# rm(data)

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
nameremove <- c("Current Location","Mumbai","India","Maharastra","Pune","Hampshire International Business Park")
data_filtered <- data_timechanged %>% filter( fulldatetime >= "2008-01-01 00:00:00" & fulldatetime <= "2008-12-31 00:00:00")
data_locationremoved <- filter(data_filtered, !str_detect(search_query, paste(nameremove,collapse = '|')))


corpp <- Corpus(VectorSource(data_locationremoved$search_query)) %>%
  tm_map(removePunctuation) %>%
  # tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, c(stopwords("english"))) %>%
  tm_map(stripWhitespace) #%>%
# tm_map(PlainTextDocument)
# docs <- tm_map(docs, stemDocument)

tdm <- TermDocumentMatrix(corpp)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
dd<-subset(d,freq>20)
png(filename = "wordsin_2008.png",
    width = 10, height = 10, units = "cm", pointsize = 12,res=500,
    bg = "white",   type = c("cairo"))
par(mar=c(0.3,0.3,0.3,0.3))
wordcloud(d$word,d$freq,scale=c(4,0.5),max.words = 150,rot.per = 0.35,
          random.order = FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

figPath = "E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches/december.png"
wordcloud2(data=dd, size = 0.5, minSize = 10, gridSize =  2,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-light', backgroundColor = "grey",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE,
           rotateRatio = 0.6, shape = NULL, ellipticity = 1,
           widgetsize = NULL, figPath = NULL)

#to find association between words (which word is usually used with which word)
findAssocs(tdm, terms = "college", corlimit = 0.3)


dd <- dd %>% head(20)
p<- ggplot(data= dd, aes(x = reorder(word, -freq), y = freq, fill= word))+
  geom_bar(stat="identity")+
    scale_fill_grey(start = 0.8, end = 0.2,guide=FALSE)+
  theme(axis.text.x = element_text(angle=60, hjust=1),
        plot.title = element_text(hjust = 0.5))+
  labs(title="Most frequently searched words", x="Words", y="Frequency")
print(p)


#data_filtered------
daterange<-as.Date(c("2015-01-01",max(data_timechanged$fulldatetime)))

rterms <- c("ggplot"," in r","tidyr", "stringr","dplyr", "lubridate","tidyr","tidyverse","scales","data.table","locfit")

data_filtered_r <- data_timechanged %>% filter(str_detect(search_query,paste(rterms,collapse="|")))

termstoremove_R <- c("rajdhani","rupee","rome","room","research","rej","reg","rail","rys","reddit","ric")

data_filtered_r <- filter(data_filtered_r,!str_detect(search_query,paste(termstoremove_R,collapse="|")))

aa <- ggplot(data= data_filtered_r, aes(x=Week))+
  geom_bar(stat="count",fill="blue" ) +
  geom_freqpoly(color="red")+
  scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 year"),
               limits = daterange)+
  labs(title="Searches related to R", x="Time", y="Frequency")
print(aa)


top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}
top_terms_by_topic_LDA(data$event.query.query_text, number_of_topics = 2)
