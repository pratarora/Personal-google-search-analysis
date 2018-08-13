#libraries required for the app------------------
library(shiny)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(broom)
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
library(methods)
library(tidytext)
library(zoo)
options(expressions = 10000)

#ui------------------
ui <- fluidPage(sidebarLayout(
  #sidebar panel UI-------------
  sidebarPanel(
    
    #input JSON files
    fileInput(
      inputId = "file_input",
      "Upload .Json Google takeaway files",
      multiple = TRUE,
      accept = ".json"
    ),
    
    #download csv file
    downloadButton(outputId = "downloadData", label = "Download the csv"),
    
    #input date range for analysis based on input files
    uiOutput("date_range"),
    tags$hr(),
    tabsetPanel(
      type = "pills",
      #search count analysis ui----------------
      tabPanel("Search Count Analysis",
               radioButtons(
                 inputId = "analysis_type",
                 label = "How would you like your analysis to be performed?",
                 c(
                   "Yearly" = "yearly",
                   "Quarterly" = "quarterly",
                   "Monthly" = "monthly",
                   "Weekly" = "weekly",
                   "Daily" = "daily",
                   "According to days of the week" = "weekdays",
                   "According to months in a year" = "month_pooled",
                   "According to dates of the month" = "dates_pooled"
                 ),selected = "monthly"
               )),
      #word analysis ui-----------------
      tabPanel("Word Analysis",
               textInput(inputId = "filterwordsinput",
                         label = "Words to remove from the analysis (Seprate words by a space)",
                         value = NULL
                         ),
               radioButtons(
                 inputId = "word_analysis_type",
                 label = "How would you like your analysis to be performed?",
                 c(
                   "Word Cloud" = "wordcloud",
                   "Word Frequency chart" = "word_freq",
                   "Word Associations" = "word_assoc",
                   "Word Topic" = "word_topic"
                 ),selected = "wordcloud"
               )
               )
      )
    #select what type of analysis is to be done
  #switch command?

  ),
  
  #main panel UI------------
  mainPanel(
    # plotOutput("hist"),
    
    plotOutput("graph"),
    
    downloadButton(
      outputId = "download",
      label = "Download the plot")
    )
))

#server--------------
server <- function(input, output) {
# JSON file input------------
  getData <- reactive({
    inFile <- input$file_input
    if (is.null(inFile)) {
      return(NULL)
    } else {
      num_files <- nrow(inFile)
      
      pages <- list()
      for (i in 1:num_files) {
        data <-
          fromJSON(input$file_input[[i, 'datapath']],
                   simplifyMatrix = TRUE,
                   flatten = TRUE) %>% #read JSON file
          as.data.frame() %>% #convert it into data frame
          unnest(event.query.id) # unnest the filed to get time stamp
        pages[[i]] <- data #append pages list to add new data
        
      }
      
      #combine all files
      data <- rbind_pages(pages)
    }
    
    
    #to make main data frame-----------------------
    #convert epoch time to date and time
    
    data_timechanged <- data %>%
      dplyr::rename('search_query' = 'event.query.query_text') %>%
      mutate(
        timestamp_msec = as.numeric(timestamp_usec) / 1000000,
        fulldatetime = as_datetime(timestamp_msec, tz = "Asia/Calcutta"),
        time = format(fulldatetime, "%T", tz = "Asia/Calcutta"),
        Hour = as_datetime(cut(fulldatetime, breaks = "hour")),
        Day =  as.Date(cut(fulldatetime, breaks = "day")),
        Weekday = weekdays(as.Date(fulldatetime)),
        Week =  as.Date(cut(fulldatetime, breaks = "week")),
        Month =  as.Date(cut(fulldatetime, breaks = "month")),
        Quarter =  as.Date(as.yearqtr(fulldatetime,format = "%Y-%m-%d")),
        Year =  as.Date(cut(fulldatetime, breaks = "year")),
        allmonths = format(fulldatetime, "%m", tz = "Asia/Calcutta"),
        alldates = format(fulldatetime, "%d", tz = "Asia/Calcutta"),
        allhour = format(fulldatetime, "%H", tz = "Asia/Calcutta")
      ) %>% select(-timestamp_usec, -timestamp_msec)
    
    data_timechanged$Weekday <-
      factor(
        data_timechanged$Weekday,
        levels = c(
          "Sunday",
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday"
        )
      )
    
    data_timechanged
  })
  
  
  
  #set range of date depending on input files
  
  output$date_range <- renderUI({
    minval <- min(getData()$Day)
    maxval <- max(getData()$Day)
    dateRangeInput(
      'date_range',
      label = "Choose time-frame for analysis:",
      start = minval,
      end = maxval,
      min = minval,
      max = maxval,
      format = "dd-mm-yyyy",
      weekstart = 1
    )
  })
  
# select and count entries based on input date range--------------
  
  #select entries based on input date range
  range_selected_data <- reactive({
    data2 <- getData() %>%
      filter(Day >= min(input$date_range) &
               Day <= max(input$date_range))
    
  #count number of entries based on input date range
    yearlystats <-
      data2 %>%  group_by(Year) %>% summarise(yearlycount = n())
    data2 <-  merge(data2, yearlystats)
    
    quarterlystats <-
      data2 %>%  group_by(Quarter) %>% summarise(quarterlycount = n())
    data2 <-  merge(data2, quarterlystats)
    
    monthlystats <-
      data2 %>%  group_by(Month) %>% summarise(monthlycount = n())
    data2 <-  merge(data2, monthlystats)
    
    weeklystats <-
      data2 %>%  group_by(Week) %>% summarise(weeklycount = n())
    data2 <-  merge(data2, weeklystats)
    
    weekdaystats <-
      data2 %>%  group_by(Weekday) %>% summarise(weekdaycount = n())
    data2 <-  merge(data2, weekdaystats)
    
    dailystats <-
      data2 %>%  group_by(Day) %>% summarise(dailycount = n())
    data2 <-  merge(data2, dailystats)
    
    hourlystats <-
      data2 %>%  group_by(Hour) %>% summarise(hourlycount = n())
    data2 <-  merge(data2, hourlystats)
    data2
    
  })
  
  #JSON File csv----------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), Sys.time(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  
  #Search Count Analysis-------------
  # graphs of different kind
  #graph for year
  
  
    graph_ggplot <- reactive({
      
      if (input$analysis_type == "yearly") {
        yearlyplot <- ggplot(data = range_selected_data(),
                             aes(as.Date(Year), yearlycount)) +
          stat_summary(
            fun.y = length,
            # adds up all observations for the month
            geom = "bar",
            colour = "dark blue",
            alpha = 0.8
          ) + # or "line"
          # stat_summary(
          #   fun.y = length,
          #   # adds up all observations for the month
          #   geom = "line",
          #   colour = "red",
          #   alpha = 1,
          #   size = 0.8
          # ) + # or "line"
          geom_smooth() +
          scale_x_date(labels = date_format("%Y"),
                       date_breaks = "1 year") +
          labs(title = "Yearly Searches", x = "Time", y = "Count") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5))
        return(yearlyplot)
      }
      if (input$analysis_type == "quarterly") {
        quarterlyplot <- ggplot(data = range_selected_data(),
                                aes(as.Date(as.yearqtr(fulldatetime),format = "%Y-%m-%d"), quarterlycount)) +
          stat_summary(
            fun.y = length,
            # adds up all observations for the month
            geom = "bar",
            colour = "dark blue",
            alpha = 0.8
          ) + # or "line"
          # stat_summary(
          #   fun.y = length,
          #   # adds up all observations for the month
          #   geom = "line",
          #   colour = "red",
          #   alpha = 1,
          #   size = 0.8
          # ) + # or "line"
          # geom_point(colour="red", alpha= 0.5, shape= 21)+
          geom_smooth() +
          labs(title = "Quarterly Searches", x = "Time", y = "Count") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5))
        return(quarterlyplot)
      }
      if (input$analysis_type == "monthly") {
        monthlyplot <- ggplot(data = range_selected_data(),
                              aes(as.Date(Month), monthlycount)) +
          stat_summary(fun.y = length, # adds up all observations for the month
                       geom = "bar", colour= "dark blue", alpha= 0.8) + # or "line"
          # stat_summary(fun.y = length, # adds up all observations for the month
          #              geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
          # geom_point(colour="red", alpha= 0.5, shape= 21)+
          geom_smooth()+
          scale_x_date(
            labels = date_format("%b-'%y"),
            date_breaks = "1 month")+
          labs(title= "Monthly Searches",x= "Time", y= "Count")+
          theme(axis.text.x = element_text(angle=45, hjust=1),
                plot.title = element_text(hjust = 0.5))
        return(monthlyplot)
      }
      
      if(input$analysis_type== "weekly") {
        weeklyplot <- ggplot(data = range_selected_data(),
                             aes(as.Date(Week), weeklycount)) +
          stat_summary(fun.y = length, # adds up all observations for the month
                       geom = "bar", colour= "dark blue", alpha= 0.5) + # or "line"
          # stat_summary(fun.y = length, # adds up all observations for the month
          #              geom = "line", colour= "red", alpha= 0.8) + # or "line"
          geom_smooth()+
          # geom_point(colour="red", alpha= 0.5, shape= 21)+
          scale_x_date(
            labels = date_format("%d-%b-'%y"),
            date_breaks = "2 week")+
          labs(title= "Weekly Searches",x= "Time", y= "Count")+
          theme(axis.text.x = element_text(angle=45, hjust=1),
                plot.title = element_text(hjust = 0.5))
        return(weeklyplot)
      }
      
      if(input$analysis_type== "daily") {
        dailyplot <- ggplot(data = range_selected_data(),
                            aes(as.Date(Hour), dailycount)) +
          stat_summary(fun.y = length, # adds up all observations for the month
                       geom = "bar", colour= "dark blue", alpha= 0.5) + # or "line"
          # stat_summary(fun.y = length, # adds up all observations for the month
          #              geom = "line", colour= "red", alpha= 0.8) + # or "line"
          geom_smooth()+
          # geom_point(colour="red", alpha= 0.5, shape= 21)+
          scale_x_date(
            labels = date_format("%d-%b-'%y"),
            date_breaks = "1 month")+
          labs(title= "Daily Searches",x= "Time", y= "Count")+
          theme(axis.text.x = element_text(angle=45, hjust=1),
                plot.title = element_text(hjust = 0.5))
        return(dailyplot)
      }
      
      if(input$analysis_type== "weekdays"){
        weekdayplot <- ggplot(data=data_timechanged,aes(x= sort(Weekday), weekdaycount))+
          stat_summary(fun.y = length, # adds up all observations for the month
                       geom = "bar", colour= "dark blue", alpha= 0.8) + # or "line"
          # stat_summary(fun.y = length, # adds up all observations for the month
          #              geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
          # geom_point(colour="red", alpha= 0.5, shape= 21)+
          geom_smooth()+
          labs(title= "Searches on various days of the Week",x= "Day", y= "Count")+
          theme(axis.text.x = element_text(angle=45, hjust=1),
                plot.title = element_text(hjust = 0.5))
        return(weekdayplot)
      }
      
      if(input$analysis_type== "month_pooled"){
        monthpooledplot <- ggplot(data_timechanged, aes(x=allmonths,y= monthlycount))+
          stat_summary(fun.y = length, # adds up all observations for the month
                       geom = "bar", colour= "dark blue", alpha= 0.7)+
          # stat_summary(fun.y = length, # adds up all observations for the month
          #              geom = "line", colour= "red", alpha= 0.8, size= 1) +
          labs(title= "Searches on various Months of the Year",x= "Months", y= "Count")+
          scale_x_discrete(labels= month.name)+
          theme(axis.text.x = element_text(angle=45, hjust=1),
                plot.title = element_text(hjust = 0.5))
        return(monthpooledplot)
      }
      
      if (input$analysis_type == "dates_pooled"){
        dates_pooled_plot <- ggplot(data_timechanged, aes(x=alldates,y= dailycount))+
          stat_summary(fun.y = length, # adds up all observations for the month
                       geom = "bar", colour= "dark blue", alpha= 0.7)+
          # stat_summary(fun.y = length, # adds up all observations for the month
          #              geom = "line", colour= "red", alpha= 0.8, size= 1) +
          labs(title= "Searches on various Dates in a Month",x= "Months", y= "Count")+
          scale_x_discrete(breaks = c(seq(0,30,10)))+
          theme(axis.text.x = element_text(angle=45, hjust=1),
                plot.title = element_text(hjust = 0.5))
      return(dates_pooled_plot)
        }
  })
  
  output$graph <- renderPlot({print(graph_ggplot())})
  
  
  # Word Analysis--------------------
  filter_words <- reactive({  
    words <- strsplit(input$filterwordsinput, " ")
    return(words[[1]])
    })
  search_corpus <- reactive({
    corpp <- Corpus(VectorSource(range_selected_data()$search_query)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(tolower)  %>%
      tm_map(removeWords, c(stopwords("english"),filter_words())) %>%
      tm_map(stripWhitespace)
    })
  
  
  # range_selected_data()
  #download graph--------------
  output$download <- downloadHandler(filename =
                                       function() {
                                         paste(input$analysis_type,'.pdf', sep = "")
                                       },
                                     content = function(file) {
                                       pdf(file)
                                       print(graph_ggplot())
                                       dev.off()
                                       contentType = 'application/pdf'
                                     })
  
}
shinyApp(ui = ui, server = server)
