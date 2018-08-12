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
    
    #select what type of analysis is to be done
    radioButtons(
      inputId = "analysis_type",
      label = "How would you like your analysis to be performed?",
      c(
        "Yearly" = "yearly",
        "Quarterly" = "quarterly",
        "Monthly" = "monthly",
        "Daily" = "daily"
      )
    ),#switch command?
    
    #trial slider
    sliderInput(
      inputId = "num",
      label = "Choose a number",
      value = 50,
      min = 1,
      max = 100
    ),
    textInput(
      inputId = "title",
      label = "Write a title",
      value = "Histogram of Random Normal Values"
    )
  ),
  
  #main panel UI------------
  mainPanel(
    plotOutput("hist"),
    
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
        Quarter =  as.Date(cut(fulldatetime, breaks = "quarter")),
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
      write.csv(range_selected_data(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  #trial histogram------------------
  plotInput <- reactive({
    df <- data.frame(x = rnorm(input$num))
    p <- ggplot(df, aes(x = x)) +
      geom_histogram()
  })
  
  output$hist <- renderPlot({
    print(plotInput())
  })
  
  
  
  
  #download graph--------------
  output$download <- downloadHandler(filename <-
                                       function() {
                                         paste('Boxplot.pdf')
                                       },
                                     content <- function(file) {
                                       pdf(file)
                                       
                                       print(plotInput())
                                       
                                       dev.off()
                                       contentType = 'application/pdf'
                                     })
  
}
shinyApp(ui = ui, server = server)
