# 04-well.R

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
options(expressions=10000)
#ui------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId="file_input","Upload .Json Google takeaway files", multiple = TRUE, accept = ".json"),
      downloadButton(outputId = "downloadData", label = "Download the csv"),
      
      
      uiOutput("date_range"),
     # dateRangeInput(inputId= "date_range", "Choose dates for analysis", format= "dd-mm-yyyy"),
      
      radioButtons(inputId= "analysis_type", label = "How would you like your analysis to be performed?", 
                   c("Yearly" = "yearly",
                     "Quarterly" = "quarterly",
                     "Monthly" = "monthly",
                     "Daily" = "daily")),#switch command?
      
      
      sliderInput(inputId = "num", 
        label = "Choose a number", 
        value = 25, min = 1, max = 100),
      textInput(inputId = "title", 
        label = "Write a title",
        value = "Histogram of Random Normal Values")
    ),
    mainPanel(
      plotOutput("hist"),
      downloadButton(outputId = "download", label = "Download the plot")
    )
  )
)

#server--------------
server <- function(input, output) {
  # JSON file input------------
  getData <- reactive({
  inFile <- input$file_input
  if (is.null(inFile)){
    return(NULL)
  }else {
    
  num_files <- nrow(inFile)
  
  pages <- list()
  for (i in 1:num_files) {
    data <-
      fromJSON(input$file_input[[i, 'datapath']], simplifyMatrix = TRUE, flatten = TRUE) %>% #read JSON file
      as.data.frame() %>% #convert it into data frame
      unnest(event.query.id) # unnest the filed to get time stamp
    pages[[i]] <- data #append pages list to add new data
    
  }
  
  #combine all files
  data <- rbind_pages(pages)
  }
  # renderTable(data)
  
  
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
    ) %>% select(-timestamp_usec,-timestamp_msec) 
  
  data_timechanged$Weekday <- factor(data_timechanged$Weekday, levels= c("Sunday", "Monday",
                                                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
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
  
    
    
    
  data_timechanged
    })
  
  
  
  
  #trial histogram------------------
  # output$hist <- renderPlot({
    # norm_data <- rnorm(input$num)
    
    plotInput <- reactive({
      df <- data.frame(x=rnorm(input$num))
      p <-ggplot(df, aes(x=x)) +
        geom_histogram()
    })
    
    output$hist <- renderPlot({
      print(plotInput())
    })
    #JSON File csv---------------------------------- 
    output$contents <- renderTable(
      data_timechanged()
    )
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(),Sys.time(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(getData(), file, row.names=FALSE)
      })

    
    
    
    output$download <- downloadHandler(
      filename <- function(){ paste('Boxplot.svg') },
      content <- function(file) {
        pdf(file)
        
        print(plotInput())
        
        dev.off()
      }
      # contentType = 'application/pdf'
    )
    
    # 
    # norm_data <- data.frame(x=rnorm(input$num))
    # hist_ggplot <- ggplot(data = norm_data, aes(x=x))+
    #   geom_histogram()
    # renderPlot({print(hist_ggplot)})
    # # hist(rnorm(norm_data
    #            ), main = input$title)
    # })
  
    # output$download <-  downloadHandler(
    #   filename = "test", #function() { paste("test", '.png', sep='') },
    #   content = function(file) {
    #     dev.copy(pdf, file = paste0(filename, "multiplot.svg"))
    #   }
    # )
    
    # output$download <- downloadHandler(
    #   filename =  function() {
    #   paste("iris", input$var3, sep=".")
    #   },
    # content= function(file){
    # pdf(file) # open the pdf device
    # # plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
    # print(plotInput()) # for GGPLOT
    # dev.off()  # turn the device off
    # })
          # filename = function() { paste(input$num, '.pdf', sep='') },
      # content = function(file) {
      #   ggsave(file, plot = plotInput(), device = "pdf")
      # }
    # )
}
shinyApp(ui = ui, server = server)
