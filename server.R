library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(stringr)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(tidytext)
library(zoo)
library(shinycssloaders)
library(rvest)
library(parsedate)
library(purrr)
library(plotly)
options(shiny.maxRequestSize=20*1024^2) #change here "200" to no. of MBs you want the file size to be uploaded 
options(expressions = 10000)
options(spinner.type = 1)




#server--------------
server <- function(input, output, session) {
  # JSON file input------------
  getData <- reactive({
    inFile <- input$file_input
    if (is.null(inFile)) {
      return(NULL)
    } else {
      # inputdata <- read_html("My Activity.html")
      inputdata <- read_html(inFile$datapath)
    }
    date_search <- inputdata  %>% 
      html_nodes(xpath = '//div[@class="content-cell mdl-cell mdl-cell--6-col mdl-typography--body-1"]') %>%
      str_extract(pattern = "(?<=<br>)(.*)(?=</div>)") %>% parsedate::parse_date() 
    date_search %>% head()
    
    text_search <- inputdata %>% 
      html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>%
      str_extract(pattern = '(?<=<a)(.*)(?=</a>)') %>% 
      str_extract(pattern = '(?<=\">)(.*)')  
    
    #to make main data frame-----------------------
    #convert epoch time to date and time
    
    data_timechanged <- tibble(timestamp = date_search,
                               time = format(timestamp, "%T"),
                               Hour = lubridate::hour(date_search),
                               Date =  lubridate::date(date_search),
                               Weekday = weekdays(date_search),
                               Week =  lubridate::week(date_search),
                               Month =  lubridate::month(ymd_hms(date_search), label = TRUE, abbr = FALSE),
                               Quarter =  lubridate::quarter(date_search),
                               Year = lubridate::year(date_search),
                               search_query = text_search) %>% na.omit()
    
    data_timechanged$Weekday <- factor(data_timechanged$Weekday, levels = c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday"))
    
    
    
    
    
    rm(date_search,text_search, inputdata)
    return(data_timechanged)
  })
  
  
  
  #set range of date depending on input files
  
  output$date_range <- renderUI({
    minval <- min(getData()$Date[!is.infinite(getData()$Date)], na.rm = T)
    maxval <- max(getData()$Date[!is.infinite(getData()$Date)], na.rm = T)
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
      filter(Date >= min(input$date_range) &
               Date <= max(input$date_range))

    return(data2)
    
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
  
  
  
  
  #Search Count Analysis-------------
  # graphs of different kind
  #graph for year
  
  
  color_scheme <- list(low= "grey", high= "tomato")
  
  graph_ggplot <- reactive({
    if (input$analysis_type == "yearly") {
      data3 <- range_selected_data() %>% add_count(Year, name = "count") %>% distinct(Year, count) # adding this because stat="count", looses the data ingformation
      yearlyplot <- ggplot(data = data3,
               aes(x= Year,y= count, fill=count)) +
        geom_bar(stat="identity")+
        scale_x_yearqtr(
          labels = date_format("%Y"),n = min(length(unique(range_selected_data()$Year)), 10)
        )+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        labs(title= "Yearly Searches",x= "Time", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))
        
      return(yearlyplot)
    }
    if (input$analysis_type == "quarterly") {
      data3 <- range_selected_data() %>% add_count(Quarter, name = "count") %>% distinct(Quarter, count) # adding this because stat="count", looses the data ingformation
      quarterlyplot <- ggplot(data = data3,
               aes(x=Quarter, y= count, fill= count)) +
        geom_bar(stat="identity")+  
        scale_x_yearqtr(format="%Y",)+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        labs(title= "Quarterly Searches",x= "Quarter Number", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))
      return(quarterlyplot)
    }
    if (input$analysis_type == "monthly") {
      data3 <- range_selected_data() %>% add_count(Month, name = "count") %>% distinct(Month, Year, count) # adding this because stat="count", looses the data ingformation
      
      monthlyplot <-  ggplot(data=data3,
                             aes(x=Month, y=count,fill=count, group= Year
             )) +
        geom_bar(stat = "identity")+
        scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        labs(title= "Monthly Searches",x= "Time", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))+ facet_grid(~Year, scales = "free")
      return(monthlyplot)
    }
    
    if (input$analysis_type == "weekly") {
      data3 <- range_selected_data() %>% add_count(Week, name = "count") %>% distinct(Week, count) # adding this because stat="count", looses the data ingformation
      weeklyplot <- ggplot(data = data3,
               aes(x = Week, y= count, fill=count)) +
        geom_bar(stat = "identity")+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        labs(title= "Weekly Searches",x= "Week Number", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))
      return(weeklyplot)
    }
    
    if (input$analysis_type == "daily") {
      data3 <- range_selected_data() %>% add_count(Date, name = "count") %>% distinct(Date,Month, Year, count) # adding this because stat="count", looses the data ingformation
      dailyplot <- ggplot(data = data3,
               aes(x = Date, y= count, fill=count)) +
        geom_bar(stat = "identity")+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        scale_x_date(
          labels = date_format("%d-%b-'%y")
        )+
        labs(title= "Daily Searches",x= "Time", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))
      
      return(dailyplot)
    }
    if (input$analysis_type == "hourly") {
      data3 <- range_selected_data() %>% add_count(Hour, name = "count") %>% distinct(Hour, count) # adding this because stat="count", looses the data ingformation
      hourlyplot <- ggplot(data3, aes(x=Hour, y=count,fill=count))+
        geom_bar(stat="identity")+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5))
     
      return(hourlyplot)
    }
    if (input$analysis_type == "weekdays") {
      data3 <- range_selected_data() %>% add_count(Weekday, name = "count") %>% distinct(Weekday, count) # adding this because stat="count", looses the data ingformation
      weekdayplot <- ggplot(data=data3,aes(x= Weekday, y=count, fill= count))+
        geom_bar(stat="identity")+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        labs(title= "Searches on various days of the Week",x= "Date", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))
      return(weekdayplot)
    }
    if (input$analysis_type == "hourly_weekday") {
      hourly_weekday_plot <- ggplot(range_selected_data(), aes(x=Hour,fill= ..count.., group= Weekday))+
        geom_bar()+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        facet_grid(.~Weekday, scales = "free")+
        labs(title= "Hourly Searches",x= "Time(in Hours)", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))
      return(hourly_weekday_plot)
      
    }
    if (input$analysis_type == "month_pooled") {
      data3 <- range_selected_data() %>% add_count(Month, name = "count") %>% distinct(Month, count) # adding this because stat="count", looses the data ingformation
      
      monthpooledplot <-
      ggplot(data = data3,
               aes(x=Month, y= count,fill=count)) +
        geom_bar(stat="identity")+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        labs(title= "Monthly Searches Pooled",x= "Time", y= "Count")+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5))
        return(monthpooledplot)
    }
    
    if (input$analysis_type == "dates_pooled") {
      data3 <- range_selected_data() %>% mutate(Date_of_Month=lubridate::day(Date))%>% add_count(Date_of_Month, name = "count") %>% distinct(Date_of_Month, count) # adding this because stat="count", looses the data ingformation
      dates_pooled_plot <-
        ggplot(data3, aes(x=Date_of_Month,y=count, fill=count))+
        geom_bar(stat="identity")+
        labs(title= "Searches on various Dates in a Month",x= "Months", y= "Count")+
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        theme(axis.text.x = element_text(angle=45, hjust=1),
              plot.title = element_text(hjust = 0.5)) 
      return(dates_pooled_plot)
    }
  })
 
  output$graph <- renderPlotly({
    ggplotly(graph_ggplot(),tooltip = c("y","x"))
  })
  #download searchcount graph--------------
  output$download_searchcount <- downloadHandler(
    filename =
      function() {
        paste(input$analysis_type, '.pdf', sep = "")
      },
    content = function(file) {
      pdf(file)
      print(graph_ggplot())
      dev.off()
      contentType = 'application/pdf'
    }
  )
  
  
  # Word Analysis--------------------
  filter_words <- reactive({
    words_1 <- strsplit(input$filterwordsinput, " ")
    words <- words_1[[1]] %>% tolower() %>% removePunctuation()
    
    
    return(words)
  })
  search_corpus <- reactive({
    corpp <-
      Corpus(VectorSource(range_selected_data()$search_query)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(tolower)  %>%
      tm_map(removeWords, c(stopwords("english"), filter_words())) %>%
      tm_map(stripWhitespace)
  })
  tdm_words <- reactive({
    tdm <- TermDocumentMatrix(search_corpus())
  })
  matrix_df_words <- reactive({
    data_wordcloud <- range_selected_data()$search_query %>% 
      removePunctuation() %>% 
      removeNumbers()%>% 
      tolower() %>% 
      removeWords(c(stopwords("english"),filter_words())) %>% 
      stripWhitespace()
    
    data_wordcloud <- tibble(search= data_wordcloud) %>% 
      unnest_tokens(word, search,token="regex")
    d <- data_wordcloud %>% group_by(word) %>%  summarise(freq= n()) %>% arrange(-freq)
    return(d)
  })
  wordanalysis <- reactive({
    if (input$word_analysis_type == "wordcloud") {
      wc <-
        wordcloud(
          matrix_df_words()$word,
          matrix_df_words()$freq,
          scale = c(4, 0.3),
          max.words = 150,
          rot.per = 0.35,
          random.order = FALSE,
          colors = rev(brewer.pal(n = 8,name =  "RdGy"))
        )
      return(wc)
    }
    
    if (input$word_analysis_type == "word_freq") {
      wf <- matrix_df_words() %>% slice(1:20)
      print(head(wf))
      word_freq_plot <-
        ggplot(data = wf , aes(
          x = reorder(word,-freq),
          y = freq,
          fill = freq
        )) +
        geom_bar(stat = "identity") +
         scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
        theme_bw()+
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        ) +
        labs(title = "Top 20 searched words", x = "Words", y = "Frequency")
      return(word_freq_plot)
    }
    
  })
  if(is.null(range_selected_data())){}
  else{
    output$wordcloudout <-  
      renderPlot(print(wordanalysis()))
  }
  
 
   wassoc <- eventReactive(input$wordnum, {
    wa <- matrix_df_words() %>% slice(1:5)
    wa$word <- as.character(wa$word)
    
    if (input$wordnum == "one") {
      n <- 1
    }
    if (input$wordnum == "two") {
      n <- 2
    }
    if (input$wordnum == "three") {
      n <- 3
    }
    if (input$wordnum == "four") {
      n <- 4
    }
    if (input$wordnum == "five") {
      n <- 5
    }
    
    wass <-
      findAssocs(tdm_words(), terms = wa$word[n], corlimit = 0.1)
    wass.df <- as.data.frame(wass) %>% add_rownames("Associated_Words") %>% 
      `colnames<-`(c("Associated_Words", "Correlation")) %>% arrange(-Correlation)
    

    
    print(head(wass.df))
    
    word_assoc_graph <- ggplot(data= wass.df, aes(x= reorder(Associated_Words, -Correlation), y=Correlation , fill = Correlation))+
      geom_bar(stat="identity")+
      scale_fill_gradient(low= color_scheme$low, high = color_scheme$high)+
      theme_bw()+
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(title = "Associated words with search query", x = "Words", y = "Correlation")
    
  })
  
  output$wordassocout <- renderPlotly({
    wassoc()
  })
  
  
 
  
  output$wordfreqout <- renderPlot(print(wordanalysis()))
  
  # output$wordcloudplot <- downloadHandler(
  #   filename = function() {
  #     paste("wordcloud", ".pdf", sep="")
  #   },
  #   content = function(file) {
  #     pdf(file)
  #     print(wordanalysis())
  #     dev.off()
  #     contentType = 'application/pdf'
  # })
  # 
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
}