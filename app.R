#libraries required for the app------------------
rm(list = ls())

library(shiny)
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
library(methods)
library(tidytext)
library(zoo)
library(monkeylearn)
library(rvest)
library(parsedate)
options(shiny.maxRequestSize=100*1024^2)
options(expressions = 10000)

#ui------------------
ui <- fluidPage(
  title = "Personal Google Search Analysis",
   wellPanel (
     h1("Personal Google Search Analysis"),
     p("This is a R based shiny app for personal google search analysis over the years"),
     p("To run the app you need to download your google search history. For this you'll have to:"),
     tags$ol(
       tags$li("Visit website---", tags$a(href="https://takeout.google.com/settings/takeout?pli=1","Download data from Google Account")),
       tags$li("Select none (unless you want to download other data)"),
       tags$li("Go to My Activity"),
       tags$li("Select specific activity data"),
       tags$li(" Select Search"),
       tags$li("Next and download the Archive"),
       tags$li("Extract the Archive"),
       tags$li("Remember the location of the extracted archive"),
       tags$li("Upload the search archive file named (My Activity.html) (NOT index.html)")
     ),
     
     p("Enjoy the app! :)"),
     p("Made By Prateek Arora")
   ),
  
  sidebarLayout(
  #sidebar panel UI-------------
  sidebarPanel(
    #input JSON files
    fileInput(
      inputId = "file_input",
      "Upload .html Google Search takeaway files",
      multiple = TRUE,
      accept = ".html"
    ),
    
    #download csv file
    downloadButton(outputId = "downloadData", label = "Download the csv"),
    
    #input date range for analysis based on input files
    uiOutput("date_range"),
    tags$hr(),
    tabsetPanel(
      type = "pills",
      id = "sidetabselection",
      #search count analysis ui----------------
      tabPanel(
        "Search Count Analysis",
        value = 1,
        radioButtons(
          inputId = "analysis_type",
          label = "How would you like your analysis to be performed?",
          c(
            "Yearly" = "yearly",
            "Quarterly" = "quarterly",
            "Monthly" = "monthly",
            "Weekly" = "weekly",
            "Daily" = "daily",
            "Hourly" = "hourly",
            "According to days of the week" = "weekdays",
            "Hourly searches on different days of the week"="hourly_weekday",
            "According to months in a year" = "month_pooled",
            "According to dates of the month" = "dates_pooled"
          ),
          selected = "monthly"
        )
      ),
      #word analysis ui-----------------
      tabPanel(
        "Word Analysis",
        value = 2,
        textInput(
          inputId = "filterwordsinput",
          label = "Words to remove from the analysis (Seprate words by a space)",
          value = NULL
        ),
        
        radioButtons(
          inputId = "word_analysis_type",
          label = "How would you like your analysis to be performed?",
          c("Word Cloud" = "wordcloud",
            "Word Frequency chart" = "word_freq"),
          selected = "wordcloud"
        ),
        
        checkboxInput(inputId = "word_assoc", label = "See word associations" , FALSE),
        
        conditionalPanel(
          "input.word_assoc==true",
          selectInput(
            inputId = "wordnum",
            label = "Which word's association would you like to know",
            multiple = FALSE,
            c(
              "Select Word",
              "First Word" = "one",
              "Second Word" = "two",
              "Third Word" = "three",
              "Fourth Word" = "four",
              "Fifth Word" = "five"
            )
          )
        ),
        checkboxInput(inputId = "word_topic", label = "See what topic is your top word usually associated with? (Does not work on locations/numbers -- Remove them from analysis to know the topics) (May not work-- Requires Monkeylearn ID and API; contact author for details)  " , FALSE)
      )  
      
    )
    #select what type of analysis is to be done
    #switch command?
    
  ),
  
  #main panel UI------------
  mainPanel(
    conditionalPanel(
      condition = "input.sidetabselection==1",
      plotOutput("graph", width = 900, height = 700),
      downloadButton(outputId = "download_searchcount",
                     label = "Download the plot")
    ),
    conditionalPanel(
      condition = "input.sidetabselection==2",
      
      conditionalPanel(condition = "input.word_analysis_type==wordcloud",
                       plotOutput(
                         "wordcloudout", width = 500, height = 500
                       )#,
                       # downloadButton(outputId = "wordcloudplot",
                       #                label = "Download the plot")
      ),
      
      # uiOutput("wordassocout")
      p(conditionalPanel(condition = "input.word_topic=true",
                         verbatimTextOutput("wordtopicout"))),
      conditionalPanel(condition = "input.word_assoc=true",column(1,
                                                                  tableOutput("wordassocout")))
      
      # conditionalPanel(condition ="input.word_assoc=false",
      # removeUI("wordassocout"))
    )
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
      inputdata <- read_html(inFile$datapath)
    }
    date_search <- inputdata %>% 
      html_nodes(xpath = '//div[@class="content-cell mdl-cell mdl-cell--6-col mdl-typography--body-1"]') %>% 
      str_extract(pattern = "(?<=<br>)(.*)(?=UTC)") %>% parsedate::parse_date() #%>% 
    text_search <- inputdata %>% 
      html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>%
      str_extract(pattern = '(?<=<a)(.*)(?=</a>)') %>% 
      str_extract(pattern = '(?<=\">)(.*)')  
    
    #to make main data frame-----------------------
    #convert epoch time to date and time
    
    data_timechanged <- tibble(timestamp = date_search,
                               time = format(timestamp, "%T"),
                               Hour = as_datetime(cut(timestamp, breaks = "hour")),
                               Date =  as.Date(cut(timestamp, breaks = "day")),
                               Weekday = weekdays(as.Date(timestamp)),
                               Week =  as.Date(cut(timestamp, breaks = "week")),
                               Month =  as.Date(cut(timestamp, breaks = "month")),
                               Quarter =  as.Date(cut(timestamp, breaks = "quarter")),
                               Year =  as.Date(cut(timestamp, breaks = "year")),
                               allmonths = format(timestamp, "%m"),
                               alldates = format(timestamp, "%d"),
                               allhour = format(timestamp, "%H"),
                               search_query = text_search) %>% na.omit()
    rm(date_search,text_search, inputdata)
    
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
    minval <- min(getData()$Date)
    maxval <- max(getData()$Date)
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
    
    #count number of entries based on input date range
    yearlystats <-
      data2 %>%  group_by(Year) %>% summarise(yearlycount = n())
    data2 <-  merge(data2, yearlystats)
    rm(yearlystats)
    
    quarterlystats <-
      data2 %>%  group_by(Quarter) %>% summarise(quarterlycount = n())
    data2 <-  merge(data2, quarterlystats)
    rm(quarterlystats)
    
    
    monthlystats <-
      data2 %>%  group_by(Month) %>% summarise(monthlycount = n())
    data2 <-  merge(data2, monthlystats)
    rm(monthlystats)
    
    
    weeklystats <-
      data2 %>%  group_by(Week) %>% summarise(weeklycount = n())
    data2 <-  merge(data2, weeklystats)
    rm(weeklystats)
    
    
    weekdaystats <-
      data2 %>%  group_by(Weekday) %>% summarise(weekdaycount = n())
    data2 <-  merge(data2, weekdaystats)
    rm(weekdaystats)
    
    
    dailystats <-
      data2 %>%  group_by(Date) %>% summarise(dailycount = n())
    data2 <-  merge(data2, dailystats)
    rm(dailystats)
    
    
    hourlystats <-
      data2 %>%  group_by(Hour) %>% summarise(hourlycount = n())
    data2 <-  merge(data2, hourlystats)
    rm(hourlystats)
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
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(yearlyplot)
    }
    if (input$analysis_type == "quarterly") {
      quarterlyplot <- ggplot(data = range_selected_data(),
                              aes(
                                as.Date(as.yearqtr(timestamp), format = "%Y-%m-%d"),
                                quarterlycount
                              )) +
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
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(quarterlyplot)
    }
    if (input$analysis_type == "monthly") {
      monthlyplot <- ggplot(data = range_selected_data(),
                            aes(as.Date(cut(timestamp, breaks = "month")), monthlycount)) +
        stat_summary(
          fun.y = length,
          # adds up all observations for the month
          geom = "bar",
          colour = "dark blue",
          alpha = 0.8
        ) + # or "line"
        # stat_summary(fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
        # geom_point(colour="red", alpha= 0.5, shape= 21)+
        # geom_smooth() +
        scale_x_date(labels = date_format("%b-'%y"),
                     date_breaks = "1 month") +
        labs(title = "Monthly Searches", x = "Time", y = "Count") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(monthlyplot)
    }
    
    if (input$analysis_type == "weekly") {
      weeklyplot <- ggplot(data = range_selected_data(),
                           aes(as.Date(Week), weeklycount)) +
        stat_summary(
          fun.y = length,
          # adds up all observations for the month
          geom = "bar",
          colour = "dark blue",
          alpha = 0.5
        ) + # or "line"
        # stat_summary(fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 0.8) + # or "line"
        geom_smooth() +
        # geom_point(colour="red", alpha= 0.5, shape= 21)+
        scale_x_date(labels = date_format("%d-%b-'%y"),
                     date_breaks = "2 week") +
        labs(title = "Weekly Searches", x = "Time", y = "Count") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(weeklyplot)
    }
    
    if (input$analysis_type == "daily") {
      dailyplot <- ggplot(data = range_selected_data(),
                          aes(as.Date(Hour), dailycount)) +
        stat_summary(
          fun.y = length,
          # adds up all observations for the month
          geom = "bar",
          colour = "dark blue",
          alpha = 0.5
        ) + # or "line"
        # stat_summary(fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 0.8) + # or "line"
        geom_smooth() +
        # geom_point(colour="red", alpha= 0.5, shape= 21)+
        scale_x_date(labels = date_format("%d-%b-'%y"),
                     date_breaks = "1 month") +
        labs(title = "Daily Searches", x = "Time", y = "Count") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(dailyplot)
    }
    if (input$analysis_type == "hourly") {
      hourlyplot <- ggplot(range_selected_data(), aes(x=allhour,y= hourlycount))+
        stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
                     geom = "bar", colour= "dark blue", alpha= 0.7)+
        # stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 0.8, size= 1) +
        labs(title= "Hourly Searches",x= "Time(in Hours)", y= "Count")+
        # scale_x_discrete(labels= (data_timechanged$allhour))+
        theme(plot.title = element_text(hjust = 0.5))
      
      # geom_freqpoly(bins=12, alpha=0.7,colour="red",closed = c("right", "left"))+
      # geom_point(aes(y= hourlycount))
      # geom_histogram(bins=12, alpha=0.5, colour="black",fill="blue")
      # geom_line()+geom_point()#+scale_x_datetime()
      return(hourlyplot)
    }
    if (input$analysis_type == "weekdays") {
      weekdayplot <-
        ggplot(data = range_selected_data(), aes(x = sort(Weekday), weekdaycount)) +
        stat_summary(
          fun.y = length,
          # adds up all observations for the month
          geom = "bar",
          colour = "dark blue",
          alpha = 0.8
        ) + # or "line"
        # stat_summary(fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 1, size= 0.8) + # or "line"
        # geom_point(colour="red", alpha= 0.5, shape= 21)+
        geom_smooth() +
        labs(title = "Searches on various days of the Week", x = "Date", y = "Count") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(weekdayplot)
    }
    if (input$analysis_type == "hourly_weekday") {
      hourly_weekday_plot <- ggplot(range_selected_data(), aes(x=allhour,y= hourlycount, group= Weekday))+
        stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
                     geom = "bar", colour= "dark blue", alpha= 0.7)+
        facet_grid(.~Weekday, scales = "free")+
        # stat_summary(aes(x= as.numeric(allhour),y= hourlycount),fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 0.8, size= 1) +
        labs(title= "Hourly Searches",x= "Time(in Hours)", y= "Count")+
        # scale_x_discrete(labels= (data_timechanged$allhour))+
        theme(plot.title = element_text(hjust = 0.5))
      return(hourly_weekday_plot)
      
    }
    if (input$analysis_type == "month_pooled") {
      monthpooledplot <-
        ggplot(range_selected_data(), aes(x = allmonths, y = monthlycount)) +
        stat_summary(
          fun.y = length,
          # adds up all observations for the month
          geom = "bar",
          colour = "dark blue",
          alpha = 0.7
        ) +
        # stat_summary(fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 0.8, size= 1) +
        labs(title = "Searches on various Months of the Year", x = "Months", y = "Count") +
        scale_x_discrete(labels = month.name) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(monthpooledplot)
    }
    
    if (input$analysis_type == "dates_pooled") {
      dates_pooled_plot <-
        ggplot(range_selected_data(), aes(x = alldates, y = dailycount)) +
        stat_summary(
          fun.y = length,
          # adds up all observations for the month
          geom = "bar",
          colour = "dark blue",
          alpha = 0.7
        ) +
        # stat_summary(fun.y = length, # adds up all observations for the month
        #              geom = "line", colour= "red", alpha= 0.8, size= 1) +
        labs(title = "Searches on various Dates in a Month", x = "Months", y = "Count") +
        scale_x_discrete(breaks = c(seq(0, 30, 10))) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      return(dates_pooled_plot)
    }
  })
  
  output$graph <- renderPlot({
    print(graph_ggplot())
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
    rm(data_wordcloud)
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
          colors = brewer.pal(8, "Dark2")
        )
      return(wc)
    }
    
    if (input$word_analysis_type == "word_freq") {
      wf <- matrix_df_words() %>% slice(1:20)
      word_freq_plot <-
        ggplot(data = wf , aes(
          x = reorder(word,-freq),
          y = freq,
          fill = word
        )) +
        geom_bar(stat = "identity") +
        scale_fill_grey(start = 0.8,
                        end = 0.2,
                        guide = FALSE) +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        ) +
        labs(title = "Top 20 searched words", x = "Words", y = "Frequency")
      return(word_freq_plot)
    }
    
  })
  
  # wordcloud_plot2 <- reactive({
  #   # png("wordcloud.png")
  #   wc <-
  #     wordcloud(
  #       matrix_df_words()$word,
  #       matrix_df_words()$freq,
  #       scale = c(4, 0.3),
  #       max.words = 150,
  #       rot.per = 0.35,
  #       random.order = FALSE,
  #       colors = brewer.pal(8, "Dark2")
  #     )
  #   return(wc)})
  
  
  output$wordcloudout <-  renderPlot(print( wordanalysis()))
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
    wass.df <- as.data.frame(wass) %>% add_rownames("VALUE")
    firstcolname <-
      paste("Words associated with - ", wa$word[n], sep = " ")
    wass.df <-
      wass.df %>% `colnames<-`(c(firstcolname, "Correlation"))
    
  })
  # wassoc <- eventReactive(input$word_assoc==FALSE,{
  #   print(("Word Association turned off"))
  # })
  output$wordassocout <- renderTable({
    wassoc()
  })
  
  
  monkeylearntopic <- reactive({
    if(input$word_topic==TRUE) {
      Sys.getenv("MONKEYLEARN_KEY")
      dd <- matrix_df_words()
      dd %>% map_if(is.factor, as.character) %>% as_data_frame -> dd
      aa <- dd$word[1]
      
      monkey_topic <- monkey_classify(input = dd$word[1],key = monkeylearn_key(quiet = TRUE),
                                      classifier_id = "cl_o46qggZq")
      output_monkey <- monkey_topic %>% unnest() %>% as.data.frame()
      print(paste(output_monkey$req,"is usually associated with", output_monkey$label,"( probability =",output_monkey$probability,", confidence =",output_monkey$confidence,")"), sep=" "
      )
      
    }
    else{print("Word Analysis")}
  })
  output$wordtopicout <- renderText({
    print(monkeylearntopic())
  })
  output$wordfreqout <- renderPlot(print(wordanalysis()))
  
  # output$wordcloudplot <- downloadHandler(
  #   filename = function() {
  #     paste("wordcloud", ".pdf", sep="")
  #   },
  #   content = function(file) {
  #     pdf(file)
  #     print(wordcloud_plot2())
  #     dev.off()
  # 
  # })
  # 
  
}






shinyApp(ui = ui, server = server)
