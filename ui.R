
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
options(shiny.maxRequestSize=100*1024^2)
options(expressions = 10000)
options(spinner.type = 1)

#ui------------------
ui <- fluidPage(
  title = "SearchAna",
  theme = shinytheme("journal"),
  wellPanel (
    h1("SearchAna"),
    p("This is a R based app which analyzes your Google Searches over the years."),
    p(tags$b("The app does not store any of your information and runs offline using your computer.")),
    p("To run the app you need to download your google search history. For this you'll have to:"),
    tags$ol(
      tags$li("Visit website---", tags$a(href="https://takeout.google.com/settings/takeout?pli=1","Download data from Google Account", target= "_blank")),
      tags$li("Select none (unless you want to download other data)"),
      tags$li("Go to My Activity and toggle it on"),
      tags$li("Select specific activity data"),
      tags$li("Select Search"),
      tags$li("Next and download the Archive"),
      tags$li("Extract the Archive"),
      tags$li("Remember the location of the extracted archive"),
      tags$li("Now in the app, 'Browse' to your to the extracted archive"),
      tags$li("Goto Takeout >> My Activity >> Search >> Upload file named (My Activity.html) (NOT index.html)")
    ),
    p("Enjoy the app! :)"),
    p("Made By Prateek Arora"),
    p("Code available at ", tags$a(href="https://github.com/pratarora/Personal-google-search-analysis", "Github.", target= "_blank"))
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
          checkboxInput(inputId = "word_topic", label = "See what topic is your top word usually associated with? (Does not work on locations/numbers -- Remove them from analysis to know the topics)(May not work-- Requires Monkeylearn ID and API; contact author for details) " , FALSE)
        )  
        
      )
      #select what type of analysis is to be done
      #switch command?
      
    ),
    
    #main panel UI------------
    mainPanel(
      conditionalPanel(
        condition = "input.sidetabselection==1",
        plotOutput("graph", width = 900, height = 700) %>% withSpinner(),
        downloadButton(outputId = "download_searchcount",
                       label = "Download the plot")
      ),
      conditionalPanel(
        condition = "input.sidetabselection==2",
        
        conditionalPanel(condition = "input.word_analysis_type==wordcloud",
                         plotOutput(
                           "wordcloudout", width = 500, height = 500
                         )%>% withSpinner()#,
                         # downloadButton(outputId = "wordcloudplot",
                         #                label = "Download the plot")
        ),
        
        # uiOutput("wordassocout")
        p(conditionalPanel(condition = "input.word_topic=true",
                           verbatimTextOutput("wordtopicout"))%>% withSpinner()),
        conditionalPanel(condition = "input.word_assoc=true",column(1,
                                                                    tableOutput("wordassocout")))
        
        # conditionalPanel(condition ="input.word_assoc=false",
        # removeUI("wordassocout"))
      )
    )
  ))