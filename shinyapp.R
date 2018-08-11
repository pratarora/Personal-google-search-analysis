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


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId="file_input","Upload .Json Google takeaway files", multiple = TRUE, accept = ".json"),
      downloadButton(outputId = "downloadData", label = "Download the csv"),
      
      
      
      dateRangeInput(inputId= "date_range", "Choose dates for analysis"),
      
      radioButtons(inputId= "analysis_timeframe", label = "How would you like your analysis to be performed?", 
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
  }})
  # renderTable(data)
  
  
  
  
  
  
  
  
  
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
      getData() 
    )
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste("data-", Sys.Date(), ".csv", sep="")
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
