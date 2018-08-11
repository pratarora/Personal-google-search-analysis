# 04-well.R

library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId="file_input","Upload .Json Google takeaway files"),
      
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
    
    output$download <- downloadHandler(
      filename <- function(){ paste('Boxplot.svg') },
      content <- function(file) {
        pdf(file)
        
        print(plotInput())
        
        dev.off()
      },
      contentType = 'application/pdf'
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
