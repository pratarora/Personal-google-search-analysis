# Personal-google-search-analysis
This is a R based shiny app for personal google search analysis over the years

It has two parts, one just the script and the other trying to convert the same into a shiny(R) app.

To run the app you need to download your google search history. For this you'll have to:

1) Visit website--- 
https://takeout.google.com/settings/takeout?pli=1

2) Select none (unless you want to download other data)
3) Go to My Activity
4) Select specific activity data
5) Select Search
6) Next and download the Archive
7) Extract the Archive
8) Remember the location of the extracted archive

Then run app.R in Rstudio and upload the search archive file named (My Activity.html) (NOT index.html)
Enjoy the insights :)
(If you are new to R/Rstudio, please install all the libraries using the command install.packages("packagename")
(If you are using just R and not Rstudio, you'll need to run the following commands in R console :

require(shiny)
<br>folder_address = 'C://Users//User 1//Documents//test_app'
<br>runApp(folder_address, launch.browser=TRUE)

If you are using Linux and want to launch the app from terminal (not R console) then:

Rscript -e 'library(methods); shiny::runApp("shinyapp/", launch.browser=TRUE)'
)


Hope you enjoy the app!
