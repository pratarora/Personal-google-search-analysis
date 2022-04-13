devtools::install_github("ficonsulting/RInno",  build_vignettes = TRUE)
RInno::install_inno() # Use RInno to get Inno Setup (only need to install Inno once)

library(RInno)

#example app from the package
example_app(wd=getwd())
create_app(app_name = "Your appname", app_dir = "app")


create_app(
  app_name = "SearchAna", 
  app_dir = "E:/Data Science/Google data Prateek January 2018/google data/Analysis/Searches/Personal-google-search-analysis/SearchAna",
  pkgs = c("shiny", "ggplot2","dplyr","lubridate","scales","stringr","tm","RColorBrewer",
           "wordcloud", "tidytext","zoo","monkeylearn","parsedate","rvest","purrr","jsonlite","magrittr", "plotly","shinythemes", "shinycssloaders"),
  include_R   = TRUE,     # Download R and install it with your app
  R_version   = "3.5.1",
  privilege   = "lowest",   # Does not require Admin installation
  default_dir = "userdesktop",  # Install to desktop to avoid issues with servers. pf for program files 
  R_flags = '/SILENT'   # Install R 
    )
compile_iss()

install.packages(c("shiny", "ggplot2","dplyr","lubridate","scales","stringr","tm","RColorBrewer",
           "wordcloud", "tidytext","zoo","monkeylearn","parsedate","rvest","purrr","jsonlite","magrittr", "plotly","shinythemes", "shinycssloaders"))
