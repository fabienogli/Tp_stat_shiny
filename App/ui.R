library(shiny)

# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  headerPanel("CSV File Upload Demo"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv'))
    )
    
  ,
  mainPanel(
    tableOutput("filetable")
  )
))