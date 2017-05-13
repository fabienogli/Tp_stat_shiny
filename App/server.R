library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath, header=TRUE, sep = ";")
  })
  
 
  
  
  
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    df<-filedata()
  })
  

  
})