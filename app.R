#suppressPackageStartupMessages(library(pdftools))
#cat("test")  
library(shiny)
#runExample("01_hello")
#  library(magrittr)
 # library(rdrop2)
#  suppressPackageStartupMessages(library(data.table))

#setwd("/home/rstudio/nlp")
source("conllu.R")
  

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Checking file contents","Shiny"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(width = 4,selectInput(inputId = "sn",label = "Number of the file",choices = c(1:10),selected = 1)),
                                      
     mainPanel(
       h3(textOutput("First page")),
       tableOutput("x")
     )
   )
)

      

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    #dt <- reactive(input$files) %>% as.data.table()
     
  #file <- isolate(input$file$datapath)

  
     output$x <- renderText({
       seeraw(filename = scfile[input$sn],pageno = 1,string = "")
      # input$file$datapath
     })
     
}

# Run the application 
shinyApp(ui = ui, server = server)

