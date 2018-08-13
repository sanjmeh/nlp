library(shiny)
library(rdrop2)
#print(getwd())
source("fsm.R")

if(ubuntu){
root <- "nlp/"
token <- drop_auth(rdstoken = path.expand("~/nlp/tokenfile.RDS"))
fold <- drop_dir(dtoken = token,path = "nlp/") %>% as.data.table %>% .[.tag=="folder" & !grepl("/nlp/.git|.rproj|rscon|.pm2",path_lower),str_sub(path_display,6)]
own <- drop_acc(dtoken = token)$name$display_name
}
#runExample("01_hello")
#  library(magrittr)
#  suppressPackageStartupMessages(library(data.table))

#setwd("/home/rstudio/nlp")
source("fsm.R")

ui <- fluidPage(h1("DROPBOX ACCESS TEXT"),
    sidebarLayout(
                  sidebarPanel(p("Dropbox account linked:"),
                      h4(own),
                     # textInput("text","Search string",placeholder = "Enter a file search string"),
                      #p("Files matching:"),
                      selectInput("folders","Folders in dropbox /nlp",choices = fold),
                      actionButton(inputId = "go","GO!"),
                     br(),
                      selectInput("fname","Select File",choices = "loading",selected = "loading")
                     
                  ),                    
     mainPanel(
       h3("List of files"),
       dataTableOutput("files"),
       textOutput("selectedfile")
     )
   )
)

      
server <- function(input, output, session) {
  observe({
    #req(nchar(input$text)>2,cancelOutput = T)
    #updateSelectInput(session,inputId = "folders",label = "Dropbox folders to choose from",choices = dlist)
    #print(paste("file selected:",str_sub(input$folders,6)))
    #print(paste("Normalised:",normalizePath(str_sub(input$folders,6))))
  
   #p <- eventReactive(input$go,paste0(root,input$folders))
    observeEvent(input$go,{
      p <- paste0(root,input$folders)
      p2 <- drop_dir(p) %>% as.data.table
      output$files <- renderDataTable(p2[,c(".tag","name","client_modified","server_modified","size")])
      updateSelectInput(session,inputId = "fname",choices = p2$name)
      output$selectedfile <- renderText(input$fname) 
      #drop_download(path = input$folders)
      #output$page <- renderText({
        #pdf_text(str_sub(input$folders,6)) %>% str_split(pattern = "\n") %>% .[[1]] %>% str_trim() 
    })
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

