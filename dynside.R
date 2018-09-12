library(shiny)
library(ggplot2)
source("fsm.R")
#dsent <- genr_dt(dtm_flat,big = F)
if(!exists("dsent$gold")) {
  # adding dummy gold statements as last few of each case
  dsent[,{x<- max(sentence_id); .SD[sentence_id %in% (x-5):(x-3),.(lastfew=suid)]},by=doc_id] -> goldsent
  dsent[suid %in% goldsent$lastfew,gold:=T]
}
#pos <- dtm_flat$upos %>% unique()
#features <- dtm_flat$feats %>% unique()

server <- function(input,output)({
  
  dt <- reactive({
    fpat(dsent,pat = input$keyword,id = "pattern")[doc_id==input$doc]
  })
  
  ## to output the full DT for a file
  output$dat <- renderDataTable({
    dt()
  })
  
  # output gold DT
  output$gold <- renderDataTable({
    dt()[gold==T,.(doc_id,suid,sentence)] %>% unique
  })
  
   dat1 <- reactive({
    f1 <- f1score(dt(),rule = "pattern")
    suppressWarnings(melt(f1,id.vars = "rule")[1:3,c(2,3)])
  })
   
   dat2 <- reactive({
     f1 <- f1score(dt(),rule = "words")
     suppressWarnings(melt(f1,id.vars = "rule")[1:3,c(2,3)])
   })
    # For plot
  output$plot1 <- renderPlot({
    ggplot(dat1(),aes(variable,value)) + geom_col()
  }) 
  
  output$plot2 <- renderPlot({
    ggplot(dat2(),aes(variable,value)) + geom_col()
  }) 
  
})


ui <- fluidPage(
  h2("Use case - Change the side bar panel elements based on the selected tab."),
  sidebarLayout(
    sidebarPanel(
      
      ## conditionalPanel() functions for selected tab
      conditionalPanel(condition="input.tabselected==1",h4("Files available"),
                       selectInput("doc","File Name for Viewing",choices = unique(dtm_flat[,doc_id])
                       ),
                       radioButtons("choice","Choose an option", choices=c("ALL" = 1, "Gold" = 2)
                       )),
      conditionalPanel(condition="input.tabselected==2",h4("Rules applied"),
                       textInput("keyword",
                                   "Keywords",
                                   value = ""
                       ),
                       sliderInput("nos",
                                   "Number of words",min = 3,max = 45,step = 5,ticks = T,value = 10
                       )
      )
      
    ),
    mainPanel(
      # id argument is important in the tabsetPanel()
      # value argument is important in the tabPanel()
      tabsetPanel(
        tabPanel("Sentences", value=1, conditionalPanel(condition="input.choice==1", dataTableOutput("dat")),
                 conditionalPanel(condition="input.choice==2", dataTableOutput("gold"))),
        tabPanel("F1 scoring", value=2, conditionalPanel(condition="input.choice==1", plotOutput("plot1")),
                 conditionalPanel(condition="input.choice==2", plotOutput("plot2"))), 
        id = "tabselected"
      )
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)