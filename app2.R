library(shiny)
library(ggplot2)
library(magrittr)
source("fsm.R")

#dsent <- genr_dt(dtm_flat,big = F)
dsent <- readxl::read_excel(goldfile,sheet = 1) %>% as.data.table %>% genr_dt(big = F)

pos <- dtm_flat$upos %>% unique()
features <- dtm_flat$feats %>% unique()


ui <- fluidPage( h1("Ripple Down Rules - Dashboard"),
                 sidebarLayout(
                   sidebarPanel(sliderInput("count",
                                "Number of words per sentence",
                                step=3,
                                min=3,
                                max=100,
                                value=10,ticks = T),
                                textInput("pattern",
                                          "Keywords",
                                            ""
                                          ),
                                actionButton("now","GO"),
                                selectInput("pos",
                                            "Part of Speech",
                                            choices = pos,
                                            multiple = T,
                                            selected = "VERB"
                                            ),
                                selectInput("feat",
                                            "Feature of the POS",
                                            choices = features,
                                            selected = "Number=Sing|Tense=Pres|VerbForm=Part",
                                            multiple = T
                                            )
                                
                   ),
                   mainPanel(
                     h2("F1 score"),
                     plotOutput("f1plot"),
                     h2("Confusion Matrix"),
                     plotOutput("confusion")
                   )
                 )
)
    

      
server <- function(input, output, session) {
    d1 <- eventReactive(input$now,fpos(dtm_flat,pos = paste0(input$pos,collapse = "|"),featpat = input$feat,id="pos"))
    goldpos(dt = d1(),range = sample(1:nrow(d1()),size = 500,replace = F))
  observe({
    fpat(d1(),pat = input$pattern,id = "pat")
    fno(d1(),n = input$count,id = "words")
    f1 <- f1group(d1(),c("pos","pat","words"),filewise = F)
    
    mf1 <- suppressWarnings(
      melt(f1,id.vars = "rule")[1:3,c(2,3)]
    )
    confm <- suppressWarnings(
      melt(f1,id.vars = "rule")[4:7,c(2,3)]
    )
    output$f1plot <- renderPlot({
      ggplot(mf1,aes(variable,value)) + geom_col(aes(fill = variable))
    })
    output$confusion <- renderPlot({
      ggplot(confm,aes(variable,value)) + geom_col(aes(fill = variable))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

