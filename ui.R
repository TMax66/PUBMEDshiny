
library(shiny)
library(RISmed)
library(DT)
library(dplyr)

ui<-navbarPage("ExplorePubMed",
               tabPanel("Query PubMed",
                        fluidPage(
                          
                          fluidRow(
                            column(12,div(align="center",
                                          textInput( "key","Keyord(s)",width = 500)))),
                          
                          fluidRow(
                            column(6,div(align="center",
                                         textInput("date1", label = "From",value="",width = 100))),
                            
                            column(6,div(align="center",
                                         textInput("date2", label = "To",width = 100)))        
                            
                          ),
                          
                          fluidRow( column(12, div(align="center",
                                                   actionButton("goButton","Cerca")))),
                          hr(),
                          
                          fluidRow(column(12, div(align="center",
                                                  verbatimTextOutput("query")))),
                          hr(),
                          
                          fluidRow(
                            column(6, div(align='center', 
                                           DT::dataTableOutput("tabella") )),
                            column(6, div(align='center',
                                          plotOutput("barPlot")))
                          )
                          
                          
                          
                     ))
)
               #, 
               # 
               # tabPanel("Abstract text mining",
               #          fluidPage(
               #            fluidRow(
               #              column(6, div(align='center',
               #                            plotOutput("barPlot")))
               #            )
               #          )
               #          
               #          
               #          
               #          
               #          
               #          ),
               # tabPanel("Journal analysis")
               # )
               # 
               #     
               #     
               #    
