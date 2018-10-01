
shinyServer(function(input, output) {
  
  word1<-eventReactive(input$goButton, {input$key})
  
  res<-reactive({
    d1<-input$date1
    d2<-input$date2
    
    res <- EUtilsSummary(word1(), 
                         type="esearch", 
                         db="pubmed", 
                         datetype='pdat', 
                         mindate=d1, maxdate=d2
                         #retmax=500
                         )
  })
  
  tabres<-reactive({
    fetch <- EUtilsGet(res(), type = "efetch", db = "pubmed")
    abstracts <- data.frame(title = fetch@ArticleTitle,
                            abstract = fetch@AbstractText, 
                            journal = fetch@Title,
                            DOI = fetch@PMID, 
                            year = fetch@YearPubmed)
    abstracts <- abstracts %>% 
                 mutate(abstract = as.character(abstract)) %>% 
                 select(title,journal, DOI, year)
    
    abstracts
  })
  
 #####stampa la query e il numero di articoli estratti 
  output$query <- renderPrint({  
    summary(res()) 
    })
  ####stampa tabella con articoli estratti#######
  
  output$tabella<-DT::renderDataTable({
    tabres()
  })
  
  ############Grafico del numero di articoli per anno!############
  
  output$barPlot <- renderPlot({

    # fetch <- EUtilsGet(res(), type = "efetch", db = "pubmed")
    #
    #   count<-table(YearPubmed(fetch))
    #   count<-as.data.frame(count)
    #   names(count)<-c("Year", "Counts")
    #   cum <- data.frame(Year=count$Year, Counts=cumsum(count$Counts))
    #   cum$g <- "g"
    #   names(cum) <- c("Year", "Counts", "g")
    #
    #
    #   ggplot(count, aes(x=Year, y=Counts))+geom_bar(stat="identity")+
    #   ggtitle(paste("PubMed articles containing \'", word1(), "\' ", "= ", max(cum$Counts), sep="")) +
    #     ylab("Number of articles") +
    #     xlab(paste("Year \n Query date: ", Sys.time(), sep="")) +
    #     labs(colour="") +
    #     theme_bw()

    tally <- array()
    x <- 1
    for (i in 2000:2018){
      Sys.sleep(1)
      r <- EUtilsSummary(word1(), type='esearch', db='pubmed', mindate=i, maxdate=i)
      tally[x] <- QueryCount(r)
      x <- x + 1
    }

    names(tally) <- 2000:2018
    max(tally)
    barplot(tally, las=2, ylim=c(0,max(tally)), main="Number of PubMed articles")





  })
  
  
})


