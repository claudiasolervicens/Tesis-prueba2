library(shinydashboard)
library(shiny)
library(readxl)
library(highcharter)
library(dplyr)
library(d3wordcloud)
library(tm)
library(dplyr)
library(ggplot2)

DataApp<-readRDS("07012018DataMaestra.rds")

Keyword<-function (StringVector)
{
  StringVector<-Corpus(VectorSource(StringVector))
  
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  
  vector_cleaning<-c("!","#","$","%","&","/", "•", "‘", "’", "-","<", ">")
  
  for (i in 1:length(vector_cleaning))
  {
    StringVector <- tm_map(StringVector, toSpace, vector_cleaning[i])
  }
  
  
  #remover signos de  puntuación
  StringVector <- tm_map(StringVector, removePunctuation)
  
  #remover número
  StringVector <- tm_map(StringVector, removeNumbers)
  
  StringVector <-tm_map(StringVector,content_transformer(tolower))
  
  #cargar la lista de stopwords
  stopwords <- c(stopwords(kind = "english"))
  
  #remover stopwords
  StringVector <- tm_map(StringVector, removeWords, stopwords)
  
  #remover whitespace
  StringVector <- tm_map(StringVector, stripWhitespace)
  
  #cargar la lista de generic terms
  genericterms <- readLines("genericterms.txt")
  
  #remover generic terms
  StringVector <- tm_map(StringVector, removeWords, genericterms)
  
  
  dtm <- DocumentTermMatrix(StringVector)
  
  #agrupar sumando por columnas
  freq <- colSums(as.matrix(dtm))
  
  #el largo debe ser igual a total de términos
  length(freq)-ncol(dtm)==0
  
  #ordenar por frecuencia (descendiente)
  ord <- order(freq,decreasing=TRUE)
  
  #Enlistar los términos por frecuencia
  data.frame(freq[ord])
  
}




# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="Aplicacion Análisis Abstract Médicos", titleWidth=400),
  dashboardSidebar(
    selectInput("variableMetodo","Seleccion Método:",
                c("LDA"="lda_10",
                  "Kmeans"="Cluster_15",
                  "Heuristica propia"="Categoria_1"
                )
    ),
    selectInput("variable", "Seleccion:",
                c("Todos" = 0,
                  "1" = 1,
                  "2" = 2)),
    
    selectInput("variableAno", "Seleccion Temporal:",
                c("Todos" = 0,
                  "2005" = 2005,
                  "2006" = 2006)),

    textOutput("seleccion"),
    
    textOutput("text")
    
  ),
  dashboardBody(
    
    plotOutput("plot"),
    d3wordcloudOutput("wordcloud"),
    dataTableOutput('tabla')
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  

  
  
  dataset <- reactive({
    
    l<-list()
    
    if(input$variable=="0")
    {
      
      A<-DataApp
      
      eval(parse(text=paste0("A%>%
                 group_by(Ano,",input$variableMetodo,")%>%
                             summarise(abs_tot=n())->DataGraph;
                             "
        
      )))
      # A%>%
      #   group_by(Ano,lda_10)%>%
      #   summarise(abs_tot=n())->DataGraph;

      
    }
    else
    {
      A<-subset(DataApp, lda_10 %in% input$variable)
      eval(parse(text=paste0("A%>%
                 group_by(Ano,",input$variableMetodo,")%>%
                             summarise(abs_tot=n())->DataGraph;
                             "
                             
      )))
      
    }
    

    l[["DataNormal"]]<-A
    l[["DataGraph"]]<-DataGraph
    l
  })
  
  
  output$seleccion<-renderText(
    {  
      
      invalidateLater(1,session)
      
      bool<-FALSE
      if(input$variable=="0")
      {
        bool<-TRUE
        
      }
      else
      {
        bool<-FALSE
      }
      
      paste("Ha escogido: ", input$variable," que tiene tipo : ", typeof(input$variable), "  ", bool)
    }
  )
  
  
  
  output$plot<- renderPlot(
    {
      
      invalidateLater(2000,session)
      
      canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
      legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
      

      ggplot(dataset()$DataGraph,aes(x=Ano,y=abs_tot),stat="identity") + geom_col(aes_string(fill = input$variableMetodo))  
      
      
        # hchart(dataset()$DataGraph,'column',hcaes(x=Ano,y=abs_tot, group=input$variableMetodo ))%>%
        # hc_title(text='Publicaciones')%>%
        #               hc_credits(text='Facultad de Medicina Universidad de Chile',href='http://www.medicina.uchile.cl', enabled=TRUE)%>%
        #               hc_xAxis(title = list(text = 'Año'))%>%hc_yAxis(title = list(text = 'N° de Artículos'))%>%
        #               hc_add_theme(hc_theme_gridlight())%>%
        #               hc_plotOptions(
        #               column=list(
        #               stacking='normal'
        #               ), series=list(animation=FALSE),
        #               events = list(click = canvasClickFunction, legendItemClick = legendClickFunction)
        #               )
                      

        
      
    }

  )
  
  
  
  makeReactiveBinding("outputText")
  
  observeEvent(input$canvasClicked, {
    outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".") 
  })
  
  observeEvent(input$legendClicked, {
    outputText <<- paste0("You clicked into the legend and selected series ", input$legendClicked, ".")
  })
  
  output$text <- renderText({
    outputText      
  })
  
  
  output$wordcloud<-renderD3wordcloud(
    {
      # B<-newData()
      # A<-newData()
      # if(input$variable=="0")
      # {
      #   B<-DataApp
      # }
      # else
      # {
      #   B<-subset(DataApp, lda_10 %in% input$variable)
      # }
      
      Nombre_Frecuencia<-Keyword(dataset()$DataNormal$Abstract)
      Nombre_Frecuencia$palabras<-row.names(Nombre_Frecuencia)
      Nombre_Frecuencia%>%top_n(300,wt=freq.ord.)->graficar_palabras
      d3wordcloud(graficar_palabras$palabras, graficar_palabras$freq.ord.)

    }
  )
  
  output$tabla<-renderDataTable(
    {
      if(input$variable=="0")
      {
        C<-DataApp
      }
      else
      {
        
        
        C<-subset(DataApp, lda_10 %in% input$variable)
      }
      
      C
    }
  )
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

