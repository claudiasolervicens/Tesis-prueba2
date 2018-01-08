library("shiny")
library("highcharter")

ui <- shinyUI(
  fluidPage(
    column(width = 8, highchartOutput("hcontainer", height = "500px")),
    column(width = 4, dataTableOutput("tablecontainer"))
  )
)

server <- function(input, output) {      
  
  a <- data.frame(b = LETTERS[1:10], c = 11:20, d = 21:30, e = 31:40)
  
  output$hcontainer <- renderHighchart({      
    
    myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClicked', this.name);}")
    
    highchart() %>% 
      hc_xAxis(categories = a$b) %>% 
      hc_add_series(name = "c", data = a$c) %>%
      hc_add_series(name = "d", data = a$d) %>% 
      hc_add_series(name = "e", data = a$e) %>%
      hc_plotOptions(series = list(stacking = FALSE, events = list(click = myClickFunc))) %>%
      hc_chart(type = "column")
    
  })      
  
  output$tablecontainer <- renderDataTable({
    if(!is.null(input$hcClicked)){
      subset(a, , c("b", input$hcClicked))
    }
  })
}

shinyApp(ui = ui, server = server)