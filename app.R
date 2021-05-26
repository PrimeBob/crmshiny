library(shiny)
library(sqldf)


ui <- fluidPage(
  checkboxGroupInput(
    "titles",
    "Tick the titles you want",
    c("it", "marketing", "finance", "data", "human resources")
  ),
  checkboxGroupInput("region", "Tick the regions you want", c("ANZ", "EMEA", "SEA")),
  checkboxGroupInput(
    "seniority",
    "Tick the seniority you want",
    c("Chief", "Director", "VP", "Senior", "Head")
  ),
  downloadButton("downloadData", "Download"),
)




server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("output.csv")
    },
    content = function(file) {
     
      
      #local database
      q1 <- read.csv("crm2019-2020legit.csv")
      
      #query on local database, pasting parts of the query as string
      q2 <-
        sqldf(
          paste0(
            "select distinct first, last, title, company, email from q1 where title like '%",
            input$titles,
            "%'"
          )
          
          
        )
      write.csv(q2, file)
    }
  )
}





shinyApp(ui, server)
