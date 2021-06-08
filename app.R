library(shiny)
library(sqldf)


ui <- fluidPage(
  checkboxGroupInput(
    "titles",
    "Tick the titles you want",
    c("IT ", "Data", "Information", "Tech", "Arch", "Human Resources", "Marketing", "Finance")),
  checkboxGroupInput(  
    "regions", 
    "Tick the regions you want",
    c("ANZ", "EMEA", "SEA", "DACH", "NAMER")),
  
  fileInput('target_upload', 'Upload domain here',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )),
  DT::dataTableOutput("sample_table"),
  

  
  downloadButton("downloadData", "Download"),
  
  
)




server <- function(input, output) {

    output$downloadData <- downloadHandler(
    filename = function() {
      paste0("output.csv")
    },
    content = function(file) {
      
      
      
      #local database
      setwd("~/Desktop/rserver")
      
      
      q0 <- read.csv("crm2019-2020legit.csv")
      
      #titles
      
      #putting the title inputs into a vector
      titlesparses <- character(length(input$titles))
      
      #loop for concatentating the complementary sql code to the inputs
      for (i in 1:length(input$titles))
      {
        if (i < length(input$titles))
        {
          titlesparses[i] <- paste0("title like '%", input$titles[i], "%' OR ")
        }
        else if (i == length(input$titles))
        {
          titlesparses[i] <- paste0("title like '%", input$titles[i], "%'")
        }
      }
      #combing all elements int he list to one string
      titlescombined = paste0(titlesparses, collapse  = "")
      
      
      
      
      q1 <-
        sqldf(
          paste0(
            "select distinct first, last, title, company, region, email from q0 where ",
            titlescombined
          )
        )
      
      
      #region
      
      regionsparses <- character(length(input$regions))
      
      #loop for concatentating the complementary sql code to the inputs
      for (i in 1:length(input$regions))
      {
        if (i < length(input$regions))
        {
          regionsparses[i] <- paste0("region like '%", input$regions[i], "%' OR ")
        }
        else if (i == length(input$regions))
        {
          regionsparses[i] <- paste0("region like '%", input$regions[i], "%'")
        }
      }
      #combing all elements int he list to one string
      regionscombined = paste0(regionsparses, collapse  = "")
      
      
      
      
      q2 <-
        sqldf(
          paste0(
            "select distinct first, last, title, company, region, email from q1 where ",
            regionscombined
          )
        )
      
      
      
      
      #domains
      domains <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
          return(NULL)
        df <- read.csv(inFile$datapath, header = TRUE,sep = ",")
        
        
        domainsparses <- character(length(df$domain))
        
        #loop for concatentating the complementary sql code to the inputs
        for (i in 1:length(df$domain))
        {
          if (i < length(df$domain))
          {
            domainsparses[i] <- paste0("email like '%", "@", df$domain[i], "%' OR ")
          }
          else if (i == length(df$domain))
          {
            domainsparses[i] <- paste0("email like '%", "@", df$domain[i], "%'")
          }
        }
        #combing all elements int he list to one string
        domaincombined = paste0(domainsparses, collapse  = "")
        
        return(domaincombined)
        
      })

      
      q3 <-
        sqldf(
          paste0(
            "select distinct first, last, title, company, region, email from q2 where ",
            domains()
          )
        )
            
      
      

      write.csv(q3, file)
    }
  )
}





shinyApp(ui, server)


