library(shiny)
library(sqldf)
library(shinyjs)


ui <- fluidPage(
  checkboxGroupInput(
    "titles",
    "Tick the titles you want",
    c(
      "IT ",
      "Data",
      "Information",
      "Tech",
      "Arch",
      "Human Resources",
      "Marketing",
      "Finance"
    )
  ),
  checkboxGroupInput(
    "regions",
    "Tick the regions you want",
    c("ANZ", "EMEA", "SEA", "DACH", "NAMER")
  ),
  
  fileInput(
    'target_upload',
    'Upload domain here',
    accept = c('text/csv',
               'text/comma-separated-values',
               '.csv')
  ),
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
      
      
      #this is the case where there is only one title selected
      if (length(input$titles) == 1 && input$titles != "")
      {
        titlesparses <-
          paste0(" where title like '%", input$titles, "%'")
      } else if (length(input$titles) > 1)
        
        #this is where there is more than one title selected
        
      {
        for (i in 1:length(input$titles))
        {
          #this is the first iteration of sets of titles which are larger than 2
          if (i == 1 && length(input$titles) != 1)
          {
            titlesparses[i] <-
              paste0(" where title like '%", input$titles[i], "%' OR ")
          }
          
          #these iterations are between 1 and end
          else if (i + 1 < length(input$titles))
          {
            titlesparses[i] <-
              paste0("title like '%", input$titles[i], "%' OR ")
          }
          
          #this is last iteration of sets of titles which are larger than 2
          else if (i == length(input$titles) && i > 1)
          {
            titlesparses[i] <- paste0("title like '%", input$titles[i], "%'")
          }
        }
      }
      
      #combing all elements int he list to one string
      titlescombined = paste0(titlesparses, collapse  = "")
      
      
      
      
      
      q1 <-
        sqldf(
          paste0(
            "select distinct first, last, title, company, region, email from q0",
            titlescombined
          )
        )
      
      
      
      #region
      
      #putting the region inputs into a vector
      regionsparses <- character(length(input$regions))
      
      #loop for concatentating the complementary sql code to the inputs
      
    
      #this is the case where there is only one regions selected
      if (length(input$regions) == 1 && input$regions != "")
      {
        regionsparses <-
          paste0(" where region like '%", input$regions, "%'")
      }else if (length(input$regions) > 1)
      #this is where there is more than one regions selected
      {
        for (i in 1:length(input$regions))
        {
          #this is the first iteration of sets of regions which are larger than 2
          if (i == 1 && length(input$regions) != 1)
          {
            regionsparses[i] <-
              paste0(" where region like '%", input$regions[i], "%' OR ")
          }
          
          #these iterations are between 1 and end
          else if (i + 1 < length(input$regions))
          {
            regionsparses[i] <-
              paste0("region like '%", input$regions[i], "%' OR ")
          }
          
          #this is last iteration of sets of regions which are larger than 2
          else if (i == length(input$regions) && i > 1)
          {
            regionsparses[i] <-
              paste0("region like '%", input$regions[i], "%'")
          }
        }
      }
      
      #combing all elements int he list to one string
      regionscombined = paste0(regionsparses, collapse  = "")
      
      
      
      
      
      q2 <-
        sqldf(
          paste0(
            "select distinct first, last, title, company, region, email from q1",
            regionscombined
          )
        )
      
      
      
      #domains
      domains <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
        {
          df = data.frame(domain="")
        }
        else if (!is.null(inFile))
        {
          df <- read.csv(inFile$datapath, header = TRUE, sep = ",")
        }
        
        domainsparses <- character(length(df$domain))
        
        #loop for concatentating the complementary sql code to the inputs
        
        
        #this is the case where there is no domain in df (i.e. no domain csv uploaded)
       
        #this is the case where there is only one domain selected
        if (length(df$domain) == 1 && df$domain != "")
        {
          domainsparses <-
            paste0(" where email like '%", "@" , df$domain, "%'")
        }else if (length(df$domain) > 1)
          #this is where there is more than one domain selected
        {
          for (i in 1:length(df$domain))
          {
            #this is the first iteration of sets of domain which are larger than 2
            if (i == 1 && length(df$domain) != 1)
            {
              domainsparses[i] <-
                paste0(" where email like '%", "@" , df$domain[i], "%' OR ")
            }
            
            #these iterations are between 1 and end
            else if (i + 1 < length(df$domain))
            {
              domainsparses[i] <-
                paste0("email like '%", "@" , df$domain[i], "%' OR ")
            }
            
            #this is last iteration of sets of domain which are larger than 2
            else if (i == length(df$domain) && i > 1)
            {
              domainsparses[i] <-
                paste0("email like '%", "@" , df$domain[i], "%'")
            }
          }
        }
        #combing all elements int he list to one string
        domaincombined = paste0(domainsparses, collapse  = "")
        
        return(domaincombined)
        
      })
      
      
      q3 <-
        sqldf(
          paste0(
            "select distinct first, last, title, company, region, email from q2 ",
            domains()
          )
        )

      write.csv(q3, file)
    }
  )
}





shinyApp(ui, server)
