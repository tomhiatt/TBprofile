###########################
# USER INTERFACE
# Tom Hiatt
# MDR-TB and other tabs of TB profile
# Updated: 26 Sep 2014
# Box 4 page 42
###########################

# library(shiny)

options(stringsAsFactors=FALSE)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("TB dashboard"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(      
      uiOutput("areaSelect"),
      
      br(),
      h5("Use your own data", title="1. Click the <<Save Data>> button below to download the dataset as a .csv file. 2. Replace everything below the header row with your own data. 3. Save the file in .csv format. 4. Click <<Choose File>> and Upload your file."), 
      p("Download"), 
      
      downloadButton("template", "Template"),
      br(),
      br(),
      
      fileInput("filled.template", "Upload"),
      
      #       radioButtons("source", "Which data would you like to use?", choices = list(`Original data`="orig", `Uploaded data`="uploaded")),
      
      a("Feedback welcome!", href="https://docs.google.com/forms/d/117_mQtABIm0qGE2IJuPaN4V6hMEJP0zkbLLCXS1ahyc/viewform", title="Please send any comments or suggestions."),
      br(),
      br(),
      
      "Data source: ", a("WHO", href="http://www.who.int/tb/data", title="World Health Organization"),   
      br(),
      br()
      
    ),
    
    # Show plots
    mainPanel(
      tabsetPanel(id = "active", type = "tabs",
                  
                  #                   tabPanel("test", htmlOutput("view")),
                  
                  
                  tabPanel("MDR-TB profile",
                           plotOutput("profileplots", 
                                      width = "auto", 
                                      height = "800px")
                           
                  ),
                  tabPanel("MDR-TB comparison", htmlOutput("compareplot")),
                  tabPanel("UUTBD", 
                           plotOutput("violet1"),
                           plotOutput("violet2"),
                           plotOutput("violet3", height="500px"),
                           plotOutput("violet4", height="600px"))
      )
    )
    
  )
  
))
