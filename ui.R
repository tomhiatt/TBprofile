###########################
# USER INTERFACE
# Tom Hiatt
# MDR-TB tab of TB profile
# Updated: 26 Sep 2014
###########################

# library(shiny)

options(stringsAsFactors=FALSE)

# load("dta.Rdata")
# dtao <- subset(dta, year==max(dta$year), c(admin0, admin1, admin2, admin2code))
# dtan <- rbind(data.frame(admin2=unique(dtao$admin0), 
#                          admin2code=unique(dtao$admin0)), 
#               data.frame(admin2=sort(unique(dtao$admin1)), 
#                          admin2code=sort(unique(dtao$admin1))), 
#               dtao[c("admin2", "admin2code")])
# Encoding(dtan$admin2) <- "latin1"
# ls.code <- as.list(dtan$admin2code)
# names(ls.code) <- sapply(ls.code, function(x) dtan[dtan$admin2code==x,"admin2"])
# ls.admin2 <- as.list(dtan$admin2)
# names(ls.admin2) <- sapply(ls.admin2, function(x) dtan[dtan$admin2==x,"admin2code"])

shinyUI(fluidPage(

  # Application title
  titlePanel("TB dashboard"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(      
#       selectInput("a2select", "Select areas to combine:", ls.code, selected=ls.code[7], width="400px", multiple=TRUE),
      uiOutput("areaSelect"),
      
      br(),
      p(HTML("<u>How?</u>"), title="1. Click the <<Save Data>> button below to download the dataset as a .csv file. 2. Replace everything below the header row with your own data. 3. Save the file in .csv format. 4. Click <<Choose File>> and Upload your file.", align="right"), h5("Use your own data"), 
      p("Download"), 
      
      downloadButton("template", "Template"),
      br(),
      br(),
      
      fileInput("filled.template", "Upload"),
      
#       radioButtons("source", "Which data would you like to use?", choices = list(`Original data`="orig", `Uploaded data`="uploaded")),
      br(),
      
      
      br(),
      
      "Data source: ", a("WHO", href="http://www.who.int/tb/data", title="World Health Organization"),
      
      
      br(),
      br()
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "active", type = "tabs",
                  tabPanel("MDR-TB",
                           plotOutput("profileplots", 
                                      width = "auto", 
                                      height = "800px")
                           )
                  )
#       plotOutput("distPlot"),
#       textOutput("check"),
      
#       tableOutput("check2")
    )
  )
))
