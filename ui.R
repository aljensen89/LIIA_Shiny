# Alexandria Jensen
# 23 September 2019

# This is the ui portion of a shiny app that provides reports and descriptive statistics
# from data within the LIIA REDCap database

fluidPage(theme=shinythemes::shinytheme("united"),
  titlePanel("Reports from LIIA REDCap Database"),
    
  # Sidebar - filters for the data
  sidebarLayout(
    sidebarPanel(
      helpText("Create tables for LIIA study using REDCap API calls"),
        
      # Input for API Token
      passwordInput("API_token","REDCap API Token"),
      
      hr(),  
      # Go button to start REDCap API call
      actionButton("goBtn", "Call REDCap API"),
      
      hr() , 
      # Which type of report to be created
      radioButtons("type_report","Type of Report to be Created",
                    choices=c("Upcoming Appts","Demographics","Active Enrollment",
                              "Baseline and Follow-Up Status","Consensus Conference",
                              "Optional Measures","Patient Drop Out/Ineligibility","Patient Death"),
                    selected="Upcoming Appts"),
      hr(),
      # Button to update the data
      actionButton("updateBtn", "Update Data")
        
    ),
      
    mainPanel(
      downloadButton("downloadData","Download Table"),
      br(),br(),
        
      tableOutput("dataTable")
    )
  )
)