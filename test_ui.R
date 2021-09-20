# Alexandria Jensen
# 23 September 2019

# This is the ui portion of a shiny app that provides reports and descriptive statistics
# from data within the LIIA REDCap database

test_ui<-navbarPage(title="LIIA REDCap Database Shiny App",theme=shinythemes::shinytheme("spacelab"),inverse=FALSE,
           #First tab - general reports
           tabPanel("Reports",
                    fluidPage(
                              titlePanel("Reports from LIIA REDCap Database"),
                              
                              # Sidebar - filters for the data
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  # Input for API Token
                                  passwordInput("API_token","REDCap API Token"),
                                  
                                  hr(),  
                                  # Go button to start REDCap API call
                                  actionButton("goBtn", "Call REDCap API"),
                                  
                                  hr() , 
                                  # Which type of report to be created
                                  radioButtons("type_report","Type of Report to be Created",
                                               choices=c("Upcoming Appts","Demographics","Active Enrollment","Participant Visit Stats",
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
                    )),
            tabPanel("Survey Invitation Log",
                     fluidPage(
                               titlePanel("Survey Invitation Reminders"),
                               
                               #Sidebar - PRA to import .csv file from REDCap
                               sidebarLayout(
                                 sidebarPanel(
                                   fileInput('file1', 'Choose CSV File',
                                             accept=c('text/csv', 
                                                      'text/comma-separated-values,text/plain', 
                                                      '.csv')),
                                   
                                   # added interface for uploading data from
                                   # http://shiny.rstudio.com/gallery/file-upload.html
                                   tags$br(),
                                   checkboxInput('header', 'Header', TRUE),
                                   radioButtons('sep', 'Separator',
                                                c(Comma=',',
                                                  Semicolon=';',
                                                  Tab='\t'),
                                                ','),
                                   radioButtons('quote', 'Quote',
                                                c(None='',
                                                  'Double Quote'='"',
                                                  'Single Quote'="'"),
                                                '"')
                                   
                                 ),
                                 mainPanel(
                                   downloadButton("downloadData","Download Table"),
                                   br(),br(),
                                   
                                   tableOutput("dataTable")
                                 )
                               )
                     ))
)

server <- function(input, output) {}

shinyApp(ui = test_ui, server = server)
