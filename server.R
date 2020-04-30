# Alexandria Jensen
# 23 September 2019

# This is the server portion of a shiny app for the LIIA RECap database

list.of.packages<-c('base','tidyverse','plyr','magrittr','qwraps2','tableone',
                    'shiny','shinyjs','shinythemes','REDCapExporter')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(plyr)
library(magrittr)
library(qwraps2)
library(tableone)
library(REDCapExporter)


source("helpers.R") # Have the helper functions available

shinyServer(function(input,output,session) {
  # ======= BUILDING THE DATASET FROM REDCAP ======= #
  
  # Use the REDCap API token input and create initial dataset
  LIIA_data<-reactive({
    
    # Add dependency on the update button (only update when clicked)
    input$goBtn
    
    # Create the initial dataset
    getData(as.character(input$API_token))
  })
  
  # ======= MANIPULATE THE DATA ======= #
  
  # The dataset to show/summarize, which is the raw data after filtering based
  # on user inputs
  LIIA_abb<-reactive({
    
    # Add dependency on the update button (only update when button is clicked)
    input$updateBtn
    
    data<-LIIA_data()
    
    # Add all the filters to the data based on the user inputs
    # Wrap in an isolate() so that the data won't update every time an input is changed
    isolate({
      
      # Filter based on radio button chosen
      if(input$type_report=="Upcoming Appts"){
        data %<>%
          select("study_id","demo_first_name","demo_last_name","next_appt") %>%
          filter(is.na("next_appt"))
      }
      
      if(input$type_report=="Patient Enrollment"){
        data %<>%
          select("study_id","demo_first_name","demo_last_name","status","comments") %>%
          filter(status != "Participant death")
      }
      
      if(input$type_report=="Patient Death"){
        data %<>%
          select("study_id","demo_first_name","demo_last_name","status","death_date") %>%
          filter(status == "Participant death")
      }
      if(input$type_report=="Demographics"){
        data %<>%
          filter(status=="Actively Enrolled") %>%
          select("curr_age","demo_sex","demo_ethnicity","demo_handedness","demo_educ_yrs",
                 "demo_race_Asian","demo_race_Black","demo_race_Cauc","demo_race_PacIsl",
                 "demo_race_NatAmer","demo_race_Unkn","demo_race_NoAns")
        
        ## Vector of variables to summarize
        myVars<-c("curr_age","demo_educ_yrs","demo_sex","demo_ethnicity","demo_handedness",
                  "demo_race_Asian","demo_race_Black","demo_race_Cauc","demo_race_PacIsl",
                  "demo_race_NatAmer","demo_race_Unkn","demo_race_NoAns")
        
        ## Vector of categorical variables that need transformation
        catVars<-c("demo_sex","demo_ethnicity","demo_handedness","demo_race_Asian",
                   "demo_race_Black","demo_race_Cauc","demo_race_PacIsl",
                   "demo_race_NatAmer","demo_race_Unkn","demo_race_NoAns")
        
        ## Create a TableOne object
        tabone<-CreateTableOne(vars=myVars,data=data,factorVars=catVars)
        tabone_frame<-print(tabone,showAllLevels=TRUE)
        row_nms<-rownames(tabone_frame)
        tabone_final<-cbind(row_nms,as.data.frame(tabone_frame))
        rownames(tabone_final)<-c()
        names(tabone_final)[names(tabone_final)=="row_nms"]<-""
        data<-tabone_final
      }
    })
    data
  })
  
  # ======= SHOW DATA IN A TABLE ======= #
  
  # Show the data in a table
  output$dataTable<-renderTable(
    {
     LIIA_abb()
    },
    include.rownames=FALSE
  )
  
  # Allow user to download the data, simply save as csv
  output$downloadData<-downloadHandler(
    filename=function(){
      "LIIA_data.csv"
    },
    
    content=function(file){
      write.table(x=LIIA_abb(),file=file,quote=FALSE,row.names=FALSE)
    }
  )
})