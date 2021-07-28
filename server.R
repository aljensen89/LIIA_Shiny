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
          select("study_id","demo_first_name","demo_last_name","next_appt_final","next_appt_date_format") %>%
          na.exclude("next_appt_final") 
      }
      
      if(input$type_report=="Active Enrollment"){
        data %<>%
          select("study_id","demo_first_name","demo_last_name","status") %>%
          filter(status == "Actively Enrolled")
      }
      
      if(input$type_report=="Baseline and Follow-Up Status"){
        data %<>%
          select("study_id","demo_first_name","demo_last_name","base_class","fu_class") %>%
          drop_na("base_class")
      }
      
      if(input$type_report=="Consensus Conference"){
        data %<>%
          select("study_id","demo_first_name","demo_last_name","cons_conf_due") %>%
          na.exclude("cons_conf_due")
      }
      
      if(input$type_report=="Optional Measures"){
        data %<>%
          select("demo_first_name","demo_last_name","demo_phone","demo_email",
                 "consent_audio_rp","consent_audio_me","consent_future","consent_b12_results") %>%
          na.exclude("consent_audio_rp","consent_audio_me","consent_future","consent_b12_results") %>%
          dplyr::rename(First_Name=demo_first_name,Last_Name=demo_last_name,
                 Phone_Number=demo_phone,Email_Address=demo_email,
                 Audio_Research_Purposes_Consent=consent_audio_rp,
                 Audio_Medical_Education_Consent=consent_audio_me,
                 Future_Study_Contact=consent_future,
                 B12_Results_Contact=consent_b12_results)
      }
      
      if(input$type_report=="Patient Drop Out/Ineligibility"){
        data %<>%
          select("study_id","demo_first_name","demo_last_name","status","comments") %>%
          filter(status %notin% c("Participant death","Actively Enrolled",NA))
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
        
        ## Vector of variables to summarize - overall
        myVars<-c("curr_age","demo_educ_yrs","demo_ethnicity","demo_handedness",
                        "demo_race_Asian","demo_race_Black","demo_race_Cauc","demo_race_PacIsl",
                        "demo_race_NatAmer","demo_race_Unkn","demo_race_NoAns")
        
        ## Create a TableOne object
        tabone_overall<-CreateTableOne(vars=myVars,data=data,test=FALSE)
        tabone_overall_frame<-print(tabone_overall,showAllLevels=TRUE,test=FALSE)
        row_nms_overall<-rownames(tabone_overall_frame)
        tabone_overall_final<-cbind(row_nms_overall,as.data.frame(tabone_overall_frame))
        rownames(tabone_overall_final)<-c()
        names(tabone_overall_final)[names(tabone_overall_final)=="row_nms"]<-""
        
        tabone_bysex<-CreateTableOne(vars=myVars,strata="demo_sex",data=data,test=FALSE)
        tabone_bysex_frame<-print(tabone_bysex,showAllLevels=TRUE,test=FALSE)
        row_nms_bysex<-rownames(tabone_bysex_frame)
        tabone_bysex_final<-cbind(row_nms_bysex,as.data.frame(tabone_bysex_frame))
        rownames(tabone_bysex_final)<-c()
        names(tabone_bysex_final)[names(tabone_bysex_final)=="row_nms"]<-""
        
        
        tabone_combined<-cbind(tabone_overall_final,tabone_bysex_final[,3:4])
        colnames(tabone_combined)<-c("Variable Name","Variable Level","Overall","Female Sex","Male Sex")
        tabone_combined[,1]<-c("n","Current Age (mean (SD))","Education Years (mean (SD))",
                                "Ethnicity (%)","","","","Handedness (%)","","","","Asian Race (%)",
                                "","Black or African American Race (%)","","White or Caucasian Race (%)",
                                "","Native Hawaiian or Pacific Islander Race (%)","",
                                "Alaska Native or American Indian Race (%)","","Unknown Race (%)","",
                                "Prefer not to Answer Race (%)","")
        tabone_combined<-tabone_combined[tabone_combined$`Variable Level` %notin% c("No","Prefer not to Answer"),]
        data<-tabone_combined
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
