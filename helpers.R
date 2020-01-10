# Alexandria Jensen
# 23 September 2019

# Helper functions for the LIIA REDCap shiny app

library(REDCapExporter)
library(magrittr)
library(tidyverse)

getData<-function(redcap_api_token) {
  # Create the REDCap connection using the API Token provided
  Sys.setenv(REDCap_API_URI = "https://redcap.ucdenver.edu/api/")
  Sys.setenv(REDCap_API_TOKEN = as.character(redcap_api_token))
  
  records_raw <- export_content("record")
  myData <- as.data.frame(records_raw)
  
  metadata_raw <- export_content("metadata")
  metadata <- as.data.frame(metadata_raw)
  
  # Subsetting the data to make it more manageable
  records_keepVars<-c("study_id","demo_first_name","demo_last_name","demo_dob","demo_sex",
                      "demo_race___0","demo_race___1","demo_race___2","demo_race___3",
                      "demo_race___4","demo_race___5","demo_race___9","demo_ethnicity",
                      "demo_handedness","demo_educ_yrs","with_inelig_choice",
                      "with_inelig_dthdte","with_inelig_detail")
  
  # Records event: demographics and withdrawal/ineligibility
  rec_myData<-myData[records_keepVars]
  
  rec_myData<-rec_myData %>% 
    mutate_all(na_if,"")
  
  rec_myData<-rec_myData %>% 
    drop_na(study_id,demo_first_name,demo_last_name,demo_dob,demo_sex,
            demo_ethnicity,demo_handedness,demo_educ_yrs)
  
  # Screening event: screening/consent
  screen_keepVars<-c("study_id","consent_scrnfail","consent_scrnfail_det",
                      "consent_yesno")
  
  screen_myData<-myData[screen_keepVars]
  
  screen_myData<-screen_myData %>% 
    mutate_all(na_if,"")
  
  screen_myData<-screen_myData[!is.na(screen_myData$consent_scrnfail) | 
                                 !is.na(screen_myData$consent_yesno),]
  
  # Baseline event: innate immune history
  base_keepVars<-c("study_id","immune_date")
  
  base_myData<-myData[base_keepVars]
  
  base_myData<-base_myData %>% 
    mutate_all(na_if,"")
  
  base_myData<-base_myData %>%
    drop_na(study_id,immune_date)
  
  # Combining the records, screening, and baseline datasets
  myData_merge<-merge(rec_myData,base_myData,by="study_id",all.x=TRUE)
  myData_merge<-merge(myData_merge,screen_myData,by="study_id",all.x=TRUE)
  
  # Character to numeric
  myData_merge$demo_sex<-as.numeric(myData_merge$demo_sex)
  myData_merge$demo_ethnicity<-as.numeric(myData_merge$demo_ethnicity)
  myData_merge$demo_handedness<-as.numeric(myData_merge$demo_handedness)
  myData_merge$demo_educ_yrs<-as.numeric(myData_merge$demo_educ_yrs)
  myData_merge$consent_scrnfail<-as.numeric(myData_merge$consent_scrnfail)
  myData_merge$consent_yesno<-as.numeric(myData_merge$consent_yesno)
  myData_merge$with_inelig_choice<-as.numeric(myData_merge$with_inelig_choice)
  
  # New demographic variables
  myData_merge$demo_sex<-ifelse(myData_merge$demo_sex==1,"Female",
                          ifelse(myData_merge$demo_sex==0,"Male",
                                 ifelse(myData_merge$demo_sex==9,"Prefer not to Answer",NA)))
  
  myData_merge$demo_ethnicity<-ifelse(myData_merge$demo_ethnicity==1,"Not Hispanic or Latino/a",
                                ifelse(myData_merge$demo_ethnicity==2,"Hispanic or Latino/a",
                                       ifelse(myData_merge$demo_ethnicity==3,"Unknown",
                                              ifelse(myData_merge$demo_ethnicity==9,"Prefer not to Answer",NA))))
  
  myData_merge$demo_handedness<-ifelse(myData_merge$demo_handedness==0,"Right",
                                 ifelse(myData_merge$demo_handedness==1,"Left",
                                        ifelse(myData_merge$demo_handedness==2,"Ambidextrous",
                                               ifelse(myData_merge$demo_handedness==9,"Prefer not to Answer",NA))))
  # Creating new race categories
  myData_merge$demo_race_NatAmer<-as.factor(ifelse(myData_merge$demo_race___0==1,"Yes","No"))
  myData_merge$demo_race_Asian<-as.factor(ifelse(myData_merge$demo_race___1==1,"Yes","No"))
  myData_merge$demo_race_Black<-as.factor(ifelse(myData_merge$demo_race___2==1,"Yes","No"))
  myData_merge$demo_race_Cauc<-as.factor(ifelse(myData_merge$demo_race___3==1,"Yes","No"))
  myData_merge$demo_race_PacIsl<-as.factor(ifelse(myData_merge$demo_race___4==1,"Yes","No"))
  myData_merge$demo_race_Unkn<-as.factor(ifelse(myData_merge$demo_race___5==1,"Yes","No"))
  myData_merge$demo_race_NoAns<-as.factor(ifelse(myData_merge$demo_race___9==1,"Yes","No"))
  
  # Creating a new death date variable
  myData_merge$death_date<-as.character(as.Date(myData_merge$with_inelig_dthdte))
  
  # Dropping old race and death date variables
  drop_rcdth_vars<-names(myData_merge) %in% c("demo_race___0","demo_race___1",
                                             "demo_race___2","demo_race___3",
                                             "demo_race___4","demo_race___5",
                                             "demo_race___9","with_inelig_dthdte") 
  myData_final<-myData_merge[!drop_rcdth_vars]
  
  myData_final$curr_age<-round(as.numeric(difftime(Sys.Date(),myData_final$demo_dob,
                                                   units="days"))/364.25,2)
  myData_final$time_diff<-difftime(Sys.Date(),myData_final$immune_date,units="days")
  myData_final$next_appt<-ifelse(myData_final$time_diff>=168 & 
                                   myData_final$time_diff<=196,"6 Month Survey",
                         ifelse(myData_final$time_diff>=350 & 
                                  myData_final$time_diff<=378, "12 Month Survey",
                                ifelse(myData_final$time_diff>=532 & 
                                         myData_final$time_diff<=560, "18 Month Survey",
                                       ifelse(myData_final$time_diff<=638 & 
                                                myData_final$time_diff>=758,"2 Year Follow Up",NA))))
  
  myData_final$status<-ifelse(is.na(myData_final$with_inelig_choice) & myData_final$consent_scrnfail==0,
                                    "Actively Enrolled",
                                    ifelse(myData_final$consent_scrnfail==1,
                                           "Screen Fail",
                                           ifelse(myData_final$with_inelig_choice==1,
                                                  "Study Withdrawal",
                                                  ifelse(myData_final$with_inelig_choice==2,
                                                         "Study Ineligibility",
                                                         ifelse(myData_final$with_inelig_choice==3,
                                                                "Participant Death",NA)))))
  
  myData_final$comments<-ifelse(!is.na(myData_final$with_inelig_detail),myData_final$with_inelig_detail,
                                ifelse(!is.na(myData_final$consent_scrnfail_det),myData_final$consent_scrnfail_det,NA))
  
  # Final dataset
  myData_final
}
