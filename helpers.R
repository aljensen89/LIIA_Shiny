# Alexandria Jensen
# 23 September 2019

# Helper functions for the LIIA REDCap shiny app

`%notin%` <- Negate(`%in%`)

getData<-function(redcap_api_token) {
  # Create the REDCap connection using the API Token provided
  Sys.setenv(REDCap_API_URI = "https://redcap.ucdenver.edu/api/")
  Sys.setenv(REDCap_API_TOKEN = as.character(redcap_api_token))

  # Patch until REDCapExporter can be updated and pushed to CRAN.
  h <- curl::new_handle()
  h <- curl::handle_setform(h,
                            token = Sys.getenv("REDCap_API_TOKEN"),
                            content = "record",
                            format = "csv")
  records_raw <- curl::curl_fetch_memory(Sys.getenv("REDCap_API_URI"), handle = h)

  # records_raw <- export_content("record")
  # myData <- as.data.frame(records_raw)
  myData <- read.csv(text = rawToChar(records_raw$content))
  
  # metadata_raw <- export_content("metadata")
  # metadata <- as.data.frame(metadata_raw)
  h <- curl::handle_setform(h, content = "metadata")
  metadata_raw <- curl::curl_fetch_memory(Sys.getenv("REDCap_API_URI"), handle = h)
  metadata <- read.csv(text = rawToChar(metadata_raw$content))

  # Survey invitation log import
  # config_options <- NULL
  # uri<-Sys.getenv("REDCap_API_URI")
  # token<-Sys.getenv("REDCap_API_TOKEN")
  # 
  # post_body_6mo <- list(
  #   token = token,
  #   content = 'participantList',
  #   instrument = 'innate_immune_history_form_survey',
  #   event = '6_month_survey_fol_arm_1',
  #   format = 'csv')
  # survey_6m0_raw<-REDCapR:::kernel_api(uri,post_body_6mo,config_options)
  # survey_6m0<-as.data.frame(read.csv(text=survey_6m0_raw$raw_text,
  #                                    stringsAsFactors=FALSE))
  # 
  # post_body_12mo <- list(
  #   token = token,
  #   content = 'participantList',
  #   instrument = 'innate_immune_history_form_survey',
  #   event = '12_month_survey_fo_arm_1',
  #   format = 'csv')
  # survey_12m0_raw<-REDCapR:::kernel_api(uri,post_body_12mo,config_options)
  # survey_12m0<-as.data.frame(read.csv(text=survey_12m0_raw$raw_text,
  #                               stringsAsFactors=FALSE))
  # 
  # post_body_18mo <- list(
  #   token = token,
  #   content = 'participantList',
  #   instrument = 'innate_immune_history_form_survey',
  #   event = '18_month_survey_fo_arm_1',
  #   format = 'csv')
  # survey_18m0_raw<-REDCapR:::kernel_api(uri,post_body_18mo,config_options)
  # survey_18m0<-as.data.frame(read.csv(text=survey_18m0_raw$raw_text,
  #                                     stringsAsFactors=FALSE))
  
  # Subsetting the data to make it more manageable
  records_keepVars<-c("study_id","demo_first_name","demo_last_name","demo_dob","demo_phone",
                      "demo_email","demo_sex","demo_race___0","demo_race___1","demo_race___2",
                      "demo_race___3","demo_race___4","demo_race___5","demo_race___9",
                      "demo_ethnicity","demo_handedness","demo_educ_yrs","with_inelig_choice",
                      "with_inelig_dthdte","with_inelig_detail")
  
  # Records event: demographics and withdrawal/ineligibility
  rec_myData<-myData[records_keepVars]
  
  rec_myData<-rec_myData %>% 
    mutate_all(na_if,"")
  
  rec_myData<-rec_myData %>% 
    drop_na(study_id,demo_first_name,demo_last_name,demo_dob,demo_phone,demo_email,
            demo_sex,demo_ethnicity,demo_handedness,demo_educ_yrs)
  
  # Screening event: screening/consent
  screen_keepVars<-c("study_id","consent_scrnfail","consent_scrnfail_det",
                      "consent_yesno","consent_audio_rp","consent_audio_me",
                     "consent_future","consent_b12_results")
  
  screen_myData<-myData[screen_keepVars]
  
  screen_myData<-screen_myData %>% 
    mutate_all(na_if,"")
  
  screen_myData<-screen_myData[!is.na(screen_myData$consent_scrnfail) | 
                                 !is.na(screen_myData$consent_yesno),]
  
  # Baseline event: innate immune history
  base_keepVars<-c("study_id","redcap_event_name","immune_date_base","head_visit_comp",
                   "lp_date","consensus_date","consensus_conference_complete")
  
  base_myData<-myData[base_keepVars]
  
  base_myData<-base_myData %>% 
    mutate_all(na_if,"")
  
  base_myData<-base_myData %>%
    drop_na(study_id,immune_date_base)
  
  base_myData<-base_myData[base_myData$redcap_event_name=="baseline_visit_arm_1",]
  
  base_myData %<>% dplyr::select(-c(redcap_event_name))
  
  # Follow-up event: innate immune history
  fu_keepVars<-c("study_id","redcap_event_name","immune_date_fu","head_visit_comp",
                   "lp_date","consensus_date","consensus_conference_complete")
  
  fu_myData<-myData[fu_keepVars]
  
  fu_myData<-fu_myData %>% 
    mutate_all(na_if,"")
  
  fu_myData<-fu_myData %>%
    drop_na(study_id,immune_date_fu)
  
  fu_myData<-fu_myData[fu_myData$redcap_event_name=="2_year_follow_up_v_arm_1",]
  
  fu_myData %<>% dplyr::rename(head_visit_comp_fu=head_visit_comp,lp_date_fu=lp_date,
                        consensus_date_fu=consensus_date,
                        consensus_conference_complete_fu=consensus_conference_complete)
  
  fu_myData %<>% dplyr::select(-c(redcap_event_name))
  
  # Combining the records, screening, and baseline datasets
  myData_merge<-merge(rec_myData,base_myData,by="study_id",all.x=TRUE)
  myData_merge<-merge(myData_merge,fu_myData,by="study_id",all.x=TRUE)
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
  
  myData_merge$demo_sex<- factor(myData_merge$demo_sex,levels=c("Female","Male","Prefer not to Answer"))
  
  myData_merge$demo_ethnicity<-ifelse(myData_merge$demo_ethnicity==1,"Not Hispanic or Latino/a",
                                ifelse(myData_merge$demo_ethnicity==2,"Hispanic or Latino/a",
                                       ifelse(myData_merge$demo_ethnicity==3,"Unknown",
                                              ifelse(myData_merge$demo_ethnicity==9,"Prefer not to Answer",NA))))
  
  myData_merge$demo_ethnicity<- factor(myData_merge$demo_ethnicity,levels=c("Not Hispanic or Latino/a","Hispanic or Latino/a",
                                                                            "Unknown","Prefer not to Answer"))
  
  myData_merge$demo_handedness<-ifelse(myData_merge$demo_handedness==0,"Right",
                                 ifelse(myData_merge$demo_handedness==1,"Left",
                                        ifelse(myData_merge$demo_handedness==2,"Ambidextrous",
                                               ifelse(myData_merge$demo_handedness==9,"Prefer not to Answer",NA))))
  
  myData_merge$demo_handedness<- factor(myData_merge$demo_handedness,levels=c("Right","Left","Ambidextrous",
                                                                              "Prefer not to Answer"))
  
  # Creating new race categories
  myData_merge$demo_race_NatAmer<-factor(ifelse(myData_merge$demo_race___0==1,"Yes","No"),levels=c("Yes","No"))
  myData_merge$demo_race_Asian<-factor(ifelse(myData_merge$demo_race___1==1,"Yes","No"),levels=c("Yes","No"))
  myData_merge$demo_race_Black<-factor(ifelse(myData_merge$demo_race___2==1,"Yes","No"),levels=c("Yes","No"))
  myData_merge$demo_race_Cauc<-factor(ifelse(myData_merge$demo_race___3==1,"Yes","No"),levels=c("Yes","No"))
  myData_merge$demo_race_PacIsl<-factor(ifelse(myData_merge$demo_race___4==1,"Yes","No"),levels=c("Yes","No"))
  myData_merge$demo_race_Unkn<-factor(ifelse(myData_merge$demo_race___5==1,"Yes","No"),levels=c("Yes","No"))
  myData_merge$demo_race_NoAns<-factor(ifelse(myData_merge$demo_race___9==1,"Yes","No"),levels=c("Yes","No"))
  
  # Numeric to yes/no optional measures consent
  myData_merge$consent_audio_rp<-ifelse(myData_merge$consent_audio_rp==1,"Yes",
                                        ifelse(myData_merge$consent_audio_rp==0,"No",NA))
  myData_merge$consent_audio_me<-ifelse(myData_merge$consent_audio_me==1,"Yes",
                                        ifelse(myData_merge$consent_audio_me==0,"No",NA))
  myData_merge$consent_future<-ifelse(myData_merge$consent_future==1,"Yes",
                                        ifelse(myData_merge$consent_future==0,"No",NA))
  myData_merge$consent_b12_results<-ifelse(myData_merge$consent_b12_results==1,"Yes",
                                        ifelse(myData_merge$consent_b12_results==0,"No",NA))
  
  
  # Creating a new death date variable
  myData_merge$death_date<-as.character(as.Date(myData_merge$with_inelig_dthdte))
  
  # Dropping old race and death date variables
  drop_rcdth_vars<-names(myData_merge) %in% c("demo_race___0","demo_race___1",
                                             "demo_race___2","demo_race___3",
                                             "demo_race___4","demo_race___5",
                                             "demo_race___9","with_inelig_dthdte") 
  myData_final<-myData_merge[!drop_rcdth_vars]
  
  # Date work
  myData_final$curr_age<-round(as.numeric(difftime(Sys.Date(),myData_final$demo_dob,
                                                   units="days"))/364.25,2)
  myData_final$time_diff<-difftime(Sys.Date(),myData_final$immune_date_base,units="days")
  myData_final$next_appt<-ifelse(myData_final$time_diff>=150 & 
                                   myData_final$time_diff<=210,"6 Month Survey",
                         ifelse(myData_final$time_diff>=335 & 
                                  myData_final$time_diff<=395, "12 Month Survey",
                                ifelse(myData_final$time_diff>=515 & 
                                         myData_final$time_diff<=575, "18 Month Survey",
                                       ifelse(myData_final$time_diff>=669 & 
                                                myData_final$time_diff<=789,"2 Year Follow Up",NA))))
  myData_final$next_appt_date<-ifelse(myData_final$next_appt=="6 Month Survey",as.Date(myData_final$immune_date_base)+180,
                                      ifelse(myData_final$next_appt=="12 Month Survey",as.Date(myData_final$immune_date_base)+365,
                                             ifelse(myData_final$next_appt=="18 Month Survey",as.Date(myData_final$immune_date_base)+545,
                                                    ifelse(myData_final$next_appt=="2 Year Follow Up",as.Date(myData_final$immune_date_base)+730,NA))))
  myData_final$next_appt_date_format<-paste0(lubridate::month(as.Date(myData_final$next_appt_date,origin="1970-01-01"),label=TRUE)," ",
                                             lubridate::day(as.Date(myData_final$next_appt_date,origin="1970-01-01")),", ",
                                             lubridate::year(as.Date(myData_final$next_appt_date,origin="1970-01-01")))
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
  
  myData_final$with_inelig_detail<-as.character(myData_final$with_inelig_detail)
  myData_final$consent_scrnfail_det<-as.character(myData_final$consent_scrnfail_det)
  
  myData_final$comments<-ifelse(!is.na(myData_final$with_inelig_detail),myData_final$with_inelig_detail,
                                ifelse(!is.na(myData_final$consent_scrnfail_det),myData_final$consent_scrnfail_det,NA))
  
  # Who is due for consensus conference?
  myData_final$cons_conf_due <- ifelse(myData_final$head_visit_comp==1 & myData_final$consensus_conference_complete==0,"Baseline",
                                       ifelse(myData_final$head_visit_comp_fu==1 & myData_final$consensus_conference_complete_fu==0,"Follow-up",NA))
  
  # Classifying state of each participant in the study
  myData_final$base_visit_comp <- ifelse(myData_final$head_visit_comp==0 | is.na(myData_final$head_visit_comp),"No",
                                         ifelse(myData_final$head_visit_comp==1,"Yes",NA))
  
  myData_final$fu_visit_comp <- ifelse(myData_final$head_visit_comp_fu==0 | is.na(myData_final$head_visit_comp_fu),"No",
                                         ifelse(myData_final$head_visit_comp_fu==1,"Yes",NA))
  
  myData_final$base_lp_comp <- ifelse(is.na(myData_final$lp_date),"No","Yes")
  myData_final$fu_lp_comp <- ifelse(is.na(myData_final$lp_date_fu),"No","Yes")
  
  
  
  # Final dataset
  myData_final
}
