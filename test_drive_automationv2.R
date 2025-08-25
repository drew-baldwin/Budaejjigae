

library(shiny)
library(shinythemes)
library('shinyFiles')
library('bslib')
library(arsenal)
#library(logrx)
library(haven)
library(dplyr)
library(tidyverse)
library('officer')
library('flextable')
library('rtf')
library(tidyr)
library(haven)
library(lubridate)

library(purrr)
library(stringr)
library(tools)
library(readxl)
library(clue)  
library('stringdist')
library('text2vec')



options(shiny.maxRequestSize = 293 * 1024^2)  
#using Junrui's function to process each individual file in the sensor data folder.
process_file = function(filei){
  
  d1=read_csv(filei)
  names(d1)=tolower(names(d1))
  data = d1 %>% mutate(
    glucose1 = as.character(glucose),
    glucosePredicted1 = as.character(glucosepredicted),
    glucose = as.numeric(glucose),
    Datetime = as.character(time),
    rateOfChange = as.character(rateofchange),
    etime = as.character(etime),
    glucosePredicted = ifelse(is.nan(glucosepredicted),NA,glucosepredicted),
    glucosePredicted = as.numeric(glucosepredicted),
    sensor = gsub(".csv","",strsplit(filei,"_")[[1]][6]),
    serial = strsplit(filei,"_")[[1]][5],
    trnsid = ifelse(nchar(serial) == 6, paste0("00",serial), serial),
    disnsln = as.character(applicatorlot),
    #disnsln = ifelse(disnsln == "241015403", "0241015403", disnsln),
    subid =  strsplit(filei,"_")[[1]][3],
    siteid =  strsplit(filei,"_")[[1]][2],
    subject = paste0(siteid,subid),
    device = toupper(strsplit(filei,"_")[[1]][4]),
    configuration = case_when(
      disnsln  == "241015402" ~"Malaysia lot",
      disnsln  == "241009407" ~ "Mesa lot",
      disnsln == "241010406"~ "San Diego lot"
    ),
  ) %>%select(-c(time,serial))
  #rename(eTime = etime) %>%
  #select(-c(time,serial))
  
  #data = data %>%
  #  mutate(rmvdttm_cw = mdy_hm(rmvdttm_cw),
  #         insertdttm_cw = mdy_hm(insertdttm_cw))
  
  
  data
}

#function that computes the point estimates needed for accuracy table
compute_pe=function(df,flag){
  
  filtered_flag=df%>%filter(class_config==flag)
  
  my_pes=c(rep(NA,8))
  
  #compute the point estimate using the mean.
  
  my_pes[1]=round(mean(filtered_flag$d15_100),3)*100
  my_pes[2]=round(mean(filtered_flag$d20_100),3)*100
  my_pes[3]=round(mean(filtered_flag$d30_100),3)*100
  my_pes[4]=round(mean(filtered_flag$d40_100),3)*100
  
  #differences
  my_pes[5]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$bias),1),round(sd(filtered_flag$bias),1))
  my_pes[6]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$ad),1),round(sd(filtered_flag$ad),1))
  my_pes[7]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$rd),1),round(sd(filtered_flag$rd),1))
  my_pes[8]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$ard),1),round(sd(filtered_flag$ard),1))
  
  return(my_pes)
  
}





#code necessary for the crf mapper. 

#first we read in the data dictionary for the standardized IH test drive studies. 
dd=read_excel('C:/Users/jk73268/OneDrive - Dexcom/Documents/data_dictionaries/test_drive_3_7_dd.xlsx',sheet='Forms')


#second DD
#dd=read_excel('C:/Users/jk73268/OneDrive - Dexcom/Documents/data_dictionaries/Medrio_Data_Dictionary_LIVE_Dexcom_PTL1000505_Test_Drive_IH_Multi_Session_22072025_1630.xlsx',sheet='Forms')



#now we subset, and create the text/variable names used in our automation pipeline
var_dict=dd%>%filter(dd$`Object Type`=='Variable')

#now subset and keep only the columns of interest. 

var_dict=var_dict%>%select('Form Name','Form Export Name','Form External ID','Variable Name','Variable Export Name','SAS Label Export','Variable External ID','Label Text on Form')

#get only the automation pipeline variable name, and its text from CRF. 

pipeline=var_dict%>%select('Form Name','Form Export Name','Variable Name','Variable Export Name','SAS Label Export')



pipeline_vars=pipeline$`Variable Name`

#make a new column in the pipeline for pipeline form names, cleaned up
pipeline$forms=trimws(gsub('_S0\\w*','',pipeline$`Form Export Name`)) #note this may need to change based on study.

pipeline=pipeline%>%select('Variable Export Name','SAS Label Export','forms')


#because the data dictionary does not seem to have medrioid/subjectID/subject status, and the form (CRF form name) 
#we need to manually add this to the data dictionary we are using for our pipeline variables. 


crf_forms=unique(pipeline$forms)

#for each of the unique forms, we need to add a variable (row) for medrioid, subjectid, subject status, and also the form name itself.

medrioid=c('medrioid','Medrio ID',NA)
subjectid=c('subjectid','Subject ID',NA)
subjectstatus=c('subjectstatus','Subject Status',NA)
form_name=c('Form','Form',NA)
site=c('site','Site',NA)
formentrydate=c('formentrydt','Form Entry Date',NA)
visit=c('visit','Visit',NA)
subjectvstformid=c('subjectvstformid','Subject Visit Form ID',NA)
vargroup1row=c('vargroup1row','vargroup1row',NA)
othsterm=c('othsterm','Othsterm',NA)
formexternalid=c('formexternalid','Form External ID',NA)
studytitle=c('studytitle','study title',NA)



total_addition=data.frame(rbind(medrioid,subjectid,subjectstatus,form_name,site,formentrydate,visit,subjectvstformid,vargroup1row,othsterm,formexternalid,studytitle))
colnames(total_addition)=names(pipeline)

updated_pipeline=list()
#iterate through each form in pipeline, subset and then add new rows. Then store in list. Take the list and combine all dataframes back together.
for(f in crf_forms){
  
  my_df=pipeline%>%filter(forms==f)
  
  total_addition[,3]=f
  
  my_df=rbind(my_df,total_addition)
  
  updated_pipeline[[f]]=my_df
  
}

total_pipeline=do.call(rbind,updated_pipeline)


#the map function 

create_map2 <- function(study_path, pipeline_variables, distance_threshold = 0.2) {
  
  
  file_names <- list.files(path = study_path, pattern = "\\.sas7bdat$", full.names = TRUE)
  file_names2 <- head(file_names, 4)
  #subset <- file_names[1:4]
  df_names <- trimws(gsub('_S0\\w*', '', file_path_sans_ext(basename(file_names2))))
  df_names2 <- df_names %>% unlist()
  crf_datasets <- setNames(lapply(file_names2, read_sas), df_names)
  
  crf_datasets <- lapply(crf_datasets, function(df) {
    df %>% select(!contains("_CODED"))
  })
  
  list_of_maps <- list()
  list_of_unmatched_crf <- list()
  list_of_unmatched_pipeline <- list()
  
  for (n in df_names2) {
    print(n)
    if (!is.null(crf_datasets[[n]])) {
      pipeline_variables <- total_pipeline %>%
        filter(forms %in% n) %>%
        pull(`Variable Export Name`) %>%
        tolower()
      
      names(crf_datasets[[n]]) <- tolower(names(crf_datasets[[n]]))
      crf_variables <- names(crf_datasets[[n]])
      
      if (length(pipeline_variables) > 0) {
        remaining_pipeline <- pipeline_variables
        matched_pipeline <- character()
        matched_distances <- numeric()
        unmatched_crf <- character()
        
        for (crf_var in crf_variables) {
          if (length(remaining_pipeline) == 0) {
            matched_pipeline <- c(matched_pipeline, NA)
            matched_distances <- c(matched_distances, NA)
            unmatched_crf <- c(unmatched_crf, crf_var)
            next
          }
          
          distances <- stringdist(crf_var, remaining_pipeline, method = "jw")
          best_index <- which.min(distances)
          best_match <- remaining_pipeline[best_index]
          best_distance <- distances[best_index]
          
          if (best_distance <= distance_threshold) {
            matched_pipeline <- c(matched_pipeline, best_match)
            matched_distances <- c(matched_distances, best_distance)
            remaining_pipeline <- remaining_pipeline[-best_index]
          } else {
            matched_pipeline <- c(matched_pipeline, NA)
            matched_distances <- c(matched_distances, NA)
            unmatched_crf <- c(unmatched_crf, crf_var)
          }
        }
        
        my_map <- data.frame(
          crf_name_original = crf_variables,
          linked_pipeline = matched_pipeline,
          distance = matched_distances,
          stringsAsFactors = FALSE
        )
        
        
        
        pipeline_info <- total_pipeline %>%
          filter(forms %in% n) %>%
          select(`Variable Export Name`, `SAS Label Export`) %>%
          mutate(`Variable Export Name` = tolower(`Variable Export Name`))
        
        # Join descriptive label to the matched pipeline variable
        my_map <- my_map %>%
          left_join(pipeline_info, by = c("linked_pipeline" = "Variable Export Name"))
        
        
        
        
        list_of_maps[[n]] <- my_map
        list_of_unmatched_crf[[n]] <- unmatched_crf
        list_of_unmatched_pipeline[[n]] <- remaining_pipeline
      } else {
        warning(sprintf("No pipeline variables found for form '%s'.", n))
        list_of_maps[[n]] <- data.frame(
          crf_name_original = crf_variables,
          linked_pipeline = NA,
          distance = NA,
          stringsAsFactors = FALSE
        )
        list_of_unmatched_crf[[n]] <- crf_variables
        list_of_unmatched_pipeline[[n]] <- character()
      }
    } else {
      message(sprintf("Dataset '%s' is NULL and was skipped.", n))
    }
  }
  
  return(list(
    maps = list_of_maps,
    unmatched_crf = list_of_unmatched_crf,
    unmatched_pipeline = list_of_unmatched_pipeline
  ))
}







#having multiple tabs to explore data/check for errors/run the analysis 

ui <- navbarPage(theme = shinytheme("lumen"),"In House Study Analysis",
                 
                 tabPanel(
                   'Overview',
                   fluidPage(
                     
                     div(style="border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;",
                         tags$strong('In-House Study Automation Overview'),
                         tags$p('This application allows users to perform different tasks based
                       upon the study requirements. To upload and view data navigate to the Show Data tab. Similarly, to perform
                       QC navigate to the Perform QC tab.
                       Additional analysis options such as one-off table generation may be conducted on the Additional Analysis Tab.')
                     )
                   )), 
                 #selectInput('tab_selector','Select the Analysis Required',
                 #            choices=c('Overview'='Overview','Show Data'='data','QC'='Perform QC'))),     
                 
                 
                 
                 tabPanel(
                   'Map CRFs',
                   fluidPage(
                     
                     div(style="border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;",
                         tags$strong('Map CRF data to automation pipeline'),
                         tags$p('To allow greater generalizability this application uses fixed variables for all dataset and output derivations. The user is 
                                required to upload the CRF (SAS) files from the current study to allow CRF variables to be mapped into automation variables. 
                                The user is then required to confirm the mapping for those variables that are not a perfect match with the automation variables.')
                     ),
                     textInput('crf_var_upload','Enter the Study Path:',value=''),
                     numericInput('threshold','Distance Threshold:',value=0.2,min=0,max=1,step = 0.1),
                     actionButton('run_map','Create CRF Map'),
                     tableOutput('my_crf_map'),
                     hr(),
                     h4('Does the fuzzy matching in the above CRF Map appear correct?'),
                     selectInput('user_approval','Your Response:', choices=c('Yes','No')),
                     actionButton('confirm_approval','Confirm'),
                     verbatimTextOutput("approval_status")
                     
                     #tableOutput('Unmatched_crf_vars')
                     
                   )),
                 
                 
                 
                 tabPanel("Build Dataset",
                          fluidPage(
                            div(style="border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;",
                                
                                tags$p('This tab allows the user to upload the source CRF datasets and construct analysis datasets.')
                            ),
                            
                            
                            #fileInput('raw_sensor_files','Choose the CGM files to upload',multiple=TRUE),
                            textInput('crf_files','Enter study path to upload CRF data'),
                            selectInput('target_df','Select the Derived Data to Build',choices=c('','A_pop','Accuracy','Precision'),selected=''),
                            
                            uiOutput('dataset_selector'),
                            actionButton('merge_btn','Merge Selected Datasets'),
                            #fileInput('Lead_file','Choose the Primary Analysis Data',multiple=TRUE),
                            #fileInput('QC_file','Choose QC Dataset', multiple=TRUE),
                            
                            textOutput('a_pop'),
                            
                            tableOutput("merged_table")
                            
                            #tableOutput('var_summary')
                            
                            
                            
                            
                            #textOutput('contents')
                            #DT::DTOutput('QC'),
                            #tableOutput('QC_report')
                            
                            
                          )
                          
                          
                          
                          
                          
                 ),
                 
                 
                 tabPanel(
                   title="Data Summary",value='data',
                   fluidPage(
                     div(style='border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;',
                         tags$p('This tab allows the user to select all SAS datasets in the raw data folder and perform initial checks such as number of subjects in the study and other data quality issues.')),
                     
                     
                     fileInput('upload','Choose CRF datafiles to upload',multiple=TRUE),
                     fileInput('cgm_data','Choose CGM datafiles to upload',multiple=TRUE),
                     
                     #DT::DTOutput('files'),
                     tableOutput('selected_file_table')
                     
                   )
                 ),
                 tabPanel("Perform QC",
                          fluidPage(
                            div(style="border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;",
                                
                                tags$p('This tab allows the user to perform QC on in-house studies by uploading the raw data from the appropriate folder
                                 and deriving analysis datasets and outputs, which are then compared to the primary statisticians outputs and a report is generated.')
                            ),
                            
                            
                            
                            fileInput('sensor_files','Choose QC Data', multiple=TRUE),
                            fileInput('Lead_file','Choose the Primary Analysis Data',multiple=TRUE),
                            #fileInput('QC_file','Choose QC Dataset', multiple=TRUE),
                            
                            tableOutput('contents'),
                            tableOutput('var_summary')
                            
                            
                            
                            
                            #textOutput('contents')
                            #DT::DTOutput('QC'),
                            #tableOutput('QC_report')
                            
                            
                          )
                          
                          
                          
                          
                          
                 ),
                 
                 
                 
                 tabPanel("Additional Analysis",
                          fluidPage(
                            div(style="border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;",
                                
                                tags$p('This tab allows selection of one-off analysis and generation of tables after the user uploads a dataset. 
                                 Possible analysis includes generation of accuracy table, reproducibility, and demographics tables.')
                            ),
                            fileInput('additional_file','Choose Dataset to Analyze', multiple=TRUE),
                            
                            selectInput(inputId='dropdown',label='Select one-off Analysis to Conduct',choices=c('Accuracy','Precision','Demographics')),
                            
                            
                            
                            #tags$h3("Table: System Accuracy of CGM vs. Comparator(All Devices)"),
                            uiOutput("table_title"),
                            tableOutput('accuracy'),
                            downloadButton("download_rtf", "Download as RTF")
                            
                            
                            #fileInput('additional_file','Choose Analysis Dataset', multiple=TRUE),
                            #fileInput('QC_file','Choose QC Dataset', multiple=TRUE),
                            #tableOutput('contents_additional')
                            #textOutput('contents')
                            #DT::DTOutput('QC'),
                            #tableOutput('QC_report')
                            
                            
                          )
                          
                          
                          
                          
                          
                 )
                 
                 
                 
                 
                 
)






server <- function(input, output) {
  
  
  
  
  #code to build the crf mapper on server side.
  #We let the user upload all the SAS files for that study, and build map function
  
  results=reactiveVal(NULL)
  observeEvent(input$run_map,{
    
    req(input$crf_var_upload)
    
    result=create_map2(
      study_path=input$crf_var_upload,
      pipeline_variables = pipeline_variables,
      distance_threshold=input$threshold
    )
    results(result)
    
  })
  
  
  
  crf_map_data <- reactive({
    req(results())
    
    do.call(rbind, lapply(names(results()$maps), function(name) {
      df <- results()$maps[[name]]
      df$form <- name
      df %>% filter(!is.na(distance) & distance > 0) %>% arrange(desc(distance))
    }))
  })
  
  
  
  output$my_crf_map=renderTable({
    
    #req(results())
    crf_map_data()
    
    # do.call(rbind,lapply(names(results()$maps),function(name) {
    #   df=results()$maps[[name]]
    #   df$form=name
    #   df %>% filter(!is.na(distance) & distance > 0)%>%arrange(desc(distance))
    
  })
  
  
  
  #code for user approval of the fuzzy matching in CRF map 
  
  
  shared <- reactiveValues(user_approval = NULL)
  
  
  observeEvent(input$confirm_approval, {
    shared$approval <- input$user_approval
    #updateTabsetPanel(session, "tabs", selected = "summary_tab")
  })
  
  
  
  observeEvent(input$confirm_approval, {
    shared$approval_status <- input$user_approval
    if (input$user_approval == "Yes") {
      shared$approved_data <- crf_map_data()
    } else {
      shared$approved_data <- NULL
    }
    #updateTabsetPanel(session, "tabs", selected = "approved_tab")
  })
  
  
  output$approval_status <- renderText({
    paste("Approval status:", shared$approval)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #first need to read all SAS files and obtain the subject IDs in each file.
  #this is the main portion of building 'sublist'.
  
  
  subject_ids=reactive({
    
    req(input$upload)
    req(input$cgm_data)
    
    sensor_data_names=list()
    
    for(i in 1:nrow(input$cgm_data)){
      sensor_data_names[[i]]=substr(input$cgm_data$name[i],13,15) #note this may need to be changed if the file format changes.
      
    }
    
    sensor_files=tibble(SubjectID=sensor_data_names%>%unlist()%>%unique())
    
    
    
    #make empty list to populate subject IDs for each CRF SAS dataset
    my_ids=list()
    
    for(i in 1:nrow(input$upload)){
      
      my_crf_file=read_sas(input$upload$datapath[i])
      
      #extract the subject ID's
      my_ids[[i]]=my_crf_file%>%pull(SubjectID)%>%unlist()%>%unique()
      
      
      
      
    }
    
    #now we have the list of subject IDS in each SAS dataset in a list.
    #we unlist the list and keep only the unique subject IDs
    
    id_list=my_ids%>%unlist()%>%unique()
    
    subject_id_list=tibble(SubjectID=id_list)
    
    #return the IDS that are in all SAS datasets and also in sensor data.
    
    my_total_ids=inner_join(subject_id_list,sensor_files,by='SubjectID')
    
    
    
  })
  
  
  
  
  
  output$selected_file_table=renderTable({
    
    #subject_ids()
    #crf_map_data()
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #QC tab
  
  
  #first read in the sensor files to build derived sensor data.
  combined_data=reactive({
    
    req(input$sensor_files)
    qc_data=read_sas(input$sensor_files$datapath)
    
    # raw_sensor_list = lapply(input$sensor_files$datapath, process_file)
    # #raw_sensor_list = mapply(process_file,input$sensor_files$datapath,input$sensor_files$name)
    # raw_sensor =  bind_rows(raw_sensor_list)
    # 
    # raw_sensor = raw_sensor %>% distinct() %>%
    #   mutate(glucose1 = gsub("Invalid EGV", "\"Invalid EGV\"",glucose1),
    #          glucose1 = ifelse(glucose1 == "0","\"0\"",glucose1))
    # 
    # raw_sensor$subid=strsplit(input$sensor_files$name,"_")[[1]][3]
    # raw_sensor$sensor=gsub('.csv','',strsplit(input$sensor_files$name,'_')[[1]][6])
    # raw_sensor$serial=strsplit(input$sensor_files$name,'_')[[1]][5]
    # raw_sensor$trnsid=ifelse(nchar(raw_sensor$serial)==6,paste0('00',raw_sensor$serial),raw_sensor$serial)
    # raw_sensor$siteid=strsplit(input$sensor_files$name,'_')[[1]][2]
    # raw_sensor$subject=paste0(raw_sensor$siteid,raw_sensor$subid)
    # raw_sensor$device=toupper(strsplit(input$sensor_files$name,'_')[[1]][4])
    # 
    # 
    # #mutate(wearloc_cw = ifelse(is.na(wearloc_cw),"",wearloc_cw)) %>%
    # #select(-glucose1,-glucosePredicted1)
    # 
    # qc_raw_sensor = raw_sensor 
    # 
    # qc_raw_sensor
    
    # data_list=lapply(input$sensor_files$datapath,function(path){
    #   tryCatch(read_csv(path),error=function(e) NULL)
    # })
    # 
    # do.call(rbind,data_list)
    
    
    
    
    
    
  })
  
  
  lead_data=reactive({
    
    req(input$Lead_file)
    lead_df=read_sas(input$Lead_file$datapath)
    
    
  })
  
  output$contents=renderTable({
    req(combined_data())
    #head(combined_data())
    
    #combined_data()
    my_summary=summary(comparedf(lead_data(),combined_data()))
    
    temp_table=my_summary$comparison.summary.table
    
    temp_table
    
    
    
  })
  
  output$var_summary=renderTable({
    
    req(combined_data())
    
    my_summary2=summary(comparedf(lead_data(),combined_data()))
    
    table2=my_summary2$diffs.table
    colnames(table2)=c('Lead Variable', 'QC Variable','Row Name', 'Lead Value','QC Value','Lead Row','QC Row')
    
    table2
    
    
    
    
  })
  
  
  
  
  
  #the dataset builder Tab. We want this to eventually be able to build 
  #a_pop, match5_smbg, precision datasets etc.
  
  #first we read in all CRF datasets, and assign the name to each.
  
  #next we need to build the sublist, which is subjects present in all datasets.
  
  #lastly we do some merging, currently we are ONLY building a_pop, which
  #is merging sublist, inclusion/exclusion, and insertion survey.
  
  
  crf_datasets=reactive({
    
    req(input$crf_files)
    
    data_builder_file_path=input$crf_files
    
    file_names_db=list.files(path=data_builder_file_path,pattern='\\.sas7bdat$',full.names=TRUE)
    
    
    max_size_bytes <- 300 * 1024  # 300 KB
    file_sizes <- file.info(file_names_db)$size
    filtered_files <- file_names_db[file_sizes <= max_size_bytes]
    
    
    
    data_list=lapply(filtered_files,read_sas)
    names(data_list)=basename(filtered_files)|>tools::file_path_sans_ext()
    
    return(data_list)
    
    #make list to store the CRF datasets 
  #   data_list=list()
  #   
  #   for(i in seq_len(nrow(input$crf_files))){
  #     
  #     name=file_path_sans_ext(input$crf_files$name[i])
  #     path=input$crf_files$datapath[i]
  #     
  #     data_list[[name]]=read_sas(path)
  #     
  #   }
  #   data_list
  #   
  #   
  #   
  #   
   })
  
  
  
  
  sensor_file_ids=reactive({
    
    #req(input$crf_files)
    req(input$raw_sensor_files)
    
    
    #this part is simply extracting the subject IDs
    sensor_data_names=list()
    
    for(i in 1:nrow(input$raw_sensor_files)){
      sensor_data_names[[i]]=substr(input$raw_sensor_files$name[i],13,15) #note this may need to be changed if the file format changes.
      
    }
    
    sensor_file_ids=tibble(SubjectID=sensor_data_names%>%unlist()%>%unique())
    
    sensor_file_ids
    
    
    #now we have to upload the actual datafiles themselves and also add column
    #that contains the subject ID for each file.
    
    raw_sensor_list = lapply(input$raw_sensor_files$datapath, process_file)
    #raw_sensor_list = mapply(process_file,input$sensor_files$datapath,input$sensor_files$name)
    raw_sensor =  bind_rows(raw_sensor_list)
    
    raw_sensor = raw_sensor %>% distinct() %>%
      mutate(glucose1 = gsub("Invalid EGV", "\"Invalid EGV\"",glucose1),
             glucose1 = ifelse(glucose1 == "0","\"0\"",glucose1))
    
    raw_sensor$subid=strsplit(input$raw_sensor_files$name,"_")[[1]][3]
    raw_sensor$sensor=gsub('.csv','',strsplit(input$raw_sensor_files$name,'_')[[1]][6])
    raw_sensor$serial=strsplit(input$raw_sensor_files$name,'_')[[1]][5]
    raw_sensor$trnsid=ifelse(nchar(raw_sensor$serial)==6,paste0('00',raw_sensor$serial),raw_sensor$serial)
    raw_sensor$siteid=strsplit(input$raw_sensor_files$name,'_')[[1]][2]
    raw_sensor$subject=paste0(raw_sensor$siteid,raw_sensor$subid)
    raw_sensor$device=toupper(strsplit(input$raw_sensor_files$name,'_')[[1]][4])
    
    
    #mutate(wearloc_cw = ifelse(is.na(wearloc_cw),"",wearloc_cw)) %>%
    #select(-glucose1,-glucosePredicted1)
    
    qc_raw_sensor = raw_sensor 
    
    
    #return(list(id_list=sensor_file_ids,datasets=qc_raw_sensor))
    
    return(qc_raw_sensor)
    
    
    
  })
  
  # output$dataset_selector <- renderUI({
  #   req(crf_datasets())
  #   checkboxGroupInput("selected_datasets", "Select Datasets to Merge",
  #                      choices = names(crf_datasets()))
  # })
  
  merged_result=eventReactive(input$merge_btn, {
    
    #req(input$target_choice != "")
    #req(input$selected_datasets)
    #selected=input$selected_datasets
    
    data_list=crf_datasets()
    
    #get all subject IDs to build sublist 
    
    
    
    subject_id_df_list <- lapply(names(data_list), function(name) {
      df <- data_list[[name]]
      if ("SubjectID" %in% names(df)) {
        return(data.frame(SubjectID = df$SubjectID, stringsAsFactors = FALSE))
      } else {
        return(NULL)
      }
    })
    
    # Combine into one data frame
    subject_id_df <- do.call(rbind, subject_id_df_list)
                             
    
    
    names(data_list)=selected
    
    #print(selected)
    
    #to build a_pop dataset, composed of sublist, inclusion/exclusion, insertion survey
    
    #because we cannot see what datasets are uploaded, we make a map list that only lets those datasets be used.
    #a_pop_datasets=list('Inclusion_Exclusion'='inclusion','Insertion_Survey'='survey')
    
    
    a_pop_datasets= selected[
      grepl("inclusion|insertion_survey", selected, ignore.case = TRUE)
    ]
    a_pop_builders=data_list[a_pop_datasets]
    
    #print(a_pop_builders)
    
    if(input$target_df=='A_pop'){
      
      #filter out the non-useful datasets. 
      #a_pop_builders=intersect(names(a_pop_datasets),selected)
      if(length(a_pop_builders)==0){
        print('No relevant datasets to build A_pop selected')
      }
      
      #make dataset names using the a_pop builders datasets 
      
      inclusion_exclusion=a_pop_builders[[which(grepl('inclusion_exclusion',names(a_pop_builders),ignore.case=TRUE))]]
      insertion_survey= a_pop_builders[[which(grepl("insertion_survey", names(a_pop_builders), ignore.case = TRUE))]]
      
      #to test, left join inclusion_exclusion and only keep specific variables. 
      
      merged=subject_id_df%>%left_join(inclusion_exclusion%>%select(SubjectID,COYN),by='SubjectID')%>%left_join(insertion_survey,by='SubjectID')
      
   
      
      # merged=a_pop_builders[[1]]
      # 
      # for(i in 2:length(a_pop_builders)){
      #   
      #   
      #   
      #   merged=full_join(merged,a_pop_builders[[i]],by='SubjectID')
      # }
      
      return(merged)
    }
    
    else if(input$target_df=='Accuracy'){
      merged=data_list[[selected[1]]]
      
      for(i in 2:length(selected)){
        merged=full_join(merged,data_list[[selected[i]]],by='SubjectID')
      }
      
      return(merged)
    }
    else if(input$target_df=='Precision'){
      merged=data_list[[selected[1]]]
      
      for(i in 2:length(selected)){
        
        merged=full_join(merged,data_list[[selected[i]]],by='SubjectID')
        
      }
      return(merged)
    }
    
    
    
    
    
  })
  
  

  
  
  
  # merged_data <- eventReactive(input$merge_btn, {
  #   req(input$selected_datasets)
  #   selected <- crf_datasets()[input$selected_datasets]
  #   validate(
  #     need(length(selected) >= 2, "Please select at least two datasets to merge.")
  #   )
  #   Reduce(function(x, y) merge(x, y, by = "SubjectID"), selected)
  # })
  
  
  
  
  
  #
  
  #have to extract the subject ID from each dataset and build sublist first
  
  # data_pop=reactive({
  #   
  #   req(sensor_file_ids())
  #   req(input$crf_files)
  #   
  #   #first read in the CRF files the user selects.
  #   
  #   data_set_list=list()
  #   crf_ids=list()
  #   
  #   for(i in 1:nrow(input$crf_files)){
  #     
  #     data_set_list[[i]]=read_sas(input$crf_files$datapath[i])
  #     
  #     crf_ids[[i]]=data_set_list[[i]]%>%pull(SubjectID)%>%unlist()%>%unique()
  #     
  #   }
  #   
  #   #now pull the subject IDs into a list from both sources and combine.
  #   
  #   crf_id_total=tibble(SubjectID=crf_ids%>%unlist()%>%unique())
  #   
  #   total_study_ids=inner_join(crf_id_total,sensor_file_ids()$id_list,by='SubjectID')
  #   
  #   
  #   
  #   total_study_ids
  #   
  #   
  # })
  
  #function that builds the analysis data. 
  #because there are many CRF datasets and only some are used, we need to ensure
  #that the correct datasets are selected, and that we can track which is which.
  
  
  #in this function, we bring in all the CRF datasets, assign names to each
  #and then use this to merge the data.
  
  
  
  
  
  
  output$a_pop=renderPrint({
    
    names(crf_datasets())
    #names(build_it())
    
  })
  
  

  
  
  output$merged_table <- renderTable({
    merged_result()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #Additional Analysis Tab, currently this only allows us to build accuracy table
  #by using an already derived dataset. This will have to be adjusted to be generalizable. 
  
  
  my_analysis_data=reactive({
    
    req(input$additional_file)
    
    a_dat=read_sas(input$additional_file$datapath)
    
    
    #make column names all lowercase for generalizability
    
    names(a_dat)=tolower(names(a_dat))
    
    a_dat
    
    
  })
  
  output$accuracy=renderTable({
    
    
    
    if(input$dropdown=='Accuracy'){
      
      
      req(my_analysis_data())
      
      #names(my_analysis_data)=tolower(names(my_analysis_data()))
      
      matched_pairs=my_analysis_data()%>%select(subject, d15_100, 	d20_100,		 d30_100, 	 d40_100,  	 d15_70, 	d20_70,	  	d30_70, 	   d40_70,	  bias, 		ad, 		ard, 	rd, configuration, glucose, egv10_5d, egv10d, egvstatus, anl_flg, daysensorwear, wearloc, trnsid, primarydev, inclinic)
      
      
      matched_pairs=matched_pairs%>%mutate(class_config=case_when(
        configuration=='Malaysia lot'~1,
        configuration=='Mesa lot'~2,
        configuration=='San Diego lot'~3
      ))
      
      matched_pairs$class_overall=1
      matched_pairs$overall=1
      
      matched_pairs_filtered=matched_pairs%>%filter(anl_flg==1,egv10_5d==1, egvstatus=='In reportable range')
      
      my_pe_results=data.frame(matrix(ncol=3,nrow=8))
      colnames(my_pe_results)=c('Malaysia Lot','Mesa Lot','San Diego Lot')
      for(i in 1:3){
        
        my_pe_results[,i]=compute_pe(matched_pairs_filtered,i)
        
      }
      
      #lastly get the number of subjects in each lot, and number of pairs.
      n_subjects=c(NA,NA,NA)
      for(i in 1:3){
        a=matched_pairs_filtered%>%filter(class_config==i)%>%summarise(n=n_distinct(subject))
        n_subjects[i]=a$n
        
        
      }
      
      pairs=c(NA,NA,NA)
      for(i in 1:3){
        pairs[i]=nrow(matched_pairs_filtered%>%filter(class_config==i))
        
        
      }
      
      
      my_total_results=rbind(n_subjects,pairs,my_pe_results)
      
      
      
      
      #make first column which displays the statistic name.
      
      stats_names=c('Subjects(N)','Matched Pairs(N)', '%15/15(%)','%20/20(%)','%30/30(%)','%40/40(%)','Bias (STD) (mg/dL)','MAD (STD) (mg/dL)','MRD (STD) (%)','MARD (STD) (%)')
      
      new_results=cbind(stats_names,my_total_results)
      
      colnames(new_results)=c('Statistics','Malaysia Lot','Mesa Lot','San Diego Lot')
      
      return(new_results)
      
      
      
      
      
      
      
      
    }
    
    
    
    
    
  })
  
  
  
  output$table_title <- renderUI({
    #tags$h3("Table: System Accuracy of CGM vs Comparator (All Devices)")
    tags$h3(paste("Table: System", input$dropdown, "of CGM vs Comparator (All Devices)"))
  })
  
  
  
  
  
  # Download handler
  
  
  
  # output$download_csv <- downloadHandler(
  #   filename = function() {
  #     paste0("table-", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(new_results, file, row.names = FALSE)
  #   },
  #   contentType = "text/csv"
  # )
  
  
  
  
  output$download_rtf <- downloadHandler(
    filename = function() {
      paste0("accuracy_table-", Sys.Date(), ".rtf")
    },
    content = function(file) {
      rtf <- RTF(file)
      addParagraph(rtf,paste("Table: System", input$dropdown, "of CGM vs Comparator (All Devices)"))
      addTable(rtf, new_results)
      done(rtf)
    },
    contentType = "application/rtf"
  )
  # 
  
  
  
  
  
  
  
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
