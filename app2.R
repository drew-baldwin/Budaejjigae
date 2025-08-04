

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
                            
                      
                            
                            fileInput('sensor_files','Choose Sensor Files', multiple=TRUE),
                            fileInput('Lead_file','Choose the Primary Analysis Data',multiple=TRUE),
                            #fileInput('QC_file','Choose QC Dataset', multiple=TRUE),
                            
                            tableOutput('contents'),
                            tableOutput('var_summary')
                            
                            
                            
                            
                            #textOutput('contents')
                            #DT::DTOutput('QC'),
                            #tableOutput('QC_report')
                            
                            
                          )
                          
                          
                          
                          
                          
                 ),
                 
                 tabPanel("Build Dataset",
                          fluidPage(
                            div(style="border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;",
                                
                                tags$p('This tab allows the user to upload the source CRF datasets and construct analysis datasets.')
                            ),
                            
                            
                            fileInput('raw_sensor_files','Choose the CGM files to upload',multiple=TRUE),
                            fileInput('crf_files','Choose CRF Files To Upload', multiple=TRUE),
                            #fileInput('Lead_file','Choose the Primary Analysis Data',multiple=TRUE),
                            #fileInput('QC_file','Choose QC Dataset', multiple=TRUE),
                            
                            tableOutput('a_pop'),
                            #tableOutput('var_summary')
                            
                            
                            
                            
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
    
    subject_ids()
    
    
    
  })
  

  

  
  
  
  
  
  
  
  
  

  
  
  
  
  
  #QC tab
  
  
  #first read in the sensor files to build derived sensor data.
  combined_data=reactive({
    req(input$sensor_files)
    

    raw_sensor_list = lapply(input$sensor_files$datapath, process_file)
    #raw_sensor_list = mapply(process_file,input$sensor_files$datapath,input$sensor_files$name)
    raw_sensor =  bind_rows(raw_sensor_list)
    
    raw_sensor = raw_sensor %>% distinct() %>%
    mutate(glucose1 = gsub("Invalid EGV", "\"Invalid EGV\"",glucose1),
    glucose1 = ifelse(glucose1 == "0","\"0\"",glucose1))
    
    raw_sensor$subid=strsplit(input$sensor_files$name,"_")[[1]][3]
    raw_sensor$sensor=gsub('.csv','',strsplit(input$sensor_files$name,'_')[[1]][6])
    raw_sensor$serial=strsplit(input$sensor_files$name,'_')[[1]][5]
    raw_sensor$trnsid=ifelse(nchar(raw_sensor$serial)==6,paste0('00',raw_sensor$serial),raw_sensor$serial)
    raw_sensor$siteid=strsplit(input$sensor_files$name,'_')[[1]][2]
    raw_sensor$subject=paste0(raw_sensor$siteid,raw_sensor$subid)
    raw_sensor$device=toupper(strsplit(input$sensor_files$name,'_')[[1]][4])

    
    #mutate(wearloc_cw = ifelse(is.na(wearloc_cw),"",wearloc_cw)) %>%
    #select(-glucose1,-glucosePredicted1)
    
    qc_raw_sensor = raw_sensor 
    
    qc_raw_sensor
    
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
  
  
  
  #we want to be able to upload both the sensor data and CRF data, and then 
  #use this data throughout. 
  
  
  
  
  #first we need to build the sublist, which is all subjects in each dataset.
  #we want to be able to extract the IDs from all the uploaded datasets. 
  #the following function will upload all of the sensor files selected and combine them.
  #The sensor data has the subject ID in the filename, so we need to both upload the datasets, and also extract subject ID from filename. 
  
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
    
    
    return(list(id_list=sensor_file_ids,datasets=qc_raw_sensor))
    
    
    
    
    
  })

  
  #
  
  #have to extract the subject ID from each dataset and build sublist first
  
  data_pop=reactive({
    
    req(sensor_file_ids())
    req(input$crf_files)
    
    #first read in the CRF files the user selects.
    
    data_set_list=list()
    crf_ids=list()
    
    for(i in 1:nrow(input$crf_files)){
      
      data_set_list[[i]]=read_sas(input$crf_files$datapath[i])
      
      crf_ids[[i]]=data_set_list[[i]]%>%pull(SubjectID)%>%unlist()%>%unique()
      
    }
    
    #now pull the subject IDs into a list from both sources and combine.
    
    crf_id_total=tibble(SubjectID=crf_ids%>%unlist()%>%unique())
    
    total_study_ids=inner_join(crf_id_total,sensor_file_ids()$id_list,by='SubjectID')
    
    

    total_study_ids

    
  })
  
  #function that builds the analysis data. 
  #because there are many CRF datasets and only some are used, we need to ensure
  #that the correct datasets are selected, and that we can track which is which.
  build_it=reactive({
    
    #first merge the sensor with the inclusion exclusion data. 
    
    
  })
  

  
  output$a_pop=renderTable({
    
    req(sensor_file_ids())
    
    #sensor_file_ids()$id_list
    
    data_pop()
    
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
