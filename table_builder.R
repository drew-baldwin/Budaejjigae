

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
compute_pe=function(df,flag,d15_col,d20_col,d30_col,d40_col,bias_col,ad_col,rd_col,ard_col){
  
  filtered_flag=df%>%filter(class_config==flag)
  
  my_pes=c(rep(NA,8))
  
  #compute the point estimate using the mean.
  
  my_pes[1]=round(mean(filtered_flag[[d15_col]]),3)*100
  my_pes[2]=round(mean(filtered_flag[[d20_col]]),3)*100
  my_pes[3]=round(mean(filtered_flag[[d30_col]]),3)*100
  my_pes[4]=round(mean(filtered_flag[[d40_col]]),3)*100
  
  #differences
  my_pes[5]=sprintf('%.2f (%.2f)',round(mean(filtered_flag[[bias_col]]),1),round(sd(filtered_flag[[bias_col]]),1))
  my_pes[6]=sprintf('%.2f (%.2f)',round(mean(filtered_flag[[ad_col]]),1),round(sd(filtered_flag[[ad_col]]),1))
  my_pes[7]=sprintf('%.2f (%.2f)',round(mean(filtered_flag[[rd_col]]),1),round(sd(filtered_flag[[rd_col]]),1))
  my_pes[8]=sprintf('%.2f (%.2f)',round(mean(filtered_flag[[ard_col]]),1),round(sd(filtered_flag[[ard_col]]),1))
  
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
  #file_names2 <- head(file_names, 4)
  #subset <- file_names[1:4]
  df_names <- trimws(gsub('_S0\\w*', '', file_path_sans_ext(basename(file_names))))
  df_names2 <- df_names %>% unlist()
  crf_datasets <- setNames(lapply(file_names, read_sas), df_names)
  
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
                 
                 

                 

                 
                 
                 tabPanel("Additional Analysis",
                          fluidPage(
                            div(style="border: 2px solid #007BFF; padding: 15px; border-radius: 5px; background-color: #F8F9FA;",
                                
                                tags$p('This tab allows selection of one-off analysis and generation of tables after the user uploads a dataset. 
                                 Possible analysis includes generation of accuracy table, reproducibility, and demographics tables.')
                            ),
                            fileInput('additional_file','Choose Dataset to Analyze', multiple=TRUE),
                            uiOutput('column_select_d15'),
                            uiOutput('column_select_d20'),
                            uiOutput('column_select_d30'),
                            uiOutput('column_select_d40'),
                            uiOutput('column_select_ad'),
                            uiOutput('column_select_ard'),
                            uiOutput('column_select_rd'),
                            uiOutput('column_select_bias'),
                            uiOutput('column_select_stratification'),
                            uiOutput('column_select_filter'),
                            
                            selectInput(inputId='dropdown',label='Select one-off Analysis to Conduct',choices=c('','Accuracy','Precision','Demographics')),
                            
                            
                            
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
  
  
  
  #Additional Analysis Tab, currently this only allows us to build accuracy table
  #by using an already derived dataset. This will have to be adjusted to be generalizable. 
  
  
  
  
  #function to compute the PE 
  
  
  # compute_pe=function(df,flag){
  #   
  #   filtered_flag=df%>%filter(class_config==flag)
  #   
  #   my_pes=c(rep(NA,8))
  #   
  #   #compute the point estimate using the mean.
  #   
  #   my_pes[1]=round(mean(filtered_flag$d15_100),3)*100
  #   my_pes[2]=round(mean(filtered_flag$d20_100),3)*100
  #   my_pes[3]=round(mean(filtered_flag$d30_100),3)*100
  #   my_pes[4]=round(mean(filtered_flag$d40_100),3)*100
  #   
  #   #differences
  #   my_pes[5]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$bias),1),round(sd(filtered_flag$bias),1))
  #   my_pes[6]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$ad),1),round(sd(filtered_flag$ad),1))
  #   my_pes[7]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$rd),1),round(sd(filtered_flag$rd),1))
  #   my_pes[8]=sprintf('%.2f (%.2f)',round(mean(filtered_flag$ard),1),round(sd(filtered_flag$ard),1))
  #   
  #   return(my_pes)
  #   
  # }
  
  
  #need to have dx, stratification flags, and population flags be general. 
  
  my_analysis_data=reactive({
    
    req(input$additional_file)
    
    a_dat=read_sas(input$additional_file$datapath)
    
    
    #make column names all lowercase for generalizability
    
    names(a_dat)=tolower(names(a_dat))
    
    a_dat
    
    
  })
  
  
  output$column_select_d15 <- renderUI({
    req(my_analysis_data())
    selectInput("d15_flag", "Select column for D15 Flag", choices = names(my_analysis_data()))
  })
  
  output$column_select_d20 <- renderUI({
    req(my_analysis_data())
    selectInput("d20_flag", "Select column for D20 Flag", choices = names(my_analysis_data()))
  })
  
  output$column_select_d30 <- renderUI({
    req(my_analysis_data())
    selectInput("d30_flag", "Select column for D30 Flag", choices = names(my_analysis_data()))
  })
  
  output$column_select_d40 <- renderUI({
    req(my_analysis_data())
    selectInput("d40_flag", "Select column for D40 Flag", choices = names(my_analysis_data()))
  })
  
  output$column_select_ad=renderUI({
    req(my_analysis_data())
    selectInput('ad_flag','Select column for AD Flag',choices=names(my_analysis_data()))
    
    
  })
  
  output$column_select_ard=renderUI({
    req(my_analysis_data())
    selectInput('ard_flag','Select column for ARD Flag',choices=names(my_analysis_data()))
    
    
  })
  
  output$column_select_rd=renderUI({
    req(my_analysis_data())
    selectInput('rd_flag','Select column for RD Flag',choices=names(my_analysis_data()))
    
    
  })
  
  output$column_select_bias=renderUI({
    req(my_analysis_data())
    selectInput('bias_flag','Select column for Bias Flag',choices=names(my_analysis_data()))
    
    
  })
  
  output$column_select_stratification=renderUI({
    req(my_analysis_data())
    selectInput('strat_flag','Select column for Stratification Flag',choices=names(my_analysis_data()))
    
    
  })
  
  output$column_select_filter=renderUI({
    req(my_analysis_data())
    selectInput('filter_flag','Select column for Filtering Flag',choices=names(my_analysis_data()))
    
    
  })
  

  
  
  
  
  
  output$accuracy=renderTable({
    
    
    
    all_inputs_selected <- reactive({
      all(
        nzchar(input$d15_flag),
        nzchar(input$d20_flag),
        nzchar(input$d30_flag),
        nzchar(input$d40_flag),
        nzchar(input$bias_flag),
        nzchar(input$ad_flag),
        nzchar(input$ard_flag),
        nzchar(input$rd_flag),
        nzchar(input$strat_flag),
        nzchar(input$filter_flag)
      )
    })
    
    
    
    
    
    
    
    if(input$dropdown=='Accuracy'){
      req(my_analysis_data())
      req(all_inputs_selected())
      
      
 
      
      #names(my_analysis_data)=tolower(names(my_analysis_data()))
      
      matched_pairs=my_analysis_data()%>%select(subject, input$d15_flag,input$d20_flag,input$d30_flag,input$d40_flag, input$bias_flag, input$ad_flag, input$ard_flag, input$rd_flag, input$strat_flag, input$filter_flag)
      
      
      matched_pairs=matched_pairs%>%mutate(class_config=as.numeric(factor(!!sym(input$strat_flag))))
      
      matched_pairs$class_overall=1
      matched_pairs$overall=1
      
      #have to add more flags
      #matched_pairs_filtered=matched_pairs%>%filter(input$filter_flag==1,egv10_5d==1, egvstatus=='In reportable range')
      
      #matched_pairs_filtered=matched_pairs%>%filter(input$filter_flag==1)
      
      matched_pairs_filtered <- matched_pairs %>%
        filter(
          !!sym(input$filter_flag) %in% c(1, "1", "Y", "y")
        )
      
      
      
      my_pe_results=data.frame(matrix(ncol=length(unique(matched_pairs[[input$strat_flag]])),nrow=8))
      colnames(my_pe_results)=unique(matched_pairs[[input$strat_flag]])
      
      #df,d15_100,d20_100,d30_100,d40_100,bias,ad,rd,ard,flag
      
      unique_strats = unique(matched_pairs[[input$strat_flag]])
      
      for(i in seq_along(unique_strats)){
        
        my_pe_results[,i]=compute_pe(matched_pairs_filtered,i,input$d15_flag,input$d20_flag,input$d30_flag,input$d40_flag,input$bias_flag,input$ad_flag,input$rd_flag,input$ard_flag)
        
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
      
      #colnames(new_results)=c('Statistics','Malaysia Lot','Mesa Lot','San Diego Lot')
      colnames(new_results)=c("Statistics", unique(matched_pairs[[input$strat_flag]]))
      
      return(new_results)
      #return(matched_pairs_filtered)
      
      
      
      
      
      
      
    }
    
    
    
    
    
  })
  
  
  
  # output$table_title <- renderUI({
  #   #tags$h3("Table: System Accuracy of CGM vs Comparator (All Devices)")
  #   #tags$h3(paste("Table: System", input$dropdown, "of CGM vs Comparator (All Devices)"))
  # })
  
  
  
  
  
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
  
  
  
  
  # output$download_rtf <- downloadHandler(
  #   filename = function() {
  #     paste0("accuracy_table-", Sys.Date(), ".rtf")
  #   },
  #   content = function(file) {
  #     rtf <- RTF(file)
  #     addParagraph(rtf,paste("Table: System", input$dropdown, "of CGM vs Comparator (All Devices)"))
  #     addTable(rtf, new_results)
  #     done(rtf)
  #   },
  #   contentType = "application/rtf"
  # )
  # 
  
  
  
  
  
  
  
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
