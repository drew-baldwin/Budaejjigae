library(tidyr)
library(haven)
library(lubridate)
library(dplyr)
library(purrr)
library(stringr)
library(tools)
library(readxl)
library(clue)  
library('stringdist')
library('text2vec')


#first we read in the data dictionary for the standardized IH test drive studies. 
dd=read_excel('C:/Users/jk73268/OneDrive - Dexcom/Documents/data_dictionaries/test_drive_3_7_dd.xlsx',sheet='Forms')


#second DD
#dd=read_excel('C:/Users/jk73268/OneDrive - Dexcom/Documents/data_dictionaries/Medrio_Data_Dictionary_LIVE_Dexcom_PTL1000505_Test_Drive_IH_Multi_Session_22072025_1630.xlsx',sheet='Forms')



#now we subset, and create the text/variable names used in our automation pipeline
var_dict=dd%>%filter(dd$`Object Type`=='Variable')

#now subset and keep only the columns of interest. 

var_dict=var_dict%>%select('Form Name','Form Export Name','Form External ID','Variable Name','Variable Export Name','SAS Label Export','Variable External ID','Label Text on Form')

#get only the automation pipeline variable name, and its text from CRF. 

pipeline=var_dict%>%select('Form Name','Form Export Name','Variable Name','SAS Label Export')



pipeline_vars=pipeline$`Variable Name`

#make a new column in the pipeline for pipeline form names, cleaned up
pipeline$forms=trimws(gsub('_S0\\w*','',pipeline$`Form Export Name`)) #note this may need to change based on study.

pipeline=pipeline%>%select('Variable Name','SAS Label Export','forms')


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


total_addition=data.frame(rbind(medrioid,subjectid,subjectstatus,form_name,site,formentrydate,visit,subjectvstformid))
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







#now we proceed and read in each CRF data file (from whatever test drive study), and create the map between names. 

#note that each CRF dataset is coming from a specific CRF form. When we build the map between names we want to do this between variables 
#with the form name fixed, else the distance may be incorrect.


#first provide the path to the specific study, and read in all raw CRF files and store the file names to link to the DD form.

get_crf_form_names=function(folder_path){
  
  file_names <- list.files(path = folder_path, pattern = "\\.sas7bdat$", full.names = TRUE)
  
  new_file_names=basename(file_names) |> file_path_sans_ext()
  new_file_names=tolower(new_file_names)
  
  #replace the '_' with space
  new_file_names2=gsub('_',' ',new_file_names)

  return(tolower(new_file_names2))
  
}

check=get_crf_form_names('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Data/Rawdata/')







#a different approach is to use the data dictionary from the study itself
#and compare it to the data dictionary template that is used across studies.









#now we can use the list of form names as input to our next function to create the map for only that form. 

#the following function will take in the study folder path, the pipeline naming dataset, and will create the map for each
#form from the CRF variables that come from the DATASETS. Then we stack all the maps together to get the overall study map.


#sometimes the dataset names don't match the form name exactly. Its best to strip the form names to keep only the main characters
#currently we remove the S02x from each dataset and form, as this may cause issues.



#NEXT STEPS: WITHIN A FORM, ENSURE THAT WE DON'T LET A PIPELINE VARIABLE MAP TO MORE THAN ONE CRF VARIABLE.



create_map=function(study_path,pipeline_variables){
  
  
  #we again read in all the SAS datasets, but this time we store the data, and the name used to filter to that form
  
  file_names <- list.files(path = study_path, pattern = "\\.sas7bdat$", full.names = TRUE)
  
  
  #make temporary subset to make sure the function works 
  subset=file_names[1:length(file_names)]
  #subset=sas_files[1:3]
  
  #df_names=file_path_sans_ext(basename(subset))
  df_names=trimws(gsub('_S0\\w*','',file_path_sans_ext(basename(subset))))
  
  df_names2=df_names%>%unlist() #this is needed to access the names in for loop later
  
  crf_datasets=setNames(lapply(subset,read_sas),df_names)
  
  
  #need to add a step, because the SAS datasets have '_coded' attached to some variables. 
  #for us we can remove these variables from the datasets initially to get a working prototype.
  
  
  crf_datasets <- lapply(crf_datasets, function(df) {
    df %>% select(!contains("_CODED"))
  })
  


  
  list_of_maps <- list()
  
  for (n in df_names2) {
    
    if (!is.null(crf_datasets[[n]])) {
      
      pipeline_variables <- total_pipeline %>%
        filter(forms %in% n) %>%
        pull(`Variable Name`)%>%tolower()
      
      names(crf_datasets[[n]]) <- tolower(names(crf_datasets[[n]]))
      crf_variables <- names(crf_datasets[[n]])
      
      if (length(pipeline_variables) > 0) {
        
        min_distance <- sapply(crf_variables, function(x) {
          distances <- stringdist(x, pipeline_variables, method = 'jw')
          
          print(data.frame(crf = x, pipeline = pipeline_variables, dist = distances))
          
          pipeline_variables[which.min(distances)]
        })

      } else {
        warning(sprintf("No pipeline variables found for form '%s'.", n))
        min_distance <- rep(NA, length(crf_variables))
      }
      
      my_map <- data.frame(
        crf_name_original = crf_variables,
        linked_pipeline = min_distance,
        stringsAsFactors = FALSE
      )
      
      list_of_maps[[n]] <- my_map
      #list_of_maps[[n]] <- pipeline_variables
      
    } else {
      message(sprintf("Dataset '%s' is NULL and was skipped.", n))
    }
  }
  #return(pipeline_variables)
  
  
  return(list_of_maps)
}


check=create_map('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH3/Data/Rawdata/',pipeline)


#look through each dataset, and count the variables that are not matching. 
combined_maps=do.call(rbind,check)

#flag the variables that are not equal (map was wrong)

combined_maps=combined_maps %>% mutate(flag_diff = crf_name_original != linked_pipeline)

#visually inspect the rows. 
mismatched=combined_maps%>%filter(flag_diff=='TRUE')


sum(combined_maps$crf_name_original!=combined_maps$linked_pipeline)




#THIS VERSION USES A SLIGHTLY DIFFERENT APPROACH TO FORCE A 1-1 MAP. 



create_map2=function(study_path,pipeline_variables){
  
  
  #we again read in all the SAS datasets, but this time we store the data, and the name used to filter to that form
  
  file_names <- list.files(path = study_path, pattern = "\\.sas7bdat$", full.names = TRUE)
  
  
  #make temporary subset to make sure the function works 
  subset=file_names[1:length(file_names)]
  #subset=sas_files[1:3]
  
  #df_names=file_path_sans_ext(basename(subset))
  df_names=trimws(gsub('_S0\\w*','',file_path_sans_ext(basename(subset))))
  
  df_names2=df_names%>%unlist() #this is needed to access the names in for loop later
  
  crf_datasets=setNames(lapply(subset,read_sas),df_names)
  
  
  #need to add a step, because the SAS datasets have '_coded' attached to some variables. 
  #for us we can remove these variables from the datasets initially to get a working prototype.
  
  
  crf_datasets <- lapply(crf_datasets, function(df) {
    df %>% select(!contains("_CODED"))
  })
  
  
  
  
  list_of_maps <- list()
  
  for (n in df_names2) {
    
    if (!is.null(crf_datasets[[n]])) {
      
      pipeline_variables <- total_pipeline %>%
        filter(forms %in% n) %>%
        pull(`Variable Name`)%>%tolower()
      
      names(crf_datasets[[n]]) <- tolower(names(crf_datasets[[n]]))
      crf_variables <- names(crf_datasets[[n]])
      
      if (length(pipeline_variables) > 0) {
        
        
        # Create distance matrix
        distance_matrix <- stringdistmatrix(crf_variables, pipeline_variables, method = "jw")
        
        
        n_rows <- length(crf_variables)
        n_cols <- length(pipeline_variables)
        
        if (n_rows > n_cols) {
          pad_matrix <- matrix(max(distance_matrix) + 1, nrow = n_rows, ncol = n_rows - n_cols)
          distance_matrix <- cbind(distance_matrix, pad_matrix)
        }
        
        
        
        
        # Solve assignment problem (Hungarian algorithm)
        assignment <- solve_LSAP(distance_matrix)
        
        # Get matched pipeline variables
        matched_pipeline <- pipeline_variables[assignment]
        
        
      } else {
        warning(sprintf("No pipeline variables found for form '%s'.", n))
        min_distance <- rep(NA, length(crf_variables))
      }
      
      my_map <- data.frame(
        crf_name_original = crf_variables,
        linked_pipeline = min_distance,
        stringsAsFactors = FALSE
      )
      
      list_of_maps[[n]] <- my_map
      #list_of_maps[[n]] <- pipeline_variables
      
    } else {
      message(sprintf("Dataset '%s' is NULL and was skipped.", n))
    }
  }
  #return(pipeline_variables)
  
  
  return(list_of_maps)
}


check=create_map('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH3/Data/Rawdata/',pipeline)


#look through each dataset, and count the variables that are not matching. 
combined_maps=do.call(rbind,check)

#flag the variables that are not equal (map was wrong)

combined_maps=combined_maps %>% mutate(flag_diff = crf_name_original != linked_pipeline)

#visually inspect the rows. 
mismatched=combined_maps%>%filter(flag_diff=='TRUE')


sum(combined_maps$crf_name_original!=combined_maps$linked_pipeline)






