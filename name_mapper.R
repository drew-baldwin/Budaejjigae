library(tidyr)
library(haven)
library(lubridate)
library(dplyr)
library(purrr)
library(stringr)
library(tools)
library(readxl)
library('stringdist')
library('text2vec')


#first we read in the data dictionary for the standardized IH test drive studies. 
dd=read_excel('C:/Users/jk73268/OneDrive - Dexcom/Documents/data_dictionaries/data_dictionary_ih_test_drive.xlsx',sheet='Forms')

#now we subset, and create the text/variable names used in our automation pipeline
var_dict=dd%>%filter(dd$`Object Type`=='Variable')

#now subset and keep only the columns of interest. 

var_dict=var_dict%>%select('Form Name','Form Export Name','Form External ID','Variable Name','Variable Export Name','SAS Label Export','Variable External ID','Label Text on Form')

#get only the automation pipeline variable name, and its text from CRF. 

pipeline=var_dict%>%select('Form Name','Form Export Name','Variable Name','SAS Label Export')

pipeline_vars=pipeline$`Variable Name`

#make a new column in the pipeline for pipeline form names, cleaned up
pipeline$forms=trimws(gsub('_S0\\w*','',pipeline$`Form Export Name`)) #note this may need to change based on study.



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

for(i in 1:length(check)){
  
  print(i)
  
}



u=list.files(path='Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Data/RawData',pattern='\\.sas7bdat',full.names=TRUE)



sas_files = u[!grepl("sensor|meter", u)]

subset=sas_files[1:3]

df_names=file_path_sans_ext(basename(subset))

df_names2=df_names%>%unlist()


for(n in df_names2){
  
  print(df_names2[n])
}




datasets=setNames(lapply(subset,read_sas),df_names)

check_df=datasets[['Additional_Comments']]



datasets <- lapply(subset, read_sas) #this works, but its very slow.

names(column_names_list) <- basename(sas_files)

column_names_list <- lapply(u, function(file) {
  colnames(read_sas(file, n_max = 0))
})







#now we can use the list of form names as input to our next function to create the map for only that form. 

#the following function will take in the study folder path, the pipeline naming dataset, and will create the map for each
#form from the CRF. Then we stack all the maps together to get the overall study map.
create_map=function(study_path,pipeline_variables){
  
  
  #we again read in all the SAS datasets, but this time we store the data, and the name used to filter to that form
  
  file_names <- list.files(path = study_path, pattern = "\\.sas7bdat$", full.names = TRUE)
  
  
  #new_file_names=basename(file_names) |> file_path_sans_ext()
  
  
  #new_file_names=tolower(subset)
  
  #replace the '_' with space
  #new_file_names2=tolower(gsub('_',' ',new_file_names))
  
  #read in the actual datasets and store as a list.
  sas_files = file_names[!grepl("sensor|meter", u)]
  
  
  #make temporary subset to make sure the function works 
  subset=sas_files[1:3]
  
  df_names=file_path_sans_ext(basename(subset))
  
  df_names2=df_names%>%unlist() #this is needed to access the names in for loop later
  
  crf_datasets=setNames(lapply(subset,read_sas),df_names)


  
  list_of_maps <- list()
  
  for (n in df_names2) {
    
    if (!is.null(crf_datasets[[n]])) {
      
      pipeline_variables <- pipeline %>%
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


check=create_map('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Data/Rawdata/',pipeline)

ae=read_sas('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Data/Rawdata/Adverse_Event.sas7bdat')


#the first step is to extract all variable names from the CRF data, and lowercase them for easy distance metrics computation.

names(crf_data)=tolower(names(crf_data))

#now store this vector of names to allow us to relate to automation pipeline variables. 

crf_variables=names(crf_data)


#now lets say our pipeline takes in the previous dataset, counts the number of unique subjects, and lists how many are enrolled, and how many are in analysis. 

#the automation pipeline variables are as follows 
pipeline_variables=c('subjectid','insdt_first','screenfail','enrolled','anal_flg','flag_dstst','flag_dsten')

#now we need to associate the crf_variables to the pipeline variables, accounting for small differences. 
#to do this, we use text distances. Specifically, for each crf variable, find the pipeline variable that has the smallest distance and link them. 

link_vars=function(crf_names,pipeline_names){
  
  min_distance=sapply(crf_names,function(x) {pipeline_names[which.min(stringdist(x,pipeline_names,method='jw'))]})
  
  my_map=data.frame(crf_name_original=crf_names,linked_pipeline=min_distance)
  
  return(my_map)
  
  
}


#this returns a two column dataframe, where the last column shows the pipeline linked variables based on text distance. 
#we would also check this using our NLP model based on all data dictionaries, and present the linked names to the user to confirm for the app.


check=link_vars(crf_variables,pipeline_variables)

#now we simply replace the names in the dataframe from CRF to the new names.

colnames(crf_data)=check$linked_pipeline



automation_pipeline_summary=function(df){
  
  unique_ids=df%>%pull(subjectid)%>%n_distinct()
  
  number_enrolled=df%>%filter(enrolled==1)%>%nrow()
  
  return(list(unique_ids,number_enrolled))
  
  
  
}

#now use the automation_pipeline_summary and ensure it works. 

my_summary=automation_pipeline_summary(crf_data)




#write some code that does some analysis using the names in the automation pipeline. 

summary_function=function(df,mapper){
  
  
  return(length(df%>%pull(subjid)%>%unique()))
  
  
}



#our code uses 'subjid' instead of the SubjectID used from the CRF. 
automation_pipeline_names=c('subject','Site','icfdat')
crf_variable_names=c('SubjectID','Site','ICFDAT')

crf_data2=crf_data%>%select(crf_variable_names)

#now use naming map to populate the information from CRF variables to our automation pipeline variables. 

name_map=setNames(automation_pipeline_names,crf_variable_names)

names(crf_data2)=name_map[names(crf_data2)]


#have to deal with the case where the variable names may be wrong etc.





