library(tidyr)
library(haven)
library(lubridate)
library(dplyr)
library(purrr)
library(stringr)
library(readxl)
library('stringdist')
library('text2vec')

crf_data=read_sas('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Data/Derived/a_pop.sas7bdat')


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





