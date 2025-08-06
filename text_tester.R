


library(tidyr)
library(haven)
library(lubridate)
library(dplyr)
library(purrr)
library(stringr)
library(readxl)
rm(list = ls())






#read in some datasets using the CRF to check approach. 

ie_data=read_sas('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Data/Rawdata/Inclusion_Exclusion.sas7bdat')



#read in the data dictionary, as this has both the text from CRF and also the variable names. 


data_dict=read_excel('Medrio_Data_Dictionary_LIVE_Dexcom_PTL1000505_Test_Drive_IH_Multi_Session_22072025_1630.xlsx',sheet='Forms') #first file. 

code_dict=read_excel('Medrio_Data_Dictionary_LIVE_Dexcom_PTL1000505_Test_Drive_IH_Multi_Session_22072025_1630.xlsx',sheet='Code Lists')

#now break this into two separate DFs, one is the text DF, the other is the variable DF. 

text_dict=data_dict%>%filter(data_dict$`Object Type`=='Text on Form')
var_dict=data_dict%>%filter(data_dict$`Object Type`=='Variable')



#for the text_dict we only need to store the text variable, and the IDs. This may not be used at all since this text seems to be the titles of the individual cells, ie not linked
#to a variable.

text_dict=text_dict%>%select('Form Name','Form Export Name','Form External ID','Text')

#now we work with the variable dictionary to link text to the variable name.

var_dict=var_dict%>%select('Form Name','Form Export Name','Form External ID','Variable Name','Variable Export Name','SAS Label Export','Variable External ID','Label Text on Form')


#to test text distance matching, create new column to fill in for automation pipeline variable, then make small changes. 

for(i in 1:nrow(var_dict)){
  
  var_dict$automation_var[i]=paste(var_dict$`Variable Name`[i],sample(LETTERS,1),sep='')
  var_dict$automation_var2[i]=paste(var_dict$automation_var[i],sample(LETTERS,1),sep='')
  
  
}

#now check the text distance between the real variable and the automation pipeline.
library('stringdist')
library('text2vec')


for(i in 1:nrow(var_dict)){
  
  var_dict$distances[i]=stringdist(var_dict$`Variable Name`[i],var_dict$automation_var2[i],method='jw')
  
}





tokens=word_tokenizer(c(a,b))
it=itoken(tokens)
v=create_vocabulary(it)

dtm=create_dtm(it,vocab_vectorizer(v))
sim2(dtm,method='cosine')


stringdist(a,b,method='jw')







#install.packages("officer")
 

#library('officer')


#start_time <- Sys.time()

#doc=read_docx('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Documents/CRF/PTL1000234_Test_Drive_IH2_CRF_Final.docx')
#end_time <- Sys.time()

#print(end_time-start_time)

#install.packages('textreadr')

library('docxtractr')


my_doc <- read_docx('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Documents/CRF/PTL1000234_Test_Drive_IH2_CRF_Final.docx')
tables <- docx_extract_all_tbls(my_doc)

tables=docx_extract_all_tbls(my_doc, guess_header = TRUE)

#for each table, the row is a variable name (roughly), but for some rows there are multiple subquestions.
#we need to write a function that will split this into multiple rows to assign variable.
#likely can just count number of rows with check boxes, because the names for these aren't related to the text. 

new=tables[[1]][6,]$V1
new1=gsub('FORMCHECKBOX','',new) #remove check box
str_count(new1,'No        Yes')




#now write function that will take the tables, loop through the fields, and break apart if there are multiple sub-variables, and then assign variable names. 


make_names=function(table_entry){
  
  mapped_var=list()
  mapped_var2=list()
  
  #isolated to first table (IE) criteria only at this point
  for(i in 1:10){
    
    #check if the row contains checkboxes. If it does then we need to make the variable name underscore the number of check box entries
    
    if(str_detect(table_entry[i,]$V1,'FORMCHECKBOX') & str_count(table_entry[i,]$V1,'FORMCHECKBOX')>1){
      
      #print('check')
      
      #have to account for extra checkboxes that may occur. 
      temp=gsub('FORMCHECKBOX','',table_entry[i,]$V1) #remove checkbox
      number_rows=str_count(temp,'No        Yes') #note that this only works for our current CRF!!
      #print(number_rows)
      #now, initially make our new variable the first 4 characters of the table entry
      
      for(j in 1:number_rows){
        
        new_list=list()

        new_list[j]=paste(substr(table_entry[i,]$V1,1,4),'_',j)
        
        mapped_var <- append(mapped_var, new_list, after = i)

      }
      #mapped_var[i]=new_list

     }
    
    else{
      #print(table_entry[i,]$V1)
      mapped_var2[i]=substr(table_entry[i,]$V1,1,4)
      
    }
    
    
  }
  total=append(mapped_var,mapped_var2)
  #return(new_list)
  return(total%>%unlist())
  
}

make_names(tables[[2]])















#THIS PART OF THE CODE IS TO CHECK FOR TEXT MATCHING AND DISTANCE METRICS
install.packages('text2vec')
install.packages('stringdist')


library('stringdist')
library('text2vec')


a='subjid'
b='subid'

tokens=word_tokenizer(c(a,b))
it=itoken(tokens)
v=create_vocabulary(it)

dtm=create_dtm(it,vocab_vectorizer(v))
sim2(dtm,method='cosine')


stringdist(a,b,method='jw')


my_vars=c('subid','subjid','sid','subjectid','sbjtu')

new_var='subject'

my_distances=list()

for(i in 1:length(my_vars)){
  my_distances[i]=stringdist(my_vars[i],new_var,method='jw')
  
}













doc_text=docx_summary(doc)


#filter on only table cells, where the actual data is entered into CRF

data_entries=doc_text[doc_text$content_type=='table cell',]


#for those rows that have multiple entries (such as numbered options) we need to split
#the text into the 





#file.exists("your_document.docx")



unzip('Z:/Biostats/Studies/PTL-1000234-G7-Test-Drive-IH2/Documents/CRF/PTL1000234_Test_Drive_IH2_CRF_Final.docx', exdir = "docx_unzipped")

doc <- read_xml("docx_unzipped/word/document.xml")








































































