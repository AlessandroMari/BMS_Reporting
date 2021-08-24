#This is code for checking and preparing the three main datasets (+ the one with BMS weeks)
#for the analysis. 
#For each dataset, the code first checks if it the dataset is already present in the environment.
#If it is, it checks if some conditions in the dataset are already present.
#If they are not, the code implements them. 
#Code developed by Alessandro Mari

#loading packages
library(here)
library(data.table)
library(lubridate)

#### m_site check ####

#checking if the datatable m_visit already exists
if(exists("m_site") == TRUE){
  
#checking if m_site is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(m_site) == FALSE){ m_site <- as.data.table(m_site) }

}

#### bms_week check #### 

#checking if the datatable bms_weeks already exists and importing it if does not
if(exists("bms_weeks") == FALSE){
#importing the excel file with the middle day of each butterfly monitoring season weeks
bms_weeks <- fread(here("inst","BMS_weeks.csv"))
bms_weeks$MIDDLE_WEEK <- format(as.Date(bms_weeks$MIDDLE_WEEK, format = "%d/%m/%Y"), "%Y-%m-%d")
setnames(bms_weeks, "MIDDLE_WEEK", "VISIT_WEEK")
}



#### m_visit check #### 

#checking if the datatable m_visit already exists
if(exists("m_visit") == TRUE){
  
#checking if m_visit is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(m_visit) == FALSE){ b_count <- as.data.table(m_visit) }
  
#checking if the MONTH and DAY columns were already added to m_visit
if(sum(c("MONTH", "DAY") %in% colnames(m_visit)) != 2){
#converting VISIT_DATE in m_visit to date class, and splitting dates in to MONTH, DAY, YEAR variables
m_visit$VISIT_DATE <- ymd(m_visit$VISIT_DATE)
m_visit[ , MONTH := month(VISIT_DATE)][ , c("DAY", "YEAR") := .(mday(VISIT_DATE), 
                                                                year(VISIT_DATE))]
#assigning a visit week (middle day of the week) to each visit date in m_visit
m_visit <- merge(m_visit, bms_weeks, by = c("MONTH", "DAY"))
}
}



#### b_count check ####

#checking if the datatable b_count already exists
if(exists("b_count") == TRUE){
  
#checking if b_count is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(b_count) == FALSE){ b_count <- as.data.table(b_count) }

#checking if the MONTH and DAY columns were already added to b_count
if(sum(c("MONTH", "DAY") %in% colnames(b_count)) != 2){
#converting VISIT_DATE in b_count to date class, and splitting dates in to MONTH, DAY, YEAR variables
b_count$VISIT_DATE <- ymd(b_count$VISIT_DATE)
b_count[ , MONTH := month(VISIT_DATE)][ , c("DAY", "YEAR") := .(mday(VISIT_DATE), 
                                                                year(VISIT_DATE))]
#assigning a visit week (middle day of the week) to each visit date in b_count 
b_count <- merge(b_count, bms_weeks[,2:4], by = c("MONTH", "DAY"))
}
}