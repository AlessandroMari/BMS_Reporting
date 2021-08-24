#This is code to prepare the Italian BMS data for the analysis performed in the other scripts. 
#Code developed by Alessandro Mari in july 2021.
#R version 4.0.0

#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(tidyverse)
library(data.table)
library(gsubfn)



###How the function "here()" works: 
###here("name of the folder in which the file is found", "name of the file")


##count datatables correspond to occurences datasets

#datatable with species counts information for 2017
count_2017 <- fread(here("data", "2017_Combinedataforallrecorders_Occurrences_20210512102454.csv"))  
#datatable with species counts information for 2018
count_2018 <- fread(here("data", "2018_Combinedataforallrecorders_Occurrences_20210512101644.csv")) 
#datatable with species counts information for 2019
count_2019 <- fread(here("data", "2019_Combinedataforallrecorders_Occurrences_20210512101602.csv")) 

##visit datatables correspond to samples datasets

#datatable with visits information for 2017
visit_2017 <- fread(here("data", "2017_Combinedataforallrecorders_Samples_20210512102454.csv"))  
#datatable with visits information for 2018
visit_2018 <- fread(here("data", "2018_Combinedataforallrecorders_Samples_20210512102409.csv")) 
#datatable with visits information for 2019
visit_2019 <- fread(here("data", "2019_Combinedataforallrecorders_Samples_20210512101602.csv"))  


#sites: original datatable with information on each site
sites <- fread(here("data", "fullsites_data_download_Italy_12052021.csv"))

#m_sites: datatable with information on each site
m_site <- sites
setnames(m_site, gsub(" ", "_", names(m_site))) #filling the space in column names with "_"
setnames(m_site, toupper(names(m_site))) #uppercase letters for column names
#changing the column names of the m_site datatable so that it will be consistent both with the other
#datatables and with the rbms package notation
setnames(m_site, c("SITE_ID", "SITE_CODE", "NO._OF_SECTIONS"), c("SITE_CODE", "SITE_ID", 
                                                                 "NUMBER_OF_SECTIONS"))
#splitting the coordinates column (SREF) in two different columns for Latitude and Longitude
m_site <-  m_site %>% separate(SREF, into = c("LAT", "LON"), sep = " ") %>% 
  mutate(LAT = as.numeric(gsubfn(".", list("N" = "", "," = ""), LAT))) %>% 
  mutate(LON = as.numeric(gsub("E", "", LON))) %>% as.data.table()


#datatable with italian species names for the validation of species names in counts datatable
sp_names <- setDT(read.xlsx(here("inst", "it_species_names.xlsx")))


##b_count: single datatable with all the species counts for all the years
b_count <- rbind(count_2017, count_2018, count_2019)
setnames(b_count, gsub(" ", "_", names(b_count))) #filling the space in column names with "_"
setnames(b_count, toupper(names(b_count))) #uppercase letters for column names
#changing the column names of the b_count datatable so that it will be consistent both with the other
#datatables and with the rbms package notation
setnames(b_count, c("SITE_CODE", "SECTION_NO.", "DATE", "SPECIES","PREFERRED", "ABUNDANCE_COUNT"), 
         c("SITE_ID","SECTION_NUMBER", "VISIT_DATE", "EN_SPECIES", "SPECIES", "COUNT"))
#converting VISIT_DATE to date class, and splitting dates in to MONTH, DAY, YEAR variables
b_count$VISIT_DATE <- format(as.Date(b_count$VISIT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")

#grouping Leptidea sp. species in a single complex Leptidea sinapis/juvernica/reali
b_count[SPECIES %like% "Leptidea"]$SPECIES <- "Leptidea sinapis/juvernica/reali"

##subsetting "b_count" dt for only those species that are listed in "sp_names" dt with Italian 
##species names
b_count_ok <- b_count[SPECIES %in% sp_names$SPECIES,]

##m_visit: single datatable with all the visits information for all the years
m_visit <- rbind(visit_2017, visit_2018, visit_2019)
setnames(m_visit, gsub(" ", "_", names(m_visit))) #filling the space in column names with "_"
setnames(m_visit, toupper(names(m_visit))) #uppercase letters for column names
#changing the column names of the m_visit datatable so that it will be consistent both with the other
#datatables and with the rbms package notation
setnames(m_visit, "SITE_CODE", "SITE_ID")
#converting VISIT_DATE to date class, and splitting dates in to MONTH, DAY, YEAR variables
m_visit$VISIT_DATE <- format(as.Date(m_visit$VISIT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")

#splitting the coordinates column (SITE_GRID_REF.) in two different columns for Latitude and Longitude
m_visit <- m_visit %>% separate(SITE_GRID_REF., into = c("LAT", "LON"), sep = " ") %>% 
  mutate(LAT = as.numeric(gsubfn(".", list("N" = "", "," = ""), LAT))) %>% 
  mutate(LON = as.numeric(gsub("E", "", LON))) %>% as.data.table()
#excluding 3 incorrect samples (SAMPLE_IDs: 6180680, 6136499, 8411185)
m_visit <- m_visit[!SAMPLE_ID %in% c("6180680", "6136499", "8411185"),]

#fixing the correct names of some recorders
m_visit[RECORDER_NAME == "CONTU, KAREN"]$RECORDER_NAME <- "Contu Karen"
m_visit[RECORDER_NAME == "Garlanda, Una"]$RECORDER_NAME <- "Una Garlanda"
m_visit[RECORDER_NAME == "IOLAS, Associazione" ]$RECORDER_NAME <- "Associazione IOLAS"
m_visit[RECORDER_NAME == "TrovÃ², Paola"]$RECORDER_NAME <- "Trovò Paola"
m_visit[RECORDER_NAME == "Bosio , Gianna" ]$RECORDER_NAME <- "Bosio Gianna"
m_visit[RECORDER_NAME == "Antonio Gennaro" ]$RECORDER_NAME <- "Gennaro, Antonio"
