#This is code to produce the main figures for the BMS report production 
#Code developed by Alessandro Mari in July 2021.
#R version 4.0.0

#Note: every BMS dataset has to be adapted to be used in the next scripts. Required variables/columns
#can be found in the "BMS_data_requirement.txt" file located in the main folder.

#Checking if the package "here" is installed. You can remove the following code line once run once
if("here" %in% rownames(installed.packages()) == FALSE) {install.packages("here")}
#Loading the package "here"
library(here)

###How the function "here()" works inside a R project: 
###here("name of the folder in which the file is found", "name of the file")

#sourcing the script the check if the required packages are installed. If not, they get installed
source(here("inst", "packages_check.R"))

##### Importing and preparing the BMS data #####

#Define the desired country as parameter. This parameter will be used for selecting the desired 
#GISCO EU country shapefile (background map) over which the transects will be drawn. 
country <- c("Italy")
#Set the language (ex. "Italian") for the plot standard axis labels (ex. for months names)
Sys.setlocale("LC_TIME", "Italian")
#Sourcing the R script that reads and prepares the BMS (ITBMS in this case) data
source(here("scripts","ITBMS_data_prep.R"))

#Checking the three main datasets for the analysis. If not occurring, these conditions 
#are implemented: conversion to datatable format + adding of the MONTH and DAY columns +
#adding the column VISIT_WEEK for the information on the visit monitoring week 
source(here("inst", "BMS_data_check.R"))
 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



#### Scripts sourcing for figures making ####

#"ntransects_nrecorders_plots.R" is the script for making the plots relative to the number of 
#transects and recorders.
#saving folder: "graphs/tr-rec-vis"
source(here("scripts", "ntransects_nrecorders_plots.R"))


#"nvisits_plots.R" is the script for making the plots relative to the number of visits.
#saving folder: "graphs/tr-rec-vis"
source(here("scripts", "nvisits_plots.R"))


#"ncounts_plots.R" is the script for making the plots relative to the number of counted
#individuals.
#saving folder: "graphs/counts"
source(here("scripts", "ncounts_plots.R"))


#"nspecies_plots.R" is the script for making the plots relative to the number of detected
#species.
#saving folder: "graphs/species"
source(here("scripts", "nspecies_plots.R"))


#"transectmaps.R" is the script for making the different types of transect maps
#saving folder: "graphs/maps"
source(here("scripts", "transects_maps.R"))


#"species_counts_list.R" is the script for making:
#-the image with the names and counts of the 10 most and 10 less common species in the last
#three years of the BMS monitoring 
#-the image with the names and counts of all the species counts in the last year of the BMS 
#monitoring 
#saving folder: "graphs/species"
source(here("scripts","species_counts_list.R"))


#"recorder_list.R" is the script for making the image of the recorder/site name list 
#relative to the last year of the BMS monitoring  
#saving folder: "graphs/tr-rec-vis"
source(here("scripts", "recorders_list.R"))


#"extra_objects.R" is the script for making 
#- an excel file with the altitude of each transects relative to the last year of 
#  monitoring
#- donuts charts relative to habitat, land tenure and management information of 
#  the transects in the last year of monitoring
#saving folder: "graphs/extra"
source(here("scripts", "extra_objects.R"))


