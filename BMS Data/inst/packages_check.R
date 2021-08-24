#This is code for checking if all the required packages are installed. If not, they get installed
#Code developed by Alessandro Mari in July 2021.
#R version 4.0.0

if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
if("openxlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("openxlsx")}
if("geonames" %in% rownames(installed.packages()) == FALSE) {install.packages("geonames")}
if("giscoR" %in% rownames(installed.packages()) == FALSE) {install.packages("giscoR")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
if("gsubfn" %in% rownames(installed.packages()) == FALSE) {install.packages("gsubfn")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}
if("tmap" %in% rownames(installed.packages()) == FALSE) {install.packages("tmap")}
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
if("writexl" %in% rownames(installed.packages()) == FALSE) {install.packages("writexl")}