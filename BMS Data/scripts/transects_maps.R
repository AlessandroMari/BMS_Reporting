#This is code for producing the transect maps. Code developed by Alessandro Mari in July 2021.
#R version 4.0.0

#Requirements:
#-b_count datatable with columns: "SITE_ID", "YEAR", "SPECIES", "COUNTS"
#-m_visit datatable with columns: "SITE_ID", "YEAR", "LON", "LAT" 
#-m_site datatable with  columns: "SITE_ID", "LON", "LAT"
#Coordinates have to be in EPSG:4326, WGS84
#Check the "BMS_data_requirement.txt" file located in the main folder for an example of columns'
#value.

#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(data.table)
library(giscoR)
library(sf)
library(tmap)


#Checking b_count, m_visit and m_site datasets for the analysis. If not occurring, these conditions 
#are implemented: conversion to datatable format + adding of the MONTH and DAY columns +
#adding the column VISIT_WEEK for the information on the visit monitoring week 
source(here("inst", "BMS_data_check.R"))

tr_path <- here("graphs/maps")   #saving path

##### Importing the country shapefile from the GISCO repository #####

giscoeu <- gisco_get_nuts(
  year = "2021",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  resolution = "01",
  spatialtype = "RG",
  country = country,
  #  region = "EU",
  nuts_level = "2"
)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



#### Map of the transects in the last BMS year #### 

#The name of the map image will change automatically according how the year is set.
#(ex. if year = 2020 -> "tr_2020_map.png")

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#Here the year is set as the last year of recording as default
year <- tail(year_unq, 1)

#filtering m_visit for the last year of monitoring
my_visit <- m_visit[YEAR == year]
#transects' coordinates in the last year of monitoring
my_coord <- unique(my_visit[,.(SITE_ID,LON,LAT)],by="SITE_ID")
#transforming coordinates in a simple feature object in EPSG:4326
my_coord <- st_as_sf(my_coord, coords = c("LON", "LAT"), crs = 4326)

tm_name <- paste("tr_", year, "_map.png", sep = "")   #name of the map

#background map
map_country <- tm_shape(giscoeu) + tm_fill(col = "aliceblue") + tm_borders(lwd = 0.7)
#adding transect points on the background map
map_country_tr <- map_country + tm_shape(my_coord) +
  tm_dots(size = 0.3, col = "#aa0052", shape = 21, border.col = NA) +
  tm_layout(frame = FALSE)
#saving the map
tmap_save(map_country_tr, paste(tr_path, "/", tm_name, sep = ""),
          dpi = 500)
#printing the map and the name of the map
print(map_country_tr)
print(paste("Name of the map:", tm_name))

  


#### Map of transects in a BMS year with species richness ####

#The name of the map image will change automatically according how the year is set.
#(ex. if year = 2020 -> "tr_species_2020_map.png")

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#Here the year is set as the last year of recording as default
year <- tail(year_unq, 1)


#filtering m_visit and b_count for the last year of monitoring
my_visit <- m_visit[YEAR == year]
my_count <- b_count[YEAR == year]

#datatable with the number of species in each transect
species_num <- my_count[, .(SPECIES_NUMBER = uniqueN(SPECIES)),
                        by = SITE_ID]

#setting the column SITE_ID (transect names) in species_num and my_visit datasets as the key
#for their subsequent merging (to have transect coordinates and species richness in the
#same dataset)
setkey(species_num, SITE_ID)
setkey(my_visit, SITE_ID)
#merging the species_num and my_visit datasets
species_num_tr <- unique(merge(species_num, my_visit[, .(SITE_ID, LON, LAT)]))
#converting species_num_tr to a simple feature object in EPSG:4326
species_num_tr <- st_as_sf(species_num_tr, coords = c("LON", "LAT"), crs = 4326)
#summary of the column SPECIES_NUMBER 
smry <- summary(species_num_tr$SPECIES_NUMBER)
#extracting the maximum species richness among all the transects from the summary
max_sp <- smry[[6]]
#creating breaks for the species richness legend using 1st,2nd and 3rd quartiles. 
#The fist value is set as 0, the last value as maximum species richness "max_sp"
bins <- c(0, round(smry[[2]], 0), round(smry[[3]], 0), round(smry[[5]], 0), max_sp)

#legend palette with 4 colors (4-level legend)
pal <- colorRampPalette(c("white","#FFC5D0","#FB6090","#AA0052"))   
tm_sp_name <- paste("tr_species_", year, "_map.png", sep = "")   #name of the map

#background map
map_country <-
  tm_shape(giscoeu) + tm_fill(col = "aliceblue") + tm_borders(lwd = 0.7)
#adding transect points with species richness information on the background map
map_country_tr_sp <- map_country + tm_shape(species_num_tr) +
  tm_dots( size = 0.5, title = "Species number", col = "SPECIES_NUMBER", breaks = bins,
    palette = pal(4), border.col = "black", border.lwd = 0.5, shape = 21) +
  tm_layout(
    frame = FALSE,   #no frame around the map
    legend.format = 0,
    legend.position = c(0.70, 0.75),
    legend.title.size = 1.5,
    legend.text.size = 1
  )
#saving the map
tmap_save(map_country_tr_sp, paste(tr_path, "/", tm_sp_name, sep = ""), dpi = 500)

#printing the map and the name of the map
print(map_country_tr_sp)
print(paste("Name of the map:", tm_sp_name))



#### Map of transects inside the "m_site" dataset (no visit info)  ####

#Name of the map image: "tr_map.png"

#subsetting m_site to only transects' name and coordinates information
my_coord <- m_site[, .(SITE_ID, LON, LAT)]
#transforming coordinates in a simple feature object in EPSG:4326
my_coord <- st_as_sf(my_coord, coords = c("LON", "LAT"), crs = 4326)

tm_site_name <- "tr_map.png"  #name of the map

#background map
map_country <- tm_shape(giscoeu) + tm_fill(col = "aliceblue") + tm_borders(lwd = 0.7)
#adding transect points with species richness information on the background map
map_country_tr <- map_country + tm_shape(my_coord) +
  tm_dots(size = 0.3, col = "#aa0052", shape = 21, border.col = NA) +
  tm_layout(frame = FALSE)  #no frame around the map

#saving the map
tmap_save(map_country_tr, paste(tr_path, "/", tm_site_name, sep = ""), dpi = 500)

#printing the map and the name of the map
print(map_country_tr)
print(paste("Name of the plot:", tm_site_name))
