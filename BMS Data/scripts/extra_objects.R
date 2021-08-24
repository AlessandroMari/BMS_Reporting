#This is code for making 
#- donuts charts relative to habitat, land tenure and management information of 
#  the transects in the last year of monitoring
#- an excel file with the altitude of each transects relative to the last year of 
#  monitoring
#- the european map with the BMS country highlighted
#Code developed by Alessandro Mari in July 2021.
#R version 4.0.0

#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(data.table)
library(geonames)
library(ggplot2)
library(writexl)


# -m_visit datatable with columns: "SITE_ID", "LON", "LAT", "YEAR"
# -m_site datatable with  columns: "SITE_ID", "PRINCIPAL_HABITAT_TERM", "PRINCIPAL_LAND_TENURE_TERM",
#                                  "PRINCIPAL_LAND_MANAGEMENT_TERM"

#checking if m_visit is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(m_visit) == FALSE){ m_visit <- as.data.table(m_visit) }
#checking if m_site is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(m_site) == FALSE){ m_site <- as.data.table(m_site) }

extra_path <- here("graphs/extra")  #saving path

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#last year of monitoring
year <- tail(year_unq,1)
#subsetting m_visit for the last year of recording and for the interested variables
m_visit2 <- unique(m_visit[, c("SITE_ID", "LON", "LAT", "YEAR")])[order(YEAR)][YEAR==year]


#### Transects' elevation #### 

#setting the username for using the geonames function for retrieve elevation
options(geonamesUsername="ale_seas")

#creating the ELEVATION column
m_visit2$ELEVATION <- 0
#computing ELEVATION for each transect's coordinates
for(i in 1:dim(m_visit2)[1]){
  m_visit2$ELEVATION[i] <- GNsrtm3(m_visit2$LAT[i], m_visit2$LON[i])[[1]]
} 
#printing m_visit2
print(m_visit2)

#writing m_visit2 with the elevation information in an excel file
write_xlsx(m_visit2, paste(here("graphs/extra"), "/transects", year, "_elevation.xlsx", sep=""))


#### Habitat donut chart ####

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#last year of monitoring
year <- tail(year_unq,1)
#subsetting m_visit for the last year of recording and for the interested variables
m_visit2 <- m_visit[YEAR==year]

setkey(m_visit2, SITE_ID)
setkey(m_site, SITE_ID)
#merging the species_num and my_visit datasets
tr_habitat <- merge(unique(m_visit2[,5]), m_site[, c(3, 15)])

#library(writexl)
#write_xlsx(tr_info[,-c(4)], paste(here("excel_files"), "/transects2020_info.xlsx", sep=""))

tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Ecotones"]$PRINCIPAL_HABITAT_TERM <- "Ecotones"
tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Urban"]$PRINCIPAL_HABITAT_TERM <- "Urbano"
tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Wetlands"]$PRINCIPAL_HABITAT_TERM <- "Wetlands"
tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Other"]$PRINCIPAL_HABITAT_TERM <- "Other"
tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Forest"]$PRINCIPAL_HABITAT_TERM <- "Forest"
tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Grassland"]$PRINCIPAL_HABITAT_TERM <- "Grassland"
tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Agricultural"]$PRINCIPAL_HABITAT_TERM <- "Agricultural"
tr_habitat[PRINCIPAL_HABITAT_TERM %like% "Heathland"]$PRINCIPAL_HABITAT_TERM <- "Heathland"

tr_habitat <- tr_habitat[ , .N, keyby = PRINCIPAL_HABITAT_TERM ][, prop := N*100/sum(N)]
tr_habitat <- tr_habitat[order(N, decreasing = TRUE)]


tr_habitat <- tr_habitat[, csum := rev(cumsum(rev(prop)))][, pos := prop/2 + lead(csum, 1)]
tr_habitat <- tr_habitat[, pos := if_else(is.na(pos), prop/2, pos)]

hab_name <- "habitat_donut.png"

habitat_plot <- ggplot(tr_habitat, 
                       aes(x = 2, y = prop, fill = fct_inorder(PRINCIPAL_HABITAT_TERM))) +
  geom_bar(stat = "identity", color = "grey28") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = pos, label = paste(round(prop,1), "%", sep="")), color = "black", size=4.2)+
  scale_fill_brewer(palette = "PuRd") +
  theme_void() +
  labs(fill = "Principal habitat") +
  xlim(0.5, 2.5)  +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=10)) +  #change legend text font size 
  ggsave(hab_name, path = extra_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

print(habitat_plot)
print(paste("Name of the plot:", hab_name))

#### Land tenure donut chart ####

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#last year of monitoring
year <- tail(year_unq,1)
#subsetting m_visit for the last year of recording and for the interested variables
m_visit2 <- m_visit[YEAR==year]

setkey(m_visit2, SITE_ID)
setkey(m_site, SITE_ID)
#merging the species_num and my_visit datasets
tr_tenure <- merge(unique(m_visit2[,5]), m_site[, c(3, 19)])

tr_tenure[PRINCIPAL_LAND_TENURE_TERM %like% "Public"]$PRINCIPAL_LAND_TENURE_TERM <- "Public"
tr_tenure[PRINCIPAL_LAND_TENURE_TERM %like% "Farming"]$PRINCIPAL_LAND_TENURE_TERM <- "Farming"
tr_tenure[PRINCIPAL_LAND_TENURE_TERM %like% "Nature Conservation"]$PRINCIPAL_LAND_TENURE_TERM <- 
  "Nature Conservation"
tr_tenure[PRINCIPAL_LAND_TENURE_TERM %like% "Private Person"]$PRINCIPAL_LAND_TENURE_TERM <- 
  "Private Person"
tr_tenure[PRINCIPAL_LAND_TENURE_TERM %like% "Forestry"]$PRINCIPAL_LAND_TENURE_TERM <- 
  "Forestry"



tr_tenure <- tr_tenure[ , .N, keyby = PRINCIPAL_LAND_TENURE_TERM ][, prop := N*100/sum(N)]
tr_tenure <- tr_tenure[order(N, decreasing = TRUE)]

tr_tenure <- tr_tenure[, csum := rev(cumsum(rev(prop)))][, pos := prop/2 + lead(csum, 1)]
tr_tenure <- tr_tenure[, pos := if_else(is.na(pos), prop/2, pos)]

tenure_name <- "tenure_donut.png"

tenure_plot <- ggplot(tr_tenure, 
                      aes(x = 2, y = prop, fill = fct_inorder(PRINCIPAL_LAND_TENURE_TERM))) +
  geom_bar(stat = "identity", color = "grey28") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = pos, label = paste(round(prop,1),"%", sep="")), color = "black", size=4.0)+
  scale_fill_brewer(palette = "PuRd") +
  theme_void() +
  labs(fill = "Land tenure") +
  xlim(0.5, 2.5)  +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=10)) +  #change legend text font size 
  ggsave(tenure_name, path = extra_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

print(tenure_plot)
print(paste("Name of the plot:", tenure_name))

#### Land management donut chart ####

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#last year of monitoring
year <- tail(year_unq,1)
#subsetting m_visit for the last year of recording and for the interested variables
m_visit2 <- m_visit[YEAR==year]

setkey(m_visit2, SITE_ID)
setkey(m_site, SITE_ID)
#merging the species_num and my_visit datasets
tr_manage <- merge(unique(m_visit2[,5]), m_site[, c(3, 23)])
setnames(tr_manage, "PRINCIPAL_LAND_MANAGEMENT_TERM", "MANAGEMENT")

tr_manage[MANAGEMENT %like% "Pest management"]$MANAGEMENT <- "Pest management"
tr_manage[MANAGEMENT %like% 
            "Forestry"]$MANAGEMENT <- "Forestry"
tr_manage[MANAGEMENT %like% 
            "No-management"]$MANAGEMENT <- "No-management"
tr_manage[MANAGEMENT %like% 
            "Grazing"]$MANAGEMENT <- "Grazing"
tr_manage[MANAGEMENT 
          %like% "Mowing"]$MANAGEMENT <- "Mowing"



tr_manage <- tr_manage[ , .N, keyby = MANAGEMENT ][, prop := N*100/sum(N)]
tr_manage <- tr_manage[order(N, decreasing = TRUE)]

tr_manage <- tr_manage[, csum := rev(cumsum(rev(prop)))][, pos := prop/2 + lead(csum, 1)]
tr_manage <- tr_manage[, pos := if_else(is.na(pos), prop/2, pos)]

manage_name <- "management_donut.png"

manage_plot <- ggplot(tr_manage, 
                      aes(x = 2, y = prop, fill = fct_inorder(MANAGEMENT))) +
  geom_bar(stat = "identity", color = "grey28") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = pos, label = paste(round(prop,1),"%", sep="")), color = "black", size=4.0)+
  scale_fill_brewer(palette = "PuRd") +
  theme_void() +
  labs(fill = "Land management") +
  xlim(0.5, 2.5)  +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=10)) +  #change legend text font size 
  ggsave(manage_name, path = extra_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

print(manage_plot)
print(paste("Name of the plot:", manage_name))
