#This is code for producing plots relative to the number of detected species in one 
#or across all the BMS years. Code developed by Alessandro Mari in july 2021.
#R version 4.0.0


#Requirements: 
# b_count datatable with variables/columns: SITE_ID, SPECIES, YEAR, VISIT_WEEK
#Check the "BMS_data_requirement.txt" file located in the main folder for an example of columns'
#value.

#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(data.table)
library(ggplot2)

#Checking b_count dataset for the analysis. If not occurring, these conditions 
#are implemented: conversion to datatable format + adding of the MONTH and DAY columns +
#adding the column VISIT_WEEK for the information on the visit monitoring week 
source(here("inst", "BMS_data_check.R"))

#theme for plot's axis
mytheme <- theme(    
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size=16),
  axis.text.y = element_text(size=16))

sp_path <- here("graphs/species")   #saving path

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 




#### Avg number of detected species per week in each BMS year ####

#number of detected species for each visit week/year combination 
sp_wk_year_unq <- b_count[, .(SPECIES_WK_UNQ = uniqueN(SPECIES)), 
                             by=c("YEAR", "VISIT_WEEK")]
#average number and standard deviation of detected species per week in each year
sp_wk_year_avg <- sp_wk_year_unq[ , .(AVG_SPECIES_WK = round(mean(SPECIES_WK_UNQ),0),
                                      SD_SPECIES_WK = round(sd(SPECIES_WK_UNQ),0)),
                                  by = YEAR][order(YEAR)]


sp_name0 <- paste("avg_species_wk_year.png", sep="")   #name of the plot
#plot
sp_plot0 <-  ggplot(data = sp_wk_year_avg, aes(x = YEAR, y = AVG_SPECIES_WK)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +   # "aa0052" for the bar filling color
  geom_errorbar(aes(ymin=AVG_SPECIES_WK - SD_SPECIES_WK, ymax = AVG_SPECIES_WK + SD_SPECIES_WK),
                width=.2, position=position_dodge(.9)) +
  labs(x = "\nyear", 
       y = "average number of species per week\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(sp_plot0)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
sp_plot0 <- sp_plot0 +
  ylim(c(0, tail(gg_breaks,1))) +
  ggsave(sp_name0, path = sp_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(sp_plot0)
print(paste("Name of the plot:", sp_name0))
sp_wk_year_avg


#### Avg number of detected species per transect in each BMS year ####

#number of detected species for each transect/year combination 
sp_tr_year_unq <- b_count[, .(SPECIES_UNQ = uniqueN(SPECIES)), 
                          by=c("YEAR", "SITE_ID")]
#average number and standard deviation of detected species per transect in each year
sp_tr_year_avg <- sp_tr_year_unq[, .(AVG_SPECIES_TR = round(mean(SPECIES_UNQ),0),
                                     SD_SPECIES_TR = round(sd(SPECIES_UNQ),0)),
                           by=c("YEAR")][order(YEAR)]


sp_name1 <- paste("avg_species_tr_year.png", sep="")   #name of the plot
#plot
sp_plot1 <-  ggplot(data = sp_tr_year_avg, aes(x = YEAR, y = AVG_SPECIES_TR)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +  # "aa0052" for the bar filling color
  geom_errorbar(aes(ymin=AVG_SPECIES_TR - SD_SPECIES_TR, ymax = AVG_SPECIES_TR + SD_SPECIES_TR),
                width=.2, position=position_dodge(.9)) +
  labs(x = "\nyear", y = "average number of species per transect\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(sp_plot1)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
sp_plot1 <- sp_plot1 +
  scale_y_continuous(breaks=gg_breaks) +
  ggsave(sp_name1, path = sp_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(sp_plot1)
print(paste("Name of the plot:", sp_name1))
sp_tr_year_avg


#### Number of detected species in each week of the last BMS year ####

#vector with all the years in which visits where made
year_unq <- sort(unique(b_count$YEAR))
#last year of monitoring
year <- tail(year_unq,1)

#number of detected species for each visit week in the last year of monitoring
sp_year_wk_unq <- b_count[YEAR %in% year][, .(SPECIES_UNQ = uniqueN(SPECIES)), 
                             by=c("YEAR", "VISIT_WEEK")]

sp_name2 <- paste("nspecies_week_",year,".png", sep = "")   #name of the plot
#plot
sp_plot2 <- ggplot(data = sp_year_wk_unq, aes(x = as.Date(VISIT_WEEK), y = SPECIES_UNQ)) + 
  theme_bw() +
  scale_x_date(breaks = "month", date_labels = "%b") +   #setting months on the x-axis
  scale_y_continuous(breaks = scales::pretty_breaks())+
  labs(y = paste("number of species","\n", 
                 sep=""),
       x = "")   +
  geom_point(colour = "black") +
  #the regression line is made with "loess" method, as it is used on less than 1000 points
  geom_smooth(method = "loess", color="#aa0052", se=F) +   # "aa0052" for the line color
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(sp_plot2)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
sp_plot2 <- sp_plot2 +
  scale_y_continuous(breaks=gg_breaks) +
 # ylim(0, tail(gg_breaks, 1)) +
  ggsave(sp_name2, path = sp_path,
         width = 20, height = 12.5, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(sp_plot2)
print(paste("Name of the plot:", sp_name2))
sp_year_wk_unq


#### Number of detected species in each BMS year ####

#number of detected species in each year
sp_year <- b_count[, .(N_SPECIES = uniqueN(SPECIES)), 
                      by="YEAR"][order(YEAR)]

sp_name3 <- paste("nspecies_year.png", sep="")  #name of the plot
#plot
sp_plot3 <-  ggplot(data = sp_year, aes(x = YEAR, y = N_SPECIES)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +  # "aa0052" for the bar filling color
  labs(x = "\nyear", y = "number of species\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(sp_plot3)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra value to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
#if the y-axis breaks are set incorrectly turn off line 198 and turn on line 199 (or viceversa)
sp_plot3 <- sp_plot3 +
  scale_y_continuous(breaks=gg_breaks) +
  ggsave(sp_name3, path = sp_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(sp_plot3)
print(paste("Name of the plot:", sp_name3))
sp_year


#### Boxplot: distribution of the number of species of each transects in each BMS year ####

#number of detected species for each transect/year combination 
sp_tr_yr <- b_count[, .(N_SPECIES = uniqueN(SPECIES)),
                            by = c("YEAR", "SITE_ID")][order(YEAR)]

#converting the YEAR column to an object of factor class
Year <- factor(sp_tr_yr$YEAR)

sp_name4 <- paste("species_tr_year_box.png", sep="")  #name of the plot

#plot
ggbox <- ggplot(sp_tr_yr, aes(x=YEAR, y=N_SPECIES, group=Year)) +
  geom_boxplot(fill="#aa0052") +   # "aa0052" for the line color
  geom_jitter(color="black", size=0.4, alpha=0.9) +   #points for every single transect value
  theme_classic() +
  labs(x="\nyear",y="number of species\n") +
  mytheme +
  theme(legend.position = "none")

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(ggbox)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra value to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
ggbox <- ggbox +
  ylim(c(0, tail(gg_breaks,1))) +
  ggsave(sp_name4 , path = here("graphs/species"),
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot and the name of the plot 
print(ggbox)
print(paste("Name of the plot:", sp_name4))
  

