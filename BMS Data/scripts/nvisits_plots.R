#This is code for producing plots relative to the number of visits/monitorings in one 
#or across all the BMS years. Code developed by Alessandro Mari in july 2021.
#R version 4.0.0


#Requirements: 
# m_visit datatable with variables/columns: SAMPLE_ID, SITE_ID, YEAR, VISIT_WEEK 
#Check the "BMS_data_requirement.txt" file located in the main folder for an example of columns'
#value.

#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(data.table)
library(ggplot2)

#Checking m_visit dataset for the analysis. If not occurring, these conditions 
#are implemented: conversion to datatable format + adding of the MONTH and DAY columns +
#adding the column VISIT_WEEK for the information on the visit monitoring week 
source(here("inst", "BMS_data_check.R"))

#theme for plot's axis
mytheme <- theme(    
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size=16),
  axis.text.y = element_text(size=16))

vis_path <- here("graphs/tr-rec-vis")   #saving path

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 




#### Avg number of visits per week in each BMS year ####

#number of visits for each visit week/year combination 
v_wk_year_unq <- m_visit[, .(VISITS_WK_UNQ = uniqueN(SAMPLE_ID)), by=c("YEAR", "VISIT_WEEK")]
#average number and standard deviation of visits per week in each year
v_wk_year_avg <- v_wk_year_unq[,.(AVG_VISITS_WK = round(mean(VISITS_WK_UNQ),1),
                                  SD_VISITS_WK = round(sd(VISITS_WK_UNQ),1)),
                               by = YEAR][order(YEAR)]


v_name0 <- paste("avg_visits_wk_year.png", sep="")  #name of the plot
#plot
v_plot0 <-  ggplot(data = v_wk_year_avg, aes(x = round(YEAR,0), y = AVG_VISITS_WK)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +   # "aa0052" for the bar filling color
  geom_errorbar(aes(ymin=AVG_VISITS_WK - SD_VISITS_WK, ymax = AVG_VISITS_WK + SD_VISITS_WK),
                width=.2, position=position_dodge(.9)) +
  labs(x = "\nyear", y = "average number of visits per week\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(v_plot0)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
v_plot0 <- v_plot0 +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(v_name0, path = vis_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(v_plot0)
print(paste("Name of the plot:", v_name0))
v_wk_year_avg



#### Avg number of visits per transect in each BMS year ####

#number of visits for each transect/year combination 
v_tr_year_unq <- m_visit[, .(VISITS_UNQ = uniqueN(SAMPLE_ID)), 
                         by=c("YEAR", "SITE_ID")]
#average number and standard deviation of visits per transect in each year
v_tr_year_avg <- v_tr_year_unq[, .(AVG_VISITS_TR = round(mean(VISITS_UNQ),1),
                                   SD_VISITS_TR = round(sd(VISITS_UNQ),1)),
                                   by=c("YEAR")][order(YEAR)]


v_name1 <- paste("avg_visits_tr_year.png", sep="")  #name of the plot
#plot
v_plot1 <-  ggplot(data = v_tr_year_avg, aes(x = YEAR, y = AVG_VISITS_TR)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +   # "aa0052" for the bar filling color
  geom_errorbar(aes(ymin=AVG_VISITS_TR - SD_VISITS_TR, ymax = AVG_VISITS_TR + SD_VISITS_TR),
                width=.2, position=position_dodge(.9)) +
  geom_hline(yintercept = 10, linetype= "longdash", color = "#563975") +
  labs(x = "\nyear", y = "average number of visits per transect\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(v_plot1)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
v_plot1 <- v_plot1 +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(v_name1, path = vis_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(v_plot1)
print(paste("Name of the plot:", v_name1))
v_tr_year_avg


#### Number of visits in each week of the last BMS year ####

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#last year of monitoring
year <- tail(year_unq,1)

#number of visits for each visit week in the last year of monitoring
v_wk_year <- m_visit[YEAR %in% year,][, .(N_VISITS_WK = uniqueN(SAMPLE_ID)), 
                                           by=c("YEAR", "VISIT_WEEK")]
  
v_name2 <- paste("nvisits_week_",year,".png", sep = "")  #name of the plot

#plot
v_plot2 <- ggplot(data = v_wk_year, aes(x = as.Date(VISIT_WEEK), y = N_VISITS_WK)) + 
  theme_bw() +
  scale_x_date(breaks = "month", date_labels = "%b") +   #setting months on the x-axis
  scale_y_continuous(breaks = scales::pretty_breaks())+
  labs(y = "number of visits \n", 
       x = "")  +
  geom_point(colour = "black") +
  #the regression line is made with "loess" method, as it is used on less than 1000 points
  geom_smooth(method = "loess", color="#aa0052", se=F) +  # "aa0052" for the line color
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(v_plot2)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
v_plot2 <- v_plot2 +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(v_name2, path = vis_path,
         width = 20, height = 12.5, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(v_plot2)
print(paste("Name of the plot:", v_name2))
v_wk_year


#### Number of visits in each BMS year ####

#number of visits in each year
v_year_sum <- m_visit[, .(N_VISITS = uniqueN(SAMPLE_ID)), by="YEAR"][order(YEAR)]

v_name3 <- paste("nvisits_year.png", sep="")  #name of the plot
#plot
v_plot3 <-  ggplot(data = v_year_sum, aes(x = YEAR, y = N_VISITS)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +   # "aa0052" for the bar filling color
  labs(x = "\nyear", y = "number of visits\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(v_plot3)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra value to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
v_plot3 <- v_plot3 +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(v_name3, path = vis_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(v_plot3)
print(paste("Name of the plot:", v_name3))
v_year_sum
