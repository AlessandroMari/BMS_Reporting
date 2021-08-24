#This is code for producing plots relative to the number of counted individuals in one 
#or across all the BMS years. Code developed by Alessandro Mari in july 2021.
#R version 4.0.0

#Requirements: 
# b_count datatable with variables/columns: SITE_ID, COUNT, YEAR, VISIT_WEEK
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

count_path <- here("graphs/counts")   #saving path

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 




#### Avg number of counted individuals per week in each BMS year ####

#number of counted individuals for each visit week/year combination 
c_wk_year_unq <- b_count[, .(COUNT_WK_SUM = sum(COUNT)),
                         by = c("YEAR", "VISIT_WEEK")]
#average number and standard deviation of counted individuals per week in each year
c_wk_year_avg <- c_wk_year_unq[, .(AVG_COUNTS_WK = round(mean(COUNT_WK_SUM),0),
                                   SD_COUNTS_WK = round(sd(COUNT_WK_SUM),0)),
                               by = YEAR][order(YEAR)]


c_name0 <- "avg_counts_wk_year.png"   #name of the plot
#plot
c_plot0 <-  ggplot(data = c_wk_year_avg, aes(x = YEAR, y = AVG_COUNTS_WK)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +   # "aa0052" for the bar filling color
  geom_errorbar(aes(ymin=AVG_COUNTS_WK - SD_COUNTS_WK, ymax = AVG_COUNTS_WK + SD_COUNTS_WK),
                width=.2, position=position_dodge(.9)) +
  labs(x = "\nyear", y = "average number of counted individuals per week\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(c_plot0)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
c_plot0 <- c_plot0 +
  ylim(c(0, tail(gg_breaks,1))) +
  ggsave(c_name0, path = count_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(c_plot0)
print(paste("Name of the plot:", c_name0))
c_wk_year_avg



#### Avg number of counted individuals per transect in each BMS year ####

#number of counted individuals for each transect/year combination 
c_tr_year_unq <- b_count[, .(COUNT_SUM = sum(COUNT)), 
                         by=c("YEAR", "SITE_ID")]
#average number and standard deviation of counted individuals per transect in each year
c_tr_year_avg <- c_tr_year_unq[, .(AVG_COUNTS_TR = round(mean(COUNT_SUM),0),
                                   SD_COUNTS_TR = round(sd(COUNT_SUM),0)),
                               by=c("YEAR")][order(YEAR)]

c_name1 <- "avg_counts_tr_year.png"   #name of the plot
#plot
c_plot1 <-  ggplot(data = c_tr_year_avg, aes(x = YEAR, y = AVG_COUNTS_TR)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +  # "aa0052" for the bar filling color
  geom_errorbar(aes(ymin=AVG_COUNTS_TR - SD_COUNTS_TR, ymax = AVG_COUNTS_TR + SD_COUNTS_TR),
                width=.2, position=position_dodge(.9)) +
  labs(x = "\nyear", y = "average number of counted individuals per transect\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(c_plot1)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
c_plot1 <- c_plot1 +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(c_name1, path = count_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(c_plot1)
print(paste("Name of the plot:", c_name1))
c_tr_year_avg



#### Number of counted individuals in each week of the last BMS year ####

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#last year of monitoring
year <- tail(year_unq,1)

#number of counted individuals for each visit week in the last year of monitoring
c_wk_year <- b_count[YEAR %in% year,][, .(N_COUNTS_WK = sum(COUNT)),
                           by = c("YEAR", "VISIT_WEEK")]

c_name2 <- paste("ncounts_week_",year,".png", sep = "")  #name of the plot
#plot
c_plot2 <- ggplot(data = c_wk_year, aes(x = as.Date(VISIT_WEEK), y = N_COUNTS_WK)) + 
  theme_bw() +
  scale_x_date(breaks = "month", date_labels = "%b") +   #setting months on the x-axis
  scale_y_continuous(breaks = scales::pretty_breaks())+
  labs(y = paste("number of counted individuals","\n", 
                 sep=""),
       x = "")   +
  geom_point(colour = "black") +
  #the regression line is made with "loess" method, as it is used on less than 1000 points
  geom_smooth(method = "loess", color="#aa0052", se=F) +   # "aa0052" for the bar filling color
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(c_plot2)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
c_plot2 <- c_plot2 +
# scale_y_continuous(breaks=gg_breaks) +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(c_name2, path = count_path,
         width = 20, height = 12.5, units = "cm", dpi = 1000)
  
#printing the plot, the name of the plot and the data content
print(c_plot2)
print(paste("Name of the plot:", c_name2))
c_wk_year


#### Number of counted individuals per km in each week of the last BMS year ####

#vector with all the years in which visits where made
year_unq <- sort(unique(m_visit$YEAR))
#last year of monitoring
year <- tail(year_unq,1)

#number of counted individuals for each visit week in the last year of monitoring 
c_wk_year <- b_count[YEAR %in% year,][, .(N_COUNTS_WK = sum(COUNT)),
                                      by = c("YEAR", "VISIT_WEEK")]

#merging m_visit with the information on transect lengths (in meters) from m_site dataset 
#(merging key: SITE_ID)
visit_len <- merge(m_visit[YEAR==year][,c(3,5,26,28)], m_site[,c(3,10)], by = "SITE_ID")
#replacing missing length values with the average length value of all the transects
visit_len[which(is.na(`OVERALL_LENGTH_(M)`))]$`OVERALL_LENGTH_(M)` <- 
           round(mean(visit_len$`OVERALL_LENGTH_(M)`, na.rm=T),0)
#sum of transect lengths for each year/visit week combination
visit_len_wk <- visit_len[, .(LENGTH_WK = sum(`OVERALL_LENGTH_(M)`)),
                           by = c("YEAR", "VISIT_WEEK")]

#merging: information on both the sum of transect lengths and the number of counted individuals for 
#each year/visit week combination
count_len_wk <- merge(visit_len_wk, c_wk_year, by=c("YEAR", "VISIT_WEEK"), all.x = TRUE)
#replacing with missing length value with a value of 0
count_len_wk[which(is.na(N_COUNTS_WK))]$COUNT_WK_SUM <- 0
#normalizing each number of counted individuals for each year/visit week combination to 
#a length of 1000m
count_len_wk[, COUNT_KM := round(N_COUNTS_WK * (1000/LENGTH_WK),0)]


c_name3 <- paste("ncounts_week_km_",year,".png", sep = "")  #name of the plot
#plot
c_plot3 <- ggplot(data = count_len_wk, aes(x = as.Date(VISIT_WEEK), y = COUNT_KM)) + 
  theme_bw() +
  scale_x_date(breaks = "month", date_labels = "%b") +   #setting months on the x-axis
  scale_y_continuous(breaks = scales::pretty_breaks())+
  labs(y = paste("number of counted individuals per km \n", sep=""),
       x = "")  +
  geom_point(colour = "black") +
  #the regression line is made with "loess" method, as it is used on less than 1000 points
  geom_smooth(method = "loess", color="#aa0052", se=F) +  # "aa0052" for the line color
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(c_plot3)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
c_plot3 <- c_plot3 +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(c_name3, path = count_path,
         width = 20, height = 12.5, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(c_plot3)
print(paste("Name of the plot:", c_name3))
count_len_wk[,c(2,5)]


#### Number of counted individuals in each BMS year ####

#number of counted individuals in each year
c_year <- b_count[, .(N_COUNT = sum(COUNT)), by="YEAR"][order(YEAR)]


c_name4 <- paste("ncounts_year.png", sep="")  #name of the plot
#plot
c_plot4 <-  ggplot(data = c_year, aes(x = YEAR, y = N_COUNT)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +   # "aa0052" for the bar filling color
  labs(x = "\nyear", y = "number of individuals\n") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(c_plot4)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra value to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
c_plot4 <- c_plot4 +
  ylim(c(0, tail(gg_breaks,1))) +
  ggsave(c_name4, path = count_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(c_plot4)
print(paste("Name of the plot:", c_name4))
c_year
