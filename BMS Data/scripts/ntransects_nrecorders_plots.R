#This is code for producing plots relative to the number of transects and recorders in 
#across the BMS years. Code developed by Alessandro Mari in july 2021.
#R version 4.0.0

#Requirements: 
# m_visit datatable with variables/columns: SITE_ID, RECORDER_NAME, YEAR
#Check the "BMS_data_requirement.txt" file located in the main folder for an example of columns'
#value.

#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(data.table)
library(ggplot2)

#checking if m_visit is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(m_visit) == FALSE){ b_count <- as.data.table(m_visit) }

#theme for plot's axis
mytheme <- theme(    
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size=16),
  axis.text.y = element_text(size=16))

var_path <- here("graphs/tr-rec-vis")   #saving path

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



#### Number of transects in each BMS year ####

#number of unique transects in each year
tr_year_unq <- m_visit[, .(N_TRANSECTS = uniqueN(SITE_ID)), by=YEAR][order(YEAR)]

tr_name <- "ntransects_year.png"   #name of the plot

#plot
tr_plot <- ggplot(data = tr_year_unq, aes(x = YEAR, y = N_TRANSECTS)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat="identity", fill = "#aa0052") +       # "aa0052" for the bar filling color
  labs(x = "\nyear", y = "number of transects\n") +
  mytheme 

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <- na.omit(ggplot_build(tr_plot)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks)+1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
tr_plot <- tr_plot +
  ylim(0, tail(gg_breaks,1)) +
  ggsave(tr_name, path = var_path,
         width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(tr_plot)
print(paste("Name of the plot:", tr_name))
tr_year_unq


#### Number of recorders in each BMS year ####

#number of unique recorder names in each year
rec_year_unq <- m_visit[RECORDER_NAME != ""][, .(N_RECORDERS = uniqueN(RECORDER_NAME)), by =
                                 YEAR][order(YEAR)]

rec_name <- "nrecorders_year.png"   #name of the plot

#plot
rec_plot <-
  ggplot(data = rec_year_unq, aes(x = YEAR, y = N_RECORDERS)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  geom_bar(stat = "identity", fill = "#aa0052") +      # "aa0052" for the bar filling color
  labs(x = "\nyear", y = "number of recorders\n") +
  mytheme

#adding an extra break to the y-axis:
#extracting the breaks from the plot object
gg_breaks <-  na.omit(ggplot_build(rec_plot)$layout$panel_params[[1]]$y$breaks)
#calculating the break width
diff <- gg_breaks[2] - gg_breaks[1]
#adding the extra break to gg_breaks
gg_breaks[length(gg_breaks) + 1] <- gg_breaks[length(gg_breaks)] + diff

#changing the the y-axis breaks and saving the plot
rec_plot <- rec_plot +
  ylim(0, tail(gg_breaks, 1)) +
  ggsave(
    rec_name, path = var_path, width = 17, height = 14, units = "cm", dpi = 1000)

#printing the plot, the name of the plot and the data content
print(rec_plot)
print(paste("Name of the plot:", rec_name))
rec_year_unq
