#This is code for writing the recorder/site name list relative to the last year of the BMS
#monitoring in an excel file. Code developed by Alessandro Mari in July 2021.w
#R version 4.0.0

#Requirements: 
# m_visit datatable with variables/columns: SITE_NAME, RECORDER_NAME, YEAR
#Check the "BMS_data_requirement.txt" file located in the main folder for an example of columns'
#value.


#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(data.table)
library(grid)
library(gridExtra)
library(ggplot2)

#checking if m_visit is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(m_visit) == FALSE){ b_count <- as.data.table(m_visit) }

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



#### recorder/site name list in the last year of the BMS monitoring ####

#ordering the years of the BMS monitoring
year_unq <- sort(unique(m_visit$YEAR))
#Here the year is chosen as the last year of monitoring as default
year <- tail(year_unq, 1)


#subsetting the visits datatable for the last year of monitoring
my_visit <- m_visit[YEAR == year, ]
#subsetting my_visit for only the recorders and sites name columns
my_rec <- unique(my_visit[, c(13, 6)])
#replacing "," symbol with ""
my_rec[, RECORDER_NAME := gsub(",", "", my_rec$RECORDER_NAME)]
#ordering my_rec according to the name of the recorders
my_rec <- my_rec[order(RECORDER_NAME)]

#changing the column names in how they will be seen in the figure
colnames(my_rec) <- c("Recorder name", "Transect name")

#table theme
tt_rec <- ttheme_default(
  core=list(bg_params=list(fill = "#F2DCDB", col = NA),
            fg_params=list(hjust=0, x=0.01)),
  colhead=list(fg_params=list(col="white", fontface="bold",hjust=0, x=0.01),
               bg_params=list(fill = "#aa0052", col = NA))
)

#converting my_rec to a table with tt_rec theme
rec_table <- tableGrob(my_rec, theme=tt_rec, rows=NULL)
#identifying the final dimensions (in cm) extracting this info from one of the three tables
r_fullheight <- convertHeight(sum(rec_table$heights), "cm", valueOnly = TRUE)
r_fullwidth <- convertWidth(sum(rec_table$widths),"cm", valueOnly=TRUE) + 0.5
#saving the table as a plot
ggsave(file = here("graphs/tr-rec-vis", paste("recorders_list_", year, ".png", sep = "")), rec_table,
       width = r_fullwidth,height=r_fullheight, units = "cm", dpi = 1000)
