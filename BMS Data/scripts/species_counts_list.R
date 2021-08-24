#This is code for making:
#-the image with the names and counts of the 10 most and 10 less common species in the last
#three years of the BMS monitoring 
#-the image with the names and counts of all the species counts in the last year of the BMS 
#monitoring 
#Code developed by Alessandro Mari in July 2021.
#R version 4.0.0

#Requirements:
# b_count datatable with variables/columns: SPECIES, COUNT, YEAR, 
#Check the "BMS_data_requirement.txt" file located in the main folder for an example of columns' value


#Loading packages (packages need to be installed first using the install.packages("") function)
library(here)
library(data.table)
library(grid)
library(gridExtra)
library(ggplot2)

#checking if b_count is of class datatable: If it is not datatable, the code converts it to it.
if(is.data.table(b_count) == FALSE){ b_count <- as.data.table(b_count) }

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


#### n most common species for the last three years of ITBMS monitoring ####

#species_count_sum: datatable with number of individuals for each species in each year
species_count_sum <- b_count[ , .(COUNT_SUM = sum(COUNT, na.rm=TRUE)), 
                                 by = .(YEAR, SPECIES)][order(YEAR, COUNT_SUM, 
                                                              decreasing = TRUE)]

#number of most common species of interest
n <- 10
#subsetting species_count_sum for the n most common species for each year
mostcommonsp <- species_count_sum[, head(.SD, n), by = YEAR]
#last 3 years of monitoring
last3yrs <- head(unique(mostcommonsp$YEAR),3)
#subsetting mostcommonsp for the last 3 years of monitoring
mostcommonsp2 <- mostcommonsp[YEAR %in% last3yrs]

#dataset with the n most common speci2s for the last year of monitoring
most_data1 <- mostcommonsp2[YEAR == last3yrs[1]]
#adding an empty column to get space in final image
most_data1$GAP <- "          "
#dataset with the n most common speci2s for the 2nd last year of monitoring
most_data2 <- mostcommonsp2[YEAR == last3yrs[2]]
#adding an empty column to get space in final image
most_data2$GAP <- "          "
#dataset with the n most common speci2s for the 3rd last year of monitoring
most_data3 <- mostcommonsp2[YEAR == last3yrs[3]]

#ttheme for the species names
tt_core1 <- ttheme_default(
  core=list(bg_params=list(fill = "white", col = NA),
            fg_params=list(hjust=0, x=0.01, fontface = "italic"))
)
#ttheme for the species abundances
tt_core2 <- ttheme_default(
  core=list(bg_params=list(fill = "white", col = NA),
            fg_params=list(hjust=0, x=0.01))
)
#ttheme for the header (year value)
tt_header <- ttheme_default(
  colhead=list(fg_params=list(col="white", fontface="bold"),
               bg_params=list(fill = "#aa0052", col = NA))
)

#table for last year
#table/column with the 10 most common species names
most_core1_1 <- tableGrob(most_data1[,-1][,1], rows=NULL, cols = NULL, theme=tt_core1)
#table/column with the 10 most common species abundances
most_core1_2 <- tableGrob(most_data1[,-1][,2:3], rows=NULL, cols = NULL, theme=tt_core2)
#merging 10 most common species names and species abundances tables
most_core1 <- gtable_combine(most_core1_1, most_core1_2, along=1)
#adding names to the columns for an identification of them
most_core1$colnames <- c("c1", "c2", "c3")
#table/row with the header (year value)
header1 <- tableGrob(most_data1[,-1][1,1], rows=NULL, cols=last3yrs[1], theme=tt_header) 
#adding the header row to the species name+abundance table
most_combine1 <- gtable_combine(header1[1,], most_core1, along = 2)
#change the relevant rows of gtable (or merging the two header cells in one)
most_combine1$layout[1:2 , c("l", "r")] <- list(c(1, 1), c(2, 2))

#table for 2nd last year
#table/column with the 10 most common species names
most_core2_1 <- tableGrob(most_data2[,-1][,1], rows=NULL, cols = NULL, theme=tt_core1)
#table/column with the 10 most common species abundances
most_core2_2 <- tableGrob(most_data2[,-1][,2:3], rows=NULL, cols = NULL, theme=tt_core2)
#merging 10 most common species names and species abundances tables
most_core2 <- gtable_combine(most_core2_1, most_core2_2, along=1)
#adding names to the columns for an identification of them
most_core2$colnames <- c("c1", "c2", "c3")
#table/row with the header (year value)
header2 <- tableGrob(most_data2[,-1][1,1], rows=NULL, cols=last3yrs[2], theme=tt_header) 
#adding the header row to the species name+abundance table
most_combine2 <- gtable_combine(header2[1,], most_core2, along = 2)
#change the relevant rows of gtable (or merging the two header cells in one)
most_combine2$layout[1:2 , c("l", "r")] <- list(c(1, 1), c(2, 2))


#table for 3rd last year
#table/column with the 10 most common species names
most_core3_1 <- tableGrob(most_data3[,-1][,1], rows=NULL, cols = NULL, theme=tt_core1)
#table/column with the 10 most common species abundances
most_core3_2 <- tableGrob(most_data3[,-1][,2], rows=NULL, cols = NULL, theme=tt_core2)
#merging 10 most common species names and species abundances tables
most_core3 <- gtable_combine(most_core3_1, most_core3_2, along=1)
#adding names to the columns for an identification of them
most_core3$colnames <- c("c1", "c2")
#table/row with the header (year value)
header3 <- tableGrob(most_data3[,-1][1,1], rows=NULL, cols=last3yrs[3], theme=tt_header) 
#adding the header row to the species name+abundance table
most_combine3 <- gtable_combine(header3[1,], most_core3, along = 2)
#change the relevant rows of gtable (or merging the two header cells in one)
most_combine3$layout[1:2 , c("l", "r")] <- list(c(1, 1), c(2, 2))

#merging the three tables
most_grid <- cbind(most_combine1, most_combine2, most_combine3)

#identifying the final dimensions (in cm) extracting this info from one of the three tables
m_fullheight <- convertHeight(sum(most_combine1$heights), "cm", valueOnly = TRUE)
m_fullwidth <- convertWidth(sum(most_combine1$widths),"cm", valueOnly=TRUE) * 3 - 1
#saving the table as a plot
ggsave(file = here("graphs/species", paste("10mostcommon.png", sep = "")), most_grid,
       width = m_fullwidth,height=m_fullheight, units = "cm", dpi = 1000)





#### n less common species for the last three years of ITBMS monitoring ####

#species_count_sum: datatable with number of individuals for each species in each year
species_count_sum <- b_count[ , .(COUNT_SUM = sum(COUNT, na.rm=TRUE)), 
                                 by = .(YEAR, SPECIES)][order(YEAR, COUNT_SUM, 
                                                              decreasing = TRUE)]

#number of less common species of interest
n <- 10
#subsetting species_count_sum for the n lesscommon species for each year
lesscommonsp <- species_count_sum[, tail(.SD, n), by = YEAR]
#last 3 years of monitoring
last3yrs <- head(unique(lesscommonsp$YEAR),3)
#subsetting lesscommonsp for the last 3 years of monitoring
lesscommonsp2 <- lesscommonsp[YEAR %in% last3yrs]

#dataset with the n less common speci2s for the last year of monitoring
less_data1 <- lesscommonsp2[YEAR == last3yrs[1]]
#adding an empty column to get space in final image
less_data1$GAP <- "          "
#dataset with the n less common speci2s for the 2nd last year of monitoring
less_data2 <- lesscommonsp2[YEAR == last3yrs[2]]
#adding an empty column to get space in final image
less_data2$GAP <- "          "
#dataset with the n less common speci2s for the 3rd last year of monitoring
less_data3 <- lesscommonsp2[YEAR == last3yrs[3]]

#ttheme for the species names
tt_core1 <- ttheme_default(
  core=list(bg_params=list(fill = "white", col = NA),
            fg_params=list(hjust=0, x=0.01, fontface = "italic"))
)
#ttheme for the species abundances
tt_core2 <- ttheme_default(
  core=list(bg_params=list(fill = "white", col = NA),
            fg_params=list(hjust=0, x=0.01))
)
#ttheme for the header (year value)
tt_header <- ttheme_default(
  colhead=list(fg_params=list(col="white", fontface="bold"),
               bg_params=list(fill = "#aa0052", col = NA))
)

#table for last year
#table/column with the 10 less common species names
less_core1_1 <- tableGrob(less_data1[,-1][,1], rows=NULL, cols = NULL, theme=tt_core1)
#table/column with the 10 less common species abundances
less_core2_1 <- tableGrob(less_data1[,-1][,2:3], rows=NULL, cols = NULL, theme=tt_core2)
#merging 10 less common species names and species abundances tables
less_core1 <- gtable_combine(less_core1_1, less_core2_1, along=1)
#adding names to the columns for an identification of them
less_core1$colnames <- c("c1", "c2", "c3")
#table/row with the header (year value)
header1 <- tableGrob(less_data1[,-1][1,1], rows=NULL, cols=last3yrs[1], theme=tt_header) 
#adding the header row to the species name+abundance table
less_combine1 <- gtable_combine(header1[1,], less_core1, along = 2)
#change the relevant rows of gtable (or merging the two header cells in one)
less_combine1$layout[1:2 , c("l", "r")] <- list(c(1, 1), c(2, 2))

#table for 2nd last year
#table/column with the 10 less common species names
less_core1_2 <- tableGrob(less_data2[,-1][,1], rows=NULL, cols = NULL, theme=tt_core1)
#table/column with the 10 less common species abundances
less_core2_2 <- tableGrob(less_data2[,-1][,2:3], rows=NULL, cols = NULL, theme=tt_core2)
#merging 10 less common species names and species abundances tables
less_core2 <- gtable_combine(less_core1_2, less_core2_2, along=1)
#adding names to the columns for an identification of them
less_core2$colnames <- c("c1", "c2", "c3")
#table/row with the header (year value)
header2 <- tableGrob(less_data2[,-1][1,1], rows=NULL, cols=last3yrs[2], theme=tt_header) 
#adding the header row to the species name+abundance table
less_combine2 <- gtable_combine(header2[1,], less_core2, along = 2)
#change the relevant rows of gtable (or merging the two header cells in one)
less_combine2$layout[1:2 , c("l", "r")] <- list(c(1, 1), c(2, 2))


#table for 3rd last year
#table/column with the 10 less common species names
less_core1_3 <- tableGrob(less_data3[,-1][,1], rows=NULL, cols = NULL, theme=tt_core1)
#table/column with the 10 less common species abundances
less_core2_3 <- tableGrob(less_data3[,-1][,2], rows=NULL, cols = NULL, theme=tt_core2)
#merging 10 less common species names and species abundances tables
less_core3 <- gtable_combine(less_core1_3, less_core2_3, along=1)
#adding names to the columns for an identification of them
less_core3$colnames <- c("c1", "c2")
#table/row with the header (year value)
header3 <- tableGrob(less_data3[,-1][1,1], rows=NULL, cols=last3yrs[3], theme=tt_header) 
#adding the header row to the species name+abundance table
less_combine3 <- gtable_combine(header3[1,], less_core3, along = 2)
#change the relevant rows of gtable (or merging the two header cells in one)
less_combine3$layout[1:2 , c("l", "r")] <- list(c(1, 1), c(2, 2))

#merging the three tables
less_grid <- cbind(less_combine1, less_combine2, less_combine3)
#identifying the final dimensions (in cm) extracting this info from one of the three tables
l_fullheight <- convertHeight(sum(less_combine1$heights), "cm", valueOnly = TRUE) 
l_fullwidth <- convertWidth(sum(less_combine1$widths),"cm", valueOnly=TRUE) * 3 
#saving the table as a plot
ggsave(file = here("graphs/species", paste("10lesscommon.png", sep = "")), less_grid,
       width = l_fullwidth,height=l_fullheight, units = "cm", dpi = 1000)



#### counts for all the species in the last ITBMS year #### 

year_unq <- sort(unique(b_count$YEAR))
#Here the year is set as the last year of recording as default
year <- tail(year_unq,1)

sp_count <- b_count[YEAR==year][ , .(COUNT_SUM = sum(COUNT, na.rm=TRUE)), 
                                    by = .(YEAR, SPECIES)][order(COUNT_SUM, 
                                                                 decreasing = TRUE)][,-1]

colnames(sp_count) <- c("Species name", "Abundance")

leng <- dim(sp_count)[1]/3
index <- ceiling(leng)
data1 <- sp_count[1:index,]
index2a <- index + 1
index2b <- index2a + index - 1
data2 <- sp_count[index2a:index2b,]
index3a <- index2b + 1
index3b <- index3a + index - 1
data3 <- sp_count[index3a:index3b,]
nas <- which(!complete.cases(data3)) 
data3$`Species name`[nas] <- " "
data3$Abundance[nas] <- " "

#ttheme for the species names
tt_core1 <- ttheme_default(
  core=list(bg_params=list(fill = "#F2DCDB", col = NA),
            fg_params=list(hjust=0, x=0.01, fontface = "italic")),
  colhead=list(fg_params=list(hjust=0, x=0.01, col="white", fontface="bold"),
               bg_params=list(fill = "#aa0052", col = NA))
)
#ttheme for the species abundances
tt_core2 <- ttheme_default(
  core=list(bg_params=list(fill = "#F2DCDB", col = NA),
            fg_params=list(hjust=0, x=0.01)),
  colhead=list(fg_params=list(hjust=0, x=0.01,col="white", fontface="bold"),
               bg_params=list(fill = "#aa0052", col = NA))
)


#table for last year
#table/column with the 10 sp common species names
sp_core1_1 <- tableGrob(data1[,1], rows=NULL, theme=tt_core1)
#table/column with the 10 sp common species abundances
sp_core1_2 <- tableGrob(data1[,2], rows=NULL, theme=tt_core2)
#merging 10 sp common species names and species abundances tables
sp_core1 <- gtable_combine(sp_core1_1, sp_core1_2, along=1)


#table for the 2nd last year
#table/column with the 10 sp common species names
sp_core2_1 <- tableGrob(data2[,1], rows=NULL, theme=tt_core1)
#table/column with the 10 sp common species abundances
sp_core2_2 <- tableGrob(data2[,2], rows=NULL, theme=tt_core2)
#merging 10 sp common species names and species abundances tables
sp_core2 <- gtable_combine(sp_core2_1, sp_core2_2, along=1)

#table for the 3rd last year
#table/column with the 10 sp common species names
sp_core3_1 <- tableGrob(data3[,1], rows=NULL, theme=tt_core1)
#table/column with the 10 sp common species abundances
sp_core3_2 <- tableGrob(data3[,2], rows=NULL, theme=tt_core2)
#merging 10 sp common species names and species abundances tables
sp_core3 <- gtable_combine(sp_core3_1, sp_core3_2, along=1)

#merging the three tables
sp_grid <- cbind(sp_core1, sp_core2, sp_core3)

#identifying the final dimensions (in cm) extracting this info from one of the three tables
sp_fullheight <- convertHeight(sum(sp_core1$heights), "cm", valueOnly = TRUE) - 1
sp_fullwidth <- convertWidth(sum(sp_core1$widths),"cm", valueOnly=TRUE) *3 
#saving the table as a plot
ggsave(file = here("graphs/species", paste("spcounts_", year, ".png", sep = "")), sp_grid,
       width = sp_fullwidth,height=sp_fullheight, units = "cm", dpi = 1000)


