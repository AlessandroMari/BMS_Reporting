### Short instructions for running the analysis

#### Installation:

- Install the latest version of [Rstudio Desktop](https://www.rstudio.com/products/rstudio/download/)

#### To know:

- Use this folder ("BMS Data" folder) as the main folder for the work.
- Files containing data on butterflies occurence, samples and sites need to be located in
  the "data" folder.
- Do not modify "inst" folder. It contains important scripts, as the one for checking the data or the packages.
- The final graphs can be found in the "graphs" folder after running the script


#### For figures making:

- For accessing the main script, open the project "BMS.Rproj". In this way you will enter directly
  in the right environment ("BMS Data" folder). Then, open the main script "BMS_data_analysis.R"
  from the "files" section in the lower-right part of Rstudio window, located in the main folder
- Now you need to import the BMS data. 
- The columns' name and the content of the required datasets and their relative variables are listed in the file 
  "BMS_data_requirement.txt", which can be found in the main folder "BMS Data". Make sure your data have this format.
- If you import data manually downloading from the eBMS website, you can place the datasets in the "data" folder
- If you harmonized the three type of dataset as mentioned before, running the main script "BMS_data_analysis.R" should
  make all the figures in the "graphs" folder without issues. 

#### Additional notes:

- Example data is not available in the "data" folder for privacy reasons. Therefore, codes are not reproducible by a github user that access this project. However, if data is     needed, you can send an email to "ale_seas@libero.it" and I can deliver the request to the owner of the data. 

