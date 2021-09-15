# BMS Reporting

### Context and aim
With increasing monitoring of biodiversity status, producing a report becomes fundamental for communicating scientific results at different scales and for various purposes. Within butterfly conservation, [Butterfly Monitoring Schemes](https://butterfly-monitoring.net/) (BMSs) operate in an increasing number of countries with broadly the same methodology, and data from monitoring are gathered in a central database, the European Butterfly Monitoring Scheme (eBMS). Consequently, they have the same format and can be analyzed with the same procedure once accurately downloaded.  Yet, not every scheme may have time or the tools to conduct an extensive data analysis and use the outputs to compile an annual report. 

Here we implemented a two-step method with the final aim of producing an annual report for a BMS. For the time being, we also organized the work in two folders which are separately associated with two R projects. So, entering the respective R project before opening any script is fundamental to set the right R main directory and avoiding path issues. Each of the two folders includes a “README.txt” text file which briefly explains the folder’s content and usage. 

### Brief description of the two-step method

#### First step
In the first step (folder “BMS Data”; R project “BMS_data.Rproj”), the BMS data collected in a monitoring period is analyzed to produce all the figures required for the report making. We suggest having monitoring data from the first year of the scheme activity to the year of interest for the report, but shorter periods are allowed. At this level of the project development, the main R script (“BMS_data_analysis.R”) still needs the data to be imported first. After downloading data from the eBMS website, they usually consist of three datasets: one with butterfly species occurrences, one with visit information, and one with site information. The names of the datasets and of the related columns used in the analysis have to be changed as specified in the text file “BMS_data_requirement.txt” located in the main folder. We apply this change to make the code clearer and easier to handle when exploring the data. If the datasets are correctly formatted, running the script will produce all the figures and save them in the “graphs” folder. 

#### Second step
In the second step (folder “BMS Report”; R project “BMS_report.Rproj”), we use the figures produced in the first step to create the annual report for the last year of the monitoring period selected in the downloading phase. Since the two main folders have not been linked yet, after the previous step one needs to copy the “graphs” folder from the first (“BMS Data”) to the second (“BMS Report) or at least to replace content figure by figure when uploading it. For producing the report, we need to access the Rmarkdown document “Report.Rmd” located in the main folder. The document includes both standard text and code chunks for adding figures or tables. Moreover, it comes with a background image  for the front page that can be easily replaced with a new one inside the “background” folder. Additional elements include logos in the front page or LaTeX-based template for setting the document layout with a greater detail. Knitting the document will produce the report in pdf format. 

### Additional notes
- The idea of this project is to help BMS coordinators in producing their own annual report showing the results of the last year of monitoring and a comparison with the previous years. Many improvements are required. Examples may be introducing an API request inside the script to retrieve BMS data  from the online database or the automatic positioning of the figures in the report with respect to the text. Indeed, at the moment figures position needs to be set manually in the Rmarkdown document. 
