# Overview

Repository of the project "Public support for welfare and redistributive policies in Ireland" by the [Economic and Social Research Institute (ESRI)](https://www.esri.ie/).

Authors:
- Daniel Capistrano
- Bertrand Maitre
- Helen Russell

# Data Availability

This project is based on secondary data only. All data files are publicly available from the projects' websites or, for one case, under request to the data custodian. The next section 

## Data sources

- European Social Survey (ESS): Files are available from the ESS website: [https://www.europeansocialsurvey.org/](https://www.europeansocialsurvey.org/). The analyses include data from the first 11 rounds and from countries that participated in more than one round.

- Voter Survey (VS) from the European Elections Study:  Data from the surveys of 2014,	2019, and 2024 conducted immediately after the European elections. Files can be downloaded from the GESIS [website](https://www.gesis.org/en/services/finding-and-accessing-data/international-survey-programs/european-election-studies).


- An Coimisiún Toghcháin 2024 General Election survey as part of the National Election and Democracy Study (NEDS). Dataset was obtained after request to research@electoralcommission.ie in 2025.

In addition, for one section of chapter 2, we make use of data from the Eurobarometer flash survey on citizen's attitudes towards taxation, conducted in 2025. The tables with response distributions by county were obtained from [data.europe.eu](https://data.europa.eu/data/datasets/s3375_fl562_eng?locale=en). However, the analysis is based on population estimates as no microdata was publicly available by the time of the publication of the report. 

We also utilised figures on seasonally adjusted monthly unemployment rate calculated by the [Central Statistics Office (CS0)](https://www.cso.ie/en/methods/labourmarket/monthlyunemployment/)


## Dataset list

| Data file | Source | Availability |
|-----------|--------|----------|
| data/raw/ess/ess_eu.dta | ESS | ESS Data Portal |
| data/raw/vs/ZA5160_v4-1-0.dta | VS | GESIS website |
| data/raw/vs/ZA7581_v2-0-1.dta | VS | GESIS website |
| data/raw/vs/ZA8868_v1-0-0.dta | VS | GESIS website |
| data/raw/neds/NEDS_General_Elections_2024_F2F_Post.dta | NEDS | request to An Coimisiún Toghcháin|
| data/raw/MUM01.20250522T130504.csv | CSO | CSO Website |


# Instructions to Replicators

The data analysis was conducted using R version 4.5.2 (2025-10-31). 
Packages used are specified in each code file. 

To reproduce the analysis published in the main report, it is necessary to create a sub-directory "data/raw" and include all data files described in the previous section. Once these data files are located in this sub-directory, the analysis can be reproduced with the following steps:

- Process original raw data running the code contained in [`paper/0_process.R`](paper/0_process.R). This code will generate the working datasets and add them to the directory `data/processed`. These files can be obtained under request to authors. 

- Import working dataset, perform variable transformations and create custom functions with [`paper/1_SetupImport.R`](paper/1_SetupImport.R)

- Generate figures and tables contained in chapter 2 with [`paper/2_C2_WhoSupports.R`](paper/2_C2_WhoSupports.R). The files generated can be found in `output/c2`.

- Create figures and tables contained in chapter 3 with [`paper/3_C3_Welfare.R`](paper/3_C3_Welfare.R). The files generated can be found in `output/c3`.

- Create figures and tables contained in chapter 4 with [`paper/4_C4_Change.R`](paper/4_C4_Change.R). The files generated can be found in `output/c4`.

- Create tables included in the appendix with [`paper/5_Appendix.R`](paper/5_Appendix.R). The files generated can be found in `output/appendix`.


