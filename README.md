# Incidence, Prevalence and Survival - OPTIMA
<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- **Study title**: The Worldwide Incidence of Lung Cancer
- **Study lead(s)**: Danielle Newby (danielle.newby@ndorms.ox.ac.uk)
- **ShinyApp PhenotypeR**:
- **ShinyApp Main Study**: https://dpa-pde-oxford.shinyapps.io/OPTIMA_lung_cancer_incidence_survival/
- **Publications**:

---

## Repository organisation

This repo is organized as follows:
- [1_Diagnostics](https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival/tree/main/1_Diagnostics): Please find the folders for the diagnostics of cancer phenotypes related to the study. [CohortDefinitions](https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival/tree/main/1_Diagnostics/CohortDefinitions) has the code related to developing the initial codelists using codelistgenerator. [PhenotypeR](https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival/tree/main/1_Diagnostics/PhenotypeR) contains the code to perform cohort diagnostics for phenotypes and [PhenotypeRShiny](https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival/tree/main/1_Diagnostics/PhenotypeRShiny) contains the shiny to review results from PhenotypeR.
- [2_Study](https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival/tree/main/2_Study): please find there the relevant code to obtain the study results.
- [3_Reporting](https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival/tree/main/3_Reporting): please find there the code to visualise the results with the shiny app.

---

## Running the main analysis
1) Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop). 
2) Open the project <i>OPTIMACancerIncidenceSurvival.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study <i>(source(here("RunStudy.R"))</i>.     
4) After running you should then have a zip folder with results to share in your output folder.

## Running the cohort diagnostics using PhenotypeR
1) Navigate to [PhenotypeR](https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival/tree/main/1_Diagnostics/PhenotypeR) Open the project <i>PhenotypeR.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session) in 1_Diagnostics/PhenotypeR
2) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study <i>(source(here("RunStudy.R"))</i>.     
3) After running you should then have a zip folder with results to share in your output folder.
