
#install.packages("renv") # if not already installed, install renv from CRAN
renv::activate()
renv::restore() # this should prompt you to install the various packages required for the study


# packages -----
# load the below packages 
# you should have them all available, with the required version, after
# having run renv::restore above
library(DatabaseConnector)
library(CohortDiagnostics)
library(CirceR)
library(CohortGenerator)
library(here)
library(stringr)
library(tibble)
library(dplyr)

# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"..."

connectionDetails <- createConnectionDetails(....)

# sql dialect used with the OHDSI SqlRender package
targetDialect <-"postgresql" 
# schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"...."
# schema that contains the vocabulariea
vocabulary_database_schema<-"...."
# schema where a results table will be created 
results_database_schema<-"...."

# stem for tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten
cohortTableStem<-"...."

# Run analysis ----
source(here("RunAnalysis.R"))

# Review results -----
CohortDiagnostics::preMergeDiagnosticsFiles(dataFolder = here::here("Results"))
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = here("Results"))


