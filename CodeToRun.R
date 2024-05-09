# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
# install.packages("renv")
# renv::activate()
renv::restore()

# load r packages
library(CirceR)
library(CDMConnector)
library(PatientProfiles)
library(CohortSurvival)
library(IncidencePrevalence)
library(cli)
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(log4r)
library(tidyr)
library(stringr)
library(ggplot2)
library(broom)
library(SqlRender)
library(tictoc)
library(RPostgres)
library(CodelistGenerator)
library(DrugUtilisation)
library(dsr)
library(CohortCharacteristics)

# database metadata and connection details -----
# The name/ acronym for the database
db_name<-"..."

# Specify databaseConnector connection details -----
# database connection details
# connect to database
user <-"..."
password <- "..."
port <- "..."
host <-"..."
server_dbi<-"..."


# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect("...",
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "..."


# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- "..."

# The name of the schema where results tables will be created 
results_database_schema <- "..."

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 
table_stem <- "..."

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = c("schema" = results_database_schema,
                                                   "prefix" = table_stem),
                                  cdm_name = db_name)

# # to check whether the DBI connection is correct, 
# # running the next line should give you a count of your person table
cdm$person %>%
  tally()

# Study start date -----
# please put the study start date if you do not have usable data before "2003-01-01" 
# for example if you have usable data from 2010 you would put 2011-01-01 (to allow for 1 year prior history)
study_start <- "..."

# Run the study ------
# For some data partners they may not have be able to run certain studies i.e. a cancer registry
# will not have a background denominator population therefore run_incidence will be set to FALSE
run_incidence <- TRUE
run_prevalence <- FALSE
run_survival <- FALSE
run_characterisation <- TRUE

source(here("RunStudy.R"))

# disconnect from the database (only do this after you have run all analysis)
#dbDisconnect(db)