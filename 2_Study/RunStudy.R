# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "output, we can use: here("output")
# but this file path could be set to somewhere else
output_folder <- here("Results", db_name)

# output files ---- 
if (!file.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)}

# read in functions -----
source(here("2_Analysis", "functions.R"))

# get cdm snapshot
cli::cli_alert_info("- Getting cdm snapshot")
write_csv(snapshot(cdm) %>% 
            mutate(start_date = study_start), here("Results", paste0(db_name,
  "/", cdmName(cdm), "_cdm_snapshot_.csv"
)))

# Cohort generation ----
# if you have already instantiated cohorts you can get them back
instantiatedCohorts <- FALSE

if (instantiatedCohorts == TRUE) {
  
  cdm <- CDMConnector::cdm_from_con(con = db, 
                                    cdm_schema = cdm_database_schema,
                                    write_schema = c("schema" = results_database_schema, 
                                                     "prefix" = table_stem),
                                    cdm_name = db_name, 
                                    cohort_tables = c(
                                      "outcome") )
 
} else {

cli::cli_alert_info("- Cohort generation")
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
cli::cli_alert_success("- Cohorts generated")

}

# incidence ----
if(isTRUE(run_incidence)){
  cli::cli_alert_info("- Running incidence")
  tryCatch({
    source(here("2_Analysis", "incidence.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here("Results", paste0(db_name,
                                      "/", cdmName(cdm),
                    
                    "_error_incidence.txt")))
  })
}


# characterisation analysis -----
if(isTRUE(run_characterisation)){
cli::cli_alert_info("- Running Characterisation")
  tryCatch({
    source(here("2_Analysis", "characterisation.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here("Results", paste0(db_name,
                                      "/", cdmName(cdm),
                    
                    "_error_characterisation.txt")))
  })
}

# zip results ----
cli::cli_alert_info("- Zipping Results")
# zip all results
zip::zip(
  zipfile = file.path(here("Results", db_name,
                           paste0("Results_", db_name, ".zip"))),
  files = list.files(here("Results", db_name)),
  root = output_folder)

cli::cli_alert_success("- Study Done!")
cli::cli_alert_success("- If all has worked, there should now be a zip folder with your results in the Results folder to share")
cli::cli_alert_success("- Thank you for running the study! :)")