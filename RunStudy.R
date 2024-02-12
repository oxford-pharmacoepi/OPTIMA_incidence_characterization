# output files ---- 
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

# get cdm snapshot
cli::cli_alert_info("- Getting cdm snapshot")
write_csv(snapshot(cdm), here("Results", paste0(db.name,
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
                                    cdm_name = db.name, 
                                    cohort_tables = c(
                                      "outcome") )
 
}

cli::cli_alert_info("- Cohort generation")
source(here("2_Study", "1_InstantiateCohorts","InstantiateStudyCohorts.R"))

# incidence ----
if(isTRUE(run_incidence)){
  cli::cli_alert_info("- Running incidence")
  tryCatch({
    source(here("2_Study", "2_Analysis", "incidence.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here("Results", "error_incidence.txt"))
  })
}

# survival analysis ----
if(isTRUE(run_survival)){
  cli::cli_alert_info("- Running survival analysis")
  tryCatch({
    source(here("2_Study", "2_Analysis", "survival.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here("Results", "error_survival.txt"))
  })
}

# characterisation analysis -----
if(isTRUE(run_characterisation)){
cli::cli_alert_info("- Running characterisation")
  tryCatch({
    source(here("2_Study", "2_Analysis", "characterisation.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here("Results", "error_characterisation.txt"))
  })
}

# zip results ----
# zip all results
zip(zipfile = file.path(here("Results",
                             paste0("Results_", db_name, ".zip"))),
    files = list.files(here("Results"), full.names = TRUE, recursive = TRUE))

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the Results folder to share")
print("-- Thank you for running the study! :)")
