# output files ---- 
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

# get cdm snapshot
cli::cli_alert_info("- Getting cdm snapshot")
write_csv(snapshot(cdm), here("Results", paste0(db.name,
  "/cdm_snapshot_", cdmName(cdm), ".csv"
)))

# Cohort generation ----
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
cli::cli_alert_info("- Running characterisation")
  tryCatch({
    source(here("2_Study", "2_Analysis", "characterisation.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here("Results", "error_characterisation.txt"))
  })

# zip results ----
# zip all results
zip(zipfile = file.path(here("Results",
                             paste0("Results_", db_name, ".zip"))),
    files = list.files(here("Results"), full.names = TRUE, recursive = TRUE))

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the Results folder to share")
print("-- Thank you for running the study! :)")
