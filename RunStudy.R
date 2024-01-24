# QC plot folders ----
qcfolder <- here::here("3_QC",db.name)
if (!file.exists(qcfolder)){
  dir.create(qcfolder, recursive = TRUE)}

# output files ---- 
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

# add functions ----
# risk table
RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

# measuring time in minutes using tictoc package
toc_min <- function(tic,toc,msg="") {
  mins <- round((((toc-tic)/60)),2)
  outmsg <- paste0(mins, " minutes elapsed")
}

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# Run incidence rate analysis ----
info(logger, 'RUNNING INCIDENCE RATE ANALYSIS')
source(here("2_Analysis","IncidenceAnalysis1.R"))
info(logger, 'INCIDENCE RATE ANALYSIS RAN')

# Run cohort characterisation analysis ----
info(logger, 'RUNNING COHORT CHARACTERISATION ANALYSIS')
source(here("2_Analysis","CohortCharacteristics3.R"))
info(logger, 'COHORT CHARACTERISATION ANALYSIS RAN')

# Run survival analysis -----
info(logger, 'RUNNING SURVIVAL ANALYSIS')
source(here("2_Analysis","SurvivalAnalysis1.R"))
info(logger, 'SURVIVAL ANALYSIS RAN')

readr::write_csv(snapshot(cdm), paste0(here::here(output.folder),"/", cdm_name(cdm), "_snapshot_cdm.csv"))

print("Done!")
print("-- If all has worked, there should now be zip folders with the incidence and survival results in the output folder to share")
print("-- Thank you for running the study! :)")
Sys.time()
Sys.time()-start
readLines(log_file)
