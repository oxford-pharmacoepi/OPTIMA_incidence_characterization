# output files ---- 
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("2_Study", "1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# Run incidence rate analysis ----
info(logger, 'RUNNING INCIDENCE RATE ANALYSIS')
source(here("2_Study", "2_Analysis","IncidenceAnalysis1.R"))
info(logger, 'INCIDENCE RATE ANALYSIS RAN')

# Run cohort characterisation analysis ----
info(logger, 'RUNNING COHORT CHARACTERISATION ANALYSIS')
source(here("2_Study","2_Analysis","CohortCharacteristics3.R"))
info(logger, 'COHORT CHARACTERISATION ANALYSIS RAN')

# Run survival analysis -----
info(logger, 'RUNNING SURVIVAL ANALYSIS')
source(here("2_Study", "2_Analysis","SurvivalAnalysis1.R"))
info(logger, 'SURVIVAL ANALYSIS RAN')

# snapshot
readr::write_csv(snapshot(cdm), paste0(here::here(output.folder),"/", cdm_name(cdm), "_snapshot_cdm.csv"))

# cohort attrition for survival

# get study outputs

# save results

# zip results



print("Done!")
print("-- If all has worked, there should now be zip folders with the incidence and survival results in the output folder to share")
print("-- Thank you for running the study! :)")
Sys.time()
Sys.time()-start
readLines(log_file)
