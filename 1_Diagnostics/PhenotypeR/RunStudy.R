# settings ------
input <- list(
  runGenerateCohort = T,              #### Generate cohort or use preloaded cohorts
  runCalculateOverlap = T,            #### Calculate Overlap
  runCountCodes = T,                  #### run orphan codes and count codes
  runIndexEvents = T,                 #### run index events
  runProfiling = T,                   #### run age and time in database characterisation
  runMatchedSampleLSC = T,            #### run matched LSC
  runIncidence = T,                   #### run Incidence
  runPrevalence = T,                  #### run Prevalence
  sampleIncidencePrevalence = 500000, #### Sample for Incidence Prevalence (NULL if all cdm)
  cdmName = db_name,
  exportResultsRData=T
)

# make sure you have the results folder
#Create folder for the results
if (!file.exists(here::here("Results", db_name))){
  dir.create(here::here("Results" , db_name), recursive = TRUE)}


# Log start ------

tic.clearlog()
tic.clear()
tic(msg = "phenotypeR total time run")

# Options and set-up: directories and settings ------
tic(msg = "Settings and loading of Phoebe")

cohort_json_dir <- here("Cohorts")
cohorts_name <- "cancer_"

# To export output 
result_names <- c("cohort_definitions", "cohort_count", "code_counts", "cohort_overlap", 
                  "age_distribution", "time_distribution", "prevalence", "incidence", 
                  "index_events", "lsc_sample", "lsc_matched", "lsc_difference", "log", "cdm_snapshot")
output <- data <- vector("list", length(result_names)) |> setNames(result_names)



if (input$runCountCodes & !file.exists(here("Phoebe/concept_recommended.csv")) ){
  if (file.exists(here("Phoebe/concept_recommended_20221006 1.zip"))){
    unzip(zipfile=here("Phoebe/concept_recommended_20221006 1.zip"), exdir=here("Phoebe/") )
  } else { 
    input$runCountCodes <- F 
  }
}


if (input$runCountCodes){ 
  concept_recommended <- read.csv(here("Phoebe/concept_recommended.csv"))
}

toc(log = TRUE)


# Connect to database using CDMConnector ########
tic(msg = "Connect to database")

if (input$runGenerateCohort) {
cdm <- cdm_from_con(con = db,
                         cdm_schema = c(schema = cdm_schema),
                         write_schema = c(schema= write_schema, prefix = study_prefix),
                         achilles_schema = achilles_schema, 
                         cdm_name = db_name
                    )
} else   {
  cdm <- cdm_from_con(con = db,
                      cdm_schema = c(schema = cdm_schema),
                      write_schema = c(schema= write_schema, prefix = study_prefix),
                      achilles_schema = achilles_schema,
                      cohort_tables = cohorts_name, # to load cohorts already there
                      cdm_name = db_name  
  )
 
}

toc(log = TRUE)


# Get cdm snapshot -----
tic(msg = "Getting cdm snapshot")
output$cdm_snapshot <- snapshot(cdm)
write_csv(output$cdm_snapshot, here("Results", db_name, paste0(
  "cdm_snapshot_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
)))


  
toc(log = TRUE)

# Step 1: Get cohorts and generate them ------
# now from json, but we can do with CapR other sources 

tic(msg = "Generate Cohort Set")
cohort_set <- read_cohort_set(cohort_json_dir)
if (input$runGenerateCohort) {
  cdm <-   generateCohortSet(cdm, 
                             cohort_set,
                             name = cohorts_name,
                             computeAttrition = TRUE,
                             overwrite = TRUE)
}

toc(log = TRUE)

# Step 1.2:  Cohort Counts #########

tic(msg = "Cohort counts, attrition")

output$cohort_count <- cohort_count(cdm[[cohorts_name]]) %>% 
  left_join(settings(cdm[[cohorts_name]])) %>% 
  mutate(cdm_name = input$cdmName)
write_csv(output$cohort_count, here("Results", db_name, paste0(
  "cohort_count_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
)))

toc(log = TRUE)


# Step 2: Cohort Overlap (Subjects) ###############
# Percentages and counts: Counts only for now

tic(msg = "Calculate Overlap")
if (input$runCalculateOverlap) {
# Summarize the number of IDs in each group
summary_by_group <- cdm[[cohorts_name]] %>%
                    group_by(cohort_definition_id) %>%
                    summarize(ids_in_group = n()) %>% 
                    collect()

# Join to get the IDs that intersect between groups
summary_intersections <- cdm[[cohorts_name]] %>%
  inner_join(cdm[[cohorts_name]], by = "subject_id") %>%
  filter(cohort_definition_id.x != cohort_definition_id.y) %>%
  select(subject_id, cohort_definition_id_x = cohort_definition_id.x, cohort_definition_id_y = cohort_definition_id.y) %>%
  distinct()  %>%
  group_by(cohort_definition_id_x, cohort_definition_id_y) %>%
  summarize(intersect_count = n()) %>% 
  collect() 


output$cohort_overlap <- summary_intersections %>% 
  mutate(cdm_name = input$cdmName) 
output$cohort_overlap <- tryCatch({ 
  output$cohort_overlap%>% 
  mutate(intersect_count = if_else(intersect_count > 0 & intersect_count < 5, NA, intersect_count))
}, error = function(e) {})

tryCatch({  write_csv(output$cohort_overlap, here("Results",db_name, paste0(
  "cohort_overlap_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
))) }, error = function(e) {})
}
toc(log = TRUE)

# Step 3: Counts : Concepts in Data Source, Orphan concepts, Cohort definition, Index Event Breakdown #########

#### Test orphans with codelistgen
tic(msg = "Orphan codes + markdown readable text for only first cohort")

cohort_set_res = cohort_set
cohort_set_res$markdown <- ""

code_counts <- tibble()
index_events <- tibble()

for (n in  row_number(cohort_set_res) ) {
  
  cohort <- cohort_set_res$cohort_name[n]  
  json <- paste0(cohort_set_res$json[n]  )
  cohortExpresion <- CirceR::cohortExpressionFromJson(json)
  markdown <- CirceR::cohortPrintFriendly(cohortExpresion)
  cohort_set_res$markdown[n] <-  markdown
  
  ### Ideally reads the same JSON character line
  json2 <- jsonlite::read_json(paste0(cohort_json_dir, "/", cohort, ".json"))
  codes <- codesFromCohort(paste0(cohort_json_dir, "/", cohort, ".json"), cdm, withConceptDetails = F)

  
  codes_id <- unlist(codes, recursive = TRUE, use.names = F)

    if (input$runCountCodes) {
      recommended_codes <- concept_recommended %>% 
        filter(concept_id_1 %in% codes_id ) %>% 
        filter(!concept_id_2 %in% codes_id) %>% 
        distinct(concept_id_2, .keep_all = TRUE)
    }
    
    try({
      if (input$runCountCodes) {
        recommended_codes_counts <- achillesCodeUse(list("recomendation" = recommended_codes$concept_id_2),
                                                    cdm,
                                                    countBy = c("record", "person"),
                                                    minCellCount = 5) %>%  
          mutate(standard_concept_id= as.integer(group_level )) %>%
          left_join( recommended_codes, 
                     join_by(standard_concept_id == concept_id_2 ) ) %>%
          mutate(type="reccomended_codes", cohort=cohort )
        
        
        original_codes_counts <- achillesCodeUse(list("original_codes" = codes_id),
                                                 cdm,
                                                 countBy = c("record", "person"),
                                                 minCellCount = 5) %>%  
          mutate(standard_concept_id= as.integer(group_level )) %>% 
          left_join( recommended_codes, 
                     join_by(standard_concept_id == concept_id_2 ) ) %>%
          mutate(type="original_codes", cohort=cohort, relationship_id="original_codes", 
                 concept_id_1=standard_concept_id  )
        
        code_counts <- rbind(code_counts, recommended_codes_counts, original_codes_counts )
      }
    })
  
  
  ####### Cohort index
  
  tic(msg = "Index Event Breakdown")
  try({
    if (input$runIndexEvents) {
      Index_events <- summariseCohortCodeUse( x= codes,
                                              cdm, 
                                              cohortTable=cohorts_name,
                                              timing = "entry",
                                              countBy =  c("record", "person"),
                                              byConcept = TRUE,
                                              cohortId = n)
      index_events <- rbind(index_events, Index_events)
    }
  })
  toc(log = TRUE)
  
} 

# save Results
output$code_counts  <- code_counts %>% mutate(cdm_name = input$cdmName)
write_csv(output$code_counts, here("Results",db_name, paste0(
  "code_counts_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
)))
output$index_events <- index_events %>% mutate(cdm_name = input$cdmName)
write_csv(output$index_events, here("Results",db_name, paste0(
  "index_events_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
)))
output$cohort_definitions <- cohort_set_res %>% mutate(cdm_name = input$cdmName)
write_csv(output$cohort_definitions, here("Results", db_name, paste0(
  "cohort_definitions_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
)))

rm(concept_recommended)
toc(log = TRUE)
   
# Step 4: Time Distributions #########
# observation time (days) after index , observation time (days) prior to index, time (days) between cohort start and end
# Need to add better characterisation of demographics (a sort of table 1)

tic(msg = "Patient_profiles summary")
#cdm$Results_dx <- cdm[[cohorts_name]]
if (input$runProfiling) {
  Patient_profiles <- cdm[[cohorts_name]] %>%
    addDemographics() %>% 
    collect()   %>%
    mutate( age_group= cut(age, c(seq(0, 110, 5 ), Inf), include.lowest=TRUE))
  
  
  
  Age_distribution <- Patient_profiles %>% group_by(cohort_definition_id, age_group, sex) %>% tally() 
  
  
  Time_distribution <- Patient_profiles %>%
    group_by(cohort_definition_id, sex) %>% 
    summarise_at(vars(age, prior_observation, future_observation), list(Min = min, Mean = mean, Median = median,  Max = max, Sd = sd)) %>%
    collect()
  
  rm(Patient_profiles)
  
  output$age_distribution <- Age_distribution %>% mutate(cdm_name = input$cdmName) |> mutate(n = if_else(n > 0 & n < 5, NA, n))
  write_csv(output$age_distribution, here("Results",db_name, paste0(
    "age_distribution_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
  )))
  output$time_distribution <- Time_distribution %>% mutate(cdm_name = input$cdmName)
  write_csv(output$time_distribution, here("Results",db_name, paste0(
    "time_distribution_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
  )))
}



toc(log = TRUE)


# Step 5: 10-13 - Cohort Characterisation : Large scale + temporal + differneces  & matching   ################
 
 tic(msg = "Generate 1K Sample and Matched sample")
if (input$runMatchedSampleLSC) {

 cdm$sample <- cdm[[cohorts_name]]  %>% 
   slice_sample( n=1000, by =cohort_definition_id ) %>% compute()
 
 cdm$sample2 <- cdm$sample %>% 
   left_join(cdm$person %>% select(person_id, year_of_birth, gender_concept_id ),
             by=join_by(subject_id==person_id))  %>% compute()
 
 
 
 cdm$person_obs <- cdm$person %>% slice_sample( n=1000, by =year_of_birth )  %>% left_join(cdm$observation_period ) %>% compute()
 
 cdm$matched_cohort <- cdm$sample2 %>% left_join(cdm$person_obs , by=join_by(year_of_birth==year_of_birth, 
                                                                         gender_concept_id==gender_concept_id),
                                             relationship = "many-to-many", 
                                             keep=T)  %>% 
   filter(cohort_start_date>=observation_period_start_date, cohort_start_date<=observation_period_end_date) %>%
   distinct(subject_id, cohort_definition_id, .keep_all = TRUE) %>% 
   select(person_id,cohort_start_date,cohort_end_date, cohort_definition_id ) %>%
   rename( subject_id =person_id    ) %>% compute()
}
 

toc(log = TRUE)

tic("LargeScaleChar matched")
if (input$runMatchedSampleLSC) {
  large_scale_char_matched <- summariseLargeScaleCharacteristics(
    cohort=cdm$matched_cohort,
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                  c(0, 0), 
                  c(1, 30), c(31, 365),  c(366, Inf)),
    eventInWindow = c("condition_occurrence", "visit_occurrence",
                      "measurement", "procedure_occurrence",  "observation"), 
    episodeInWindow = c("drug_era"),
    minimumFrequency = 0.0005
  )
  
  large_scale_char_matched <- large_scale_char_matched %>% 
    filter(variable_name != "settings") %>% 
    omopgenerics::suppress(minCellCount = 5)
  
  output$lsc_matched <- large_scale_char_matched %>% mutate(cdm_name = input$cdmName) 
  
  write_csv(output$lsc_matched, here("Results",db_name, paste0(
    "lsc_matched_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
  )))
}
toc(log = TRUE)

tic("LargeScaleChar sample")
if (input$runMatchedSampleLSC) {
  large_scale_char_sample <- summariseLargeScaleCharacteristics(
    cohort=cdm$sample,
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                  c(0, 0), 
                  c(1, 30), c(31, 365),  c(366, Inf)),
    eventInWindow = c("condition_occurrence", "visit_occurrence",
                      "measurement", "procedure_occurrence",  "observation"), 
    episodeInWindow = c("drug_era"),
    minimumFrequency = 0.0005
  )
  
  large_scale_char_sample <- large_scale_char_sample %>% 
    filter(variable_name != "settings") %>% 
    omopgenerics::suppress(minCellCount = 5)
  
  output$lsc_sample <- large_scale_char_sample %>% mutate(cdm_name = input$cdmName)
  
  write_csv(output$lsc_sample, here("Results",db_name, paste0(
    "lsc_sample_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
  )))
}
toc(log = TRUE)

tic("LargeScaleChar difference")
if (input$runMatchedSampleLSC) {
  difference <- large_scale_char_sample  %>% 
    left_join( large_scale_char_matched, 
               by = join_by(result_type, cdm_name, 
                            group_name, group_level,
                            strata_name, strata_level,  
                            additional_name, additional_level,
                            variable_name, variable_level,
                            estimate_type ), relationship = "many-to-many") %>% 
    mutate(numx =as.double(`estimate_value.x`),
           numy =as.double(`estimate_value.y`)) %>%
    mutate(difference =(numx-numy)/numy )
  
  # rm(matched_cohort)
  
  output$lsc_difference <- difference %>% mutate(cdm_name = input$cdmName) 
  
  write_csv(output$lsc_difference, here("Results",db_name, paste0(
    "lsc_difference_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
  )))
}
toc(log = TRUE)


# Step 6: Incidence Rates ################
# Stratified by Age 10y, Gender, Calendar Year
# For now stratified by kid-Adult-Older Adult 
 
tic(msg = "Incidence Prevalence Sampling + Denominator")
if (input$runIncidence|input$runPrevalence) {
  if (is.null(input$sampleIncidencePrevalence)) {
    cdmSampled <- cdm 
  } else{
    cdmSampled <- cdmSample(cdm, n = input$sampleIncidencePrevalence)
  }
  
  cdmSampled <- generateDenominatorCohortSet(
    cdm = cdmSampled, 
    name = "denominator", 
    ageGroup = list(c(0,17), c(18,64),
                    c(65,199)),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180
  )
}

toc(log = TRUE)

tic(msg = "Incidence by year, age, sex")

if (input$runIncidence ) {
  
  output$incidence <- estimateIncidence(
    cdm = cdmSampled,
    denominatorTable = "denominator",
    outcomeTable = cohorts_name,
    interval = "years",
    repeatedEvents = FALSE,
    outcomeWashout = Inf,
    completeDatabaseIntervals = FALSE,
    minCellCount = 5 ) 
  
  write_csv(output$incidence , here("Results", db_name, paste0(
    "incidence_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
  )))
  
}

toc(log = TRUE)


tic(msg = "Prevalence by year, age, sex")

if (input$runPrevalence ) {
  output$prevalence <- estimatePeriodPrevalence(
    cdm = cdmSampled,
    denominatorTable = "denominator",
    outcomeTable = cohorts_name,
    interval = "years",
    completeDatabaseIntervals = TRUE,
    fullContribution = FALSE,
    minCellCount = 5
  )
  write_csv(output$prevalence, here("Results",db_name, paste0(
    "prevalence_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
  )))
}


toc(log = TRUE)

rm(cdmSampled)

# Log close ############

toc(log = TRUE)
tic.log(format = TRUE)
tic_log <- tic.log(format = TRUE)

output$log <- tibble(cdm_name = input$cdmName, log = paste0(tic_log %>%  unlist(), collapse = "\n"))
write_csv(output$log, here("Results", db_name, paste0(
  "log_", cdmName(cdm), "_" ,format(Sys.time(), "%Y_%m_%d"), ".csv"
)))


# zip Results -----
# zip all Results -----
cli::cli_text("- Zipping Results ({Sys.time()})")
files_to_zip <- list.files(here("Results", db_name))
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        db_name)]
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        ".csv")]

zip::zip(zipfile = file.path(paste0(
  here("Results", db_name), "/Results_", study_prefix, db_name, ".zip"
)),
files = files_to_zip,
root = here("Results", db_name ))

if (input$exportResultsRData) {
  analyses_performed <- as.integer(c(input$runGenerateCohort, 
                                     input$runCalculateOverlap,
                                     input$runCountCodes,
                                     input$runIndexEvents,
                                     input$runProfiling, 
                                     input$runMatchedSampleLSC, 
                                     input$runIncidence, 
                                     input$runPrevalence, 
                                     !is.null(input$sampleIncidencePrevalence)
  ))
  
  analyses_performed <-  paste(analyses_performed , collapse = "_")
  
  save(input, output, 
       file = here(paste0("Results/", db_name, "/", input$cdmName, "_", cohorts_name, analyses_performed, "_" ,format(Sys.time(), "%Y_%m_%d") , ".RData")))
}


