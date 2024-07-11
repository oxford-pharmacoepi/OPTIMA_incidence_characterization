# # code for estimating prevalence below

if(isTRUE(run_prevalence)){
  
# if user has not run incidence the denominator will need to be run
  if(isFALSE(run_incidence)){
    
# #get denominator ------
cli::cli_alert_info("- Getting denominator")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator" ,
  cohortDateRange = c(as.Date(study_start), as.Date("2023-01-01")),
  requirementInteractions = TRUE,
  ageGroup =list(
    c(18, 150),
    c(18, 49),
    c(50, 59),
    c(60, 69),
    c(70, 79),
    c(80, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 365
)
cli::cli_alert_success("- Got denominator")
  
  }
  
}


# Estimate prevalence -------
cli::cli_alert_info("- Getting prevalence")

# prevalence until end of observation (total) and partial prevalence at 2 and 5 years
prev <- estimatePeriodPrevalence(
  cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome_p",
  interval = "years",
  completeDatabaseIntervals = TRUE,
  fullContribution = FALSE,
  minCellCount = 0,
  returnParticipants = FALSE
)

cli::cli_alert_success("- Got prevalence")



cli::cli_alert_info("- Carry out age standardization for prevalence using european standard population")
# age standardization by european 13

# read in ESP13 values
ESP13 <- readr::read_csv(here("2_Analysis", "Age_standards", "ESP13.csv"), 
                         show_col_types = FALSE) 

#collapse ESP13 
ESP13_updated <- ESP13 %>% 
  filter(Agegroup != "0-4",
         Agegroup != "5-9",
         Agegroup != "10-14",
         Agegroup != "15-19" ) %>% 
  add_row(Agegroup = "18 to 49", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '20-24'| Agegroup == '25-29' |
                                                                     Agegroup == '30-34' | Agegroup == '35-39' |
                                                                     Agegroup == '35-39' | Agegroup == '40-44' |
                                                                     Agegroup == '45-49']))) %>% 
  add_row(Agegroup = "50 to 59", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '50-54'| Agegroup == '55-59']))) %>% 
  add_row(Agegroup = "60 to 69", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '60-64'| Agegroup == '65-69']))) %>% 
  add_row(Agegroup = "70 to 79", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '70-74'| Agegroup == '75-79']))) %>% 
  add_row(Agegroup = "80 to 150", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '80-84'| Agegroup == '85-89'|Agegroup == '90+']))) %>% 
  filter(Agegroup == "18 to 49" | Agegroup == "50 to 59" | Agegroup == "60 to 69" |
           Agegroup == "70 to 79" |
           Agegroup == "80 to 150" ) 

#rename ESP column to pop (needs to be pop otherwise will not work)
ESP13_updated <- ESP13_updated %>% 
  rename(pop = ESP2013,
         denominator_age_group = Agegroup)


#create a loop for each cancer phenotype
agestandardizedprev <- list()

# filter out to only include rates
prev_std <- prev %>% 
  filter(denominator_age_group != "18 to 150",
         denominator_sex == "Both",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    prevalence_start_date,            
    n_cases ,  
    n_population,
    prevalence ,
    prevalence_95CI_lower,
    prevalence_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group ,
    age_standard ))

#create a loop for each cancer phenotype
agestandardizedprevf <- list()

prev_std_F <- prev %>% 
  filter(denominator_age_group != "18 to 150",
         denominator_sex == "Female",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    prevalence_start_date,            
    n_cases ,  
    n_population,
    prevalence ,
    prevalence_95CI_lower,
    prevalence_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group ,
    age_standard))


#create a loop for each cancer phenotype
agestandardizedprevm <- list()

prev_std_M <- prev %>% 
  filter(denominator_age_group != "18 to 150",
         denominator_sex == "Male",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    prevalence_start_date,            
    n_cases ,  
    n_population,
    prevalence ,
    prevalence_95CI_lower,
    prevalence_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group ,
    age_standard ))

# overall population
for(i in 1:length(table(prev_std$outcome_cohort_name))){
  
  prevalence_estimates_i <- prev_std %>%
    filter(outcome_cohort_name == names(table(prev_std$outcome_cohort_name)[i]))
  
  agestandardizedprev[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of events per stratum 
    fu = n_population , # column containing number of population per stratum
    subgroup = prevalence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 1 population
    decimals = 6) 
  
  agestandardizedprev[[i]] <- agestandardizedprev[[i]] %>% 
    mutate(outcome_cohort_name = names(table(prev_std$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- european age standardization for ", names(table(prev_std$outcome_cohort_name)[i]), " complete"))
  
}

# females
for(i in 1:length(table(prev_std_F$outcome_cohort_name))){
  
  prevalence_estimates_i <- prev_std_F %>%
    filter(outcome_cohort_name == names(table(prev_std_F$outcome_cohort_name)[i]))
  
  agestandardizedprevf[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of events per stratum 
    fu = n_population , # column containing number of population per stratum
    subgroup = prevalence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 1 population
    decimals = 6) 
  
  agestandardizedprevf[[i]] <- agestandardizedprevf[[i]] %>% 
    mutate(outcome_cohort_name = names(table(prev_std_F$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- european age standardization for ", names(table(prev_std_F$outcome_cohort_name)[i]), " FEMALES complete"))
  
}

# males
for(i in 1:length(table(prev_std_M$outcome_cohort_name))){
  
  prevalence_estimates_i <- prev_std_M %>%
    filter(outcome_cohort_name == names(table(prev_std_M$outcome_cohort_name)[i]))
  
  agestandardizedprevm[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of events per stratum 
    fu = n_population , # column containing number of population per stratum
    subgroup = prevalence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 1 population
    decimals = 6) 
  
  agestandardizedprevm[[i]] <- agestandardizedprevm[[i]] %>% 
    mutate(outcome_cohort_name = names(table(prev_std_M$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- european age standardization for ", names(table(prev_std_M$outcome_cohort_name)[i]), " MALES complete"))
  
}


agestandardizedprev_final_esp <- bind_rows(agestandardizedprev) %>% 
  mutate(cdm_name = db_name) %>% 
  rename(prevalence = `Std Rate (per 1)`,
         prevalence_95CI_lower = `95% LCL (Std)`,
         prevalence_95CI_upper = `95% UCL (Std)`,
         prevalence_start_date = Subgroup,
         n_population = Denominator ,
         n_cases = Numerator ) %>% 
  mutate(denominator_sex = "Both",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`)) %>% 
  mutate(age_standard = "European Standard Population")


agestandardizedprev_final_espf <- bind_rows(agestandardizedprevf) %>% 
  mutate(cdm_name = db_name) %>% 
  rename(prevalence = `Std Rate (per 1)`,
         prevalence_95CI_lower = `95% LCL (Std)`,
         prevalence_95CI_upper = `95% UCL (Std)`,
         prevalence_start_date = Subgroup,
         n_population = Denominator ,
         n_cases = Numerator) %>% 
  mutate(denominator_sex = "Female",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`)) %>% 
  mutate(age_standard = "European Standard Population")

agestandardizedprev_final_espm <- bind_rows(agestandardizedprevm) %>% 
  mutate(cdm_name = db_name) %>% 
  rename(prevalence = `Std Rate (per 1)`,
         prevalence_95CI_lower = `95% LCL (Std)`,
         prevalence_95CI_upper = `95% UCL (Std)`,
         prevalence_start_date = Subgroup,
         n_population = Denominator ,
         n_cases = Numerator ) %>% 
  mutate(denominator_sex = "Male",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`)) %>% 
  mutate(age_standard = "European Standard Population")

agestandardizedprev_final_esp <- bind_rows(agestandardizedprev_final_esp,
                                          agestandardizedprev_final_espf,
                                          agestandardizedprev_final_espm)

cli::cli_alert_info("- Age standardization for prevalence using european standard population completed")


cli::cli_alert_info("- Carry out age standardization for prevalence using world standard population")
# age standardization by world standard population
# read in WSP2000_2025 values
WSP2000_2025 <- readr::read_csv(here("2_Analysis", "Age_standards", "WSP_2000_2025.csv"), 
                                show_col_types = FALSE) 

WSP2000_2025$WSP2000_2025 <- WSP2000_2025$WSP2000_2025/ 10

#collapse WSP_2000_2025
WSP2000_2025_updated <- WSP2000_2025 %>% 
  filter(Agegroup != "0-4",
         Agegroup != "5-9",
         Agegroup != "10-14",
         Agegroup != "15-19" ) %>% 
  add_row(Agegroup = "18 to 49", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '20-24'| Agegroup == '25-29' |
                                                                                      Agegroup == '30-34' | Agegroup == '35-39' |
                                                                                      Agegroup == '35-39' | Agegroup == '40-44' |
                                                                                      Agegroup == '45-49']))) %>% 
  add_row(Agegroup = "50 to 59", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '50-54'| Agegroup == '55-59']))) %>% 
  add_row(Agegroup = "60 to 69", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '60-64'| Agegroup == '65-69']))) %>% 
  add_row(Agegroup = "70 to 79", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '70-74'| Agegroup == '75-79']))) %>% 
  add_row(Agegroup = "80 to 150", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '80-84'| Agegroup == '85-89'|Agegroup == '90+']))) %>% 
  filter(Agegroup == "18 to 49" | Agegroup == "50 to 59" | Agegroup == "60 to 69" |
           Agegroup == "70 to 79" |
           Agegroup == "80 to 150" ) 

#rename WSP column to pop (needs to be pop otherwise will not work)
WSP2000_2025_updated <- WSP2000_2025_updated %>% 
  rename(pop = WSP2000_2025,
         denominator_age_group = Agegroup)

#create a loop for each cancer phenotype
agestandardizedprev_wsp <- list()

# overall
for(i in 1:length(table(prev_std$outcome_cohort_name))){
  
  prevalence_estimates_i <- prev_std %>%
    filter(outcome_cohort_name == names(table(prev_std$outcome_cohort_name)[i]))
  
  agestandardizedprev_wsp[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of deaths per stratum 
    fu = n_population , # column containing number of population per stratum person years
    subgroup = prevalence_start_date,   
    refdata = WSP2000_2025_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 100.000 population
    decimals = 6) 
  
  agestandardizedprev_wsp[[i]] <- agestandardizedprev_wsp[[i]] %>% 
    mutate(outcome_cohort_name = names(table(prev_std$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- world age standardization for ", names(table(prev_std$outcome_cohort_name)[i]), " complete"))
  
}

# females
#create a loop for each cancer phenotype
agestandardizedprev_wspf <- list()

for(i in 1:length(table(prev_std_F$outcome_cohort_name))){
  
  prevalence_estimates_i <- prev_std_F %>%
    filter(outcome_cohort_name == names(table(prev_std_F$outcome_cohort_name)[i]))
  
  agestandardizedprev_wspf[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of deaths per stratum 
    fu = n_population , # column containing number of population per stratum person years
    subgroup = prevalence_start_date,   
    refdata = WSP2000_2025_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 100.000 population
    decimals = 6) 
  
  agestandardizedprev_wspf[[i]] <- agestandardizedprev_wspf[[i]] %>% 
    mutate(outcome_cohort_name = names(table(prev_std_F$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- world age standardization for ", names(table(prev_std_F$outcome_cohort_name)[i]), " FEMALES complete"))
  
}

# males
agestandardizedprev_wspm <- list()

for(i in 1:length(table(prev_std_M$outcome_cohort_name))){
  
  prevalence_estimates_i <- prev_std_M %>%
    filter(outcome_cohort_name == names(table(prev_std_M$outcome_cohort_name)[i]))
  
  agestandardizedprev_wspm[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of deaths per stratum 
    fu = n_population , # column containing number of population per stratum person years
    subgroup = prevalence_start_date,   
    refdata = WSP2000_2025_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 100.000 population
    decimals = 6) 
  
  agestandardizedprev_wspm[[i]] <- agestandardizedprev_wspm[[i]] %>% 
    mutate(outcome_cohort_name = names(table(prev_std_M$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- world age standardization for ", names(table(prev_std_M$outcome_cohort_name)[i]), " MALES complete"))
  
}


agestandardizedprev_wsp_final <- bind_rows(agestandardizedprev_wsp) %>% 
  rename(prevalence = `Std Rate (per 1)`,
         prevalence_95CI_lower = `95% LCL (Std)`,
         prevalence_95CI_upper = `95% UCL (Std)`,
         prevalence_start_date = Subgroup,
         n_population = Denominator ,
         n_cases = Numerator ) %>% 
  mutate(denominator_sex = "Both",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`))  %>% 
  mutate(age_standard = "World Standard Population")

agestandardizedprev_wsp_finalf <- bind_rows(agestandardizedprev_wspf) %>% 
  rename(prevalence = `Std Rate (per 1)`,
         prevalence_95CI_lower = `95% LCL (Std)`,
         prevalence_95CI_upper = `95% UCL (Std)`,
         prevalence_start_date = Subgroup,
         n_population = Denominator ,
         n_cases = Numerator ) %>% 
  mutate(denominator_sex = "Female",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`))  %>% 
  mutate(age_standard = "World Standard Population")

agestandardizedprev_wsp_finalm <- bind_rows(agestandardizedprev_wspm) %>% 
  rename(prevalence = `Std Rate (per 1)`,
         prevalence_95CI_lower = `95% LCL (Std)`,
         prevalence_95CI_upper = `95% UCL (Std)`,
         prevalence_start_date = Subgroup,
         n_population = Denominator ,
         n_cases = Numerator ) %>% 
  mutate(denominator_sex = "Male",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`))  %>% 
  mutate(age_standard = "World Standard Population")


agestandardizedprev_wsp_final <- bind_rows(agestandardizedprev_wsp_final,
                                          agestandardizedprev_wsp_finalf,
                                          agestandardizedprev_wsp_finalm)


cli::cli_alert_success("- Age standardization for prevalence using world standard population completed")


# bind the results from the age standardisation together with crude estimates

prev_crude <- prev %>% 
  filter(denominator_age_group == "18 to 150",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    prevalence_start_date,            
    n_cases ,                       
    n_population,                
    prevalence ,
    prevalence_95CI_lower,
    prevalence_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group  ,
    age_standard ))


agestandardized_results_prev <- bind_rows(
  prev_crude,
  agestandardizedprev_final_esp,
  agestandardizedprev_wsp_final
)

cli::cli_alert_success("- Age standardization for prevalence completed")


# # Export the results -----
cli::cli_alert_info("- Getting prevalence attrition")
write.csv(attrition(prev), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_prevalence_attrition.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting prevalence settings")
write.csv(settings(prev), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_prevalence_settings.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting prevalence results")
write.csv(prev, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_prevalence_estimates.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting age standardized prevalence results")
write.csv(agestandardized_results_prev, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_age_std_prevalence_estimates.csv")), row.names = FALSE)


cli::cli_alert_success("Prevalence Analysis Complete")