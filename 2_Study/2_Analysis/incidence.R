# code for getting denominator and estimating incidence below

#get denominator ------
cli::cli_alert_info("- Getting denominator")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator" ,
  cohortDateRange = c(as.Date("2000-01-01"), as.Date("2022-01-01")),
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
  daysPriorObservation = 365,
  overwrite = TRUE
)
cli::cli_alert_success("- Got denominator")

# Estimate incidence -------
cli::cli_alert_info("- Getting incidence")

#incidence
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  denominatorCohortId = NULL,
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 0,
  temporary = TRUE,
  returnParticipants = FALSE
)


cli::cli_alert_info("- Getting participants from incidence")
#getting participants for survival analysis
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_parts" ,
  cohortDateRange = c(as.Date("2000-01-01"), as.Date("2022-01-01")),
  ageGroup =list(
    c(18, 150)),
  sex = c("Both"),
  daysPriorObservation = 365,
  overwrite = TRUE
)

inc_overall_parts <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_parts",
  outcomeTable = "outcome",
  denominatorCohortId = NULL,
  interval = c("overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 0,
  temporary = FALSE,
  returnParticipants = TRUE
)

cli::cli_alert_info("- Carry out age standardization for incidence using european standard population")
# age standardization by european 13

# read in ESP13 values
ESP13 <- readr::read_csv(here("4_Age_standards", "ESP13.csv"), 
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
  add_row(Agegroup = "80 +", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '80-84'| Agegroup == '85-89'|Agegroup == '90+']))) %>% 
  filter(Agegroup == "18 to 49" | Agegroup == "50 to 59" | Agegroup == "60 to 69" |
           Agegroup == "70 to 79" |
           Agegroup == "80 +" ) 

#rename ESP column to pop (needs to be pop otherwise will not work)
ESP13_updated <- ESP13_updated %>% 
  rename(pop = ESP2013)

#create a loop for each cancer phenotype
agestandardizedinc <- list()

# filter out to only include rates
inc_std <- inc %>% 
  filter(denominator_age_group != "18 to 150") %>% 
  filter(denominator_sex == "Both") %>% 
  filter(analysis_interval == "years") %>% 
  rename(Agegroup = denominator_age_group) %>% 
  mutate(age_standard = "crude")
  

for(i in 1:length(table(inc_std$outcome_cohort_name))){
  
  incidence_estimates_i <- inc_std %>%
    filter(outcome_cohort_name == names(table(inc_std$outcome_cohort_name)[i]))
  
  agestandardizedinc[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedinc[[i]] <- agestandardizedinc[[i]] %>% 
    mutate(outcome_cohort_name = names(table(inc_std$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- european age standardization for ", names(table(inc_std$outcome_cohort_name)[i]), " complete"))
  
}

agestandardizedinc_final_esp <- bind_rows(agestandardizedinc) %>% 
  mutate(cdm_name = db_name) %>% 
  rename(age_std_incidence_100000_pys = `Std Rate (per 1e+05)`,
         age_std_incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         age_std_incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
         incidence_start_date = Subgroup,
         person_years = Denominator ,
         n_events = Numerator ) %>% 
  mutate(denominator_sex = "Both",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1e+05)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`)) %>% 
  mutate(age_standard = "European Standard Population")

cli::cli_alert_info("- Age standardization for incidence using european standard population completed")


cli::cli_alert_info("- Carry out age standardization for incidence using world standard population")
# age standardization by world standard population
# read in WSP2000_2025 values
WSP2000_2025 <- readr::read_csv(here("4_Age_standards", "WSP_2000_2025.csv"), 
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
  add_row(Agegroup = "80 +", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '80-84'| Agegroup == '85-89'|Agegroup == '90+']))) %>% 
  filter(Agegroup == "18 to 49" | Agegroup == "50 to 59" | Agegroup == "60 to 69" |
           Agegroup == "70 to 79" |
           Agegroup == "80 +" ) 

#rename WSP column to pop (needs to be pop otherwise will not work)
WSP2000_2025_updated <- WSP2000_2025_updated %>% 
  rename(pop = WSP2000_2025)

#create a loop for each cancer phenotype
agestandardizedinc_wsp <- list()

for(i in 1:length(table(inc_std$outcome_cohort_name))){
  
  incidence_estimates_i <- inc_std %>%
    filter(outcome_cohort_name == names(table(inc_std$outcome_cohort_name)[i]))
  
  agestandardizedinc_wsp[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = WSP2000_2025_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedinc_wsp[[i]] <- agestandardizedinc_wsp[[i]] %>% 
    mutate(outcome_cohort_name = names(table(inc_std$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- world age standardization for ", names(table(inc_std$outcome_cohort_name)[i]), " complete"))
  
}

agestandardizedinc_wsp_final <- bind_rows(agestandardizedinc_wsp) %>% 
  rename(age_std_incidence_100000_pys = `Std Rate (per 1e+05)`,
         age_std_incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         age_std_incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
         incidence_start_date = Subgroup,
         person_years = Denominator ,
         n_events = Numerator ) %>% 
  mutate(denominator_sex = "Both",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1e+05)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`))  %>% 
  mutate(age_standard = "World Standard Population")

cli::cli_alert_success("- Age standardization for incidence using world standard population completed")


# Export the results -----
cli::cli_alert_info("- Getting incidence attrition")
write.csv(IncidencePrevalence::incidenceAttrition(inc), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_attrition.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting incidence settings")
write.csv(IncidencePrevalence::incidenceSet(inc), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_settings.csv")), row.names = FALSE)

# inc1 <- inc %>% 
#   filter(!is.na(n_events)) %>% 
#   filter(!is.na(n_persons))

cli::cli_alert_info("- Getting incidence results")
#write.csv(IncidencePrevalence:::obscureCounts(inc1, minCellCount = 5), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_estimates.csv")), row.names = FALSE)
write.csv(inc, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_estimates.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting age standardized incidence results")
write.csv(agestandardizedinc_final_esp, here::here("Results", paste0(db_name, "/", cdmName(cdm), "age_std_eur_incidence_estimates.csv")), row.names = FALSE)

cli::cli_alert_success("Incidence Analysis Complete")