# code for getting denominator and estimating incidence below

if(isTRUE(run_incidence)){
#get denominator ------
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

# add year of birth onto denominator
cdm$denominator <- cdm$denominator %>% 
  dplyr::left_join(cdm$person %>%
                     select("person_id",  "year_of_birth") %>%
                     distinct(),
                   by = c("subject_id"= "person_id"))

# create groups based on year of birth
cdm$denominator <- cdm$denominator %>% 
  PatientProfiles::addCategories(
    variable = "year_of_birth",
    categories = list("birth_year_group" = list(
      "1889 to 1893" = c(1889, 1893),
      "1894 to 1898" = c(1894, 1898),
      "1899 to 1903" = c(1899, 1903),
      "1904 to 1908" = c(1904, 1908),
      "1909 to 1913" = c(1909, 1913),
      "1914 to 1918" = c(1914, 1918),
      "1919 to 1923" = c(1919, 1923),
      "1924 to 1928" = c(1924, 1928),
      "1929 to 1933" = c(1929, 1933),
      "1934 to 1938" = c(1934, 1938),
      "1939 to 1943" = c(1939, 1943),
      "1944 to 1948" = c(1944, 1948),
      "1949 to 1953" = c(1949, 1953),
      "1954 to 1958" = c(1954, 1958),
      "1959 to 1963" = c(1959, 1963),
      "1964 to 1968" = c(1964, 1968),
      "1969 to 1973" = c(1969, 1973),
      "1974 to 1978" = c(1974, 1978),
      "1979 to 1983" = c(1979, 1983),
      "1984 to 1988" = c(1984, 1988),
      "1989 to 1993" = c(1989, 1993),
      "1994 to 1998" = c(1994, 1998),
      "1999 to 2003" = c(1999, 2003),
      "2004 to 2008" = c(2004, 2008),
      "2009 to 2013" = c(2009, 2013),
      "2014 to 2018" = c(2014, 2018),
      "2019 to 2023" = c(2019, 2023)                            
      
    )
    )
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
  returnParticipants = FALSE,
  strata = list("birth_year_group") ,
  includeOverallStrata = TRUE
)

cli::cli_alert_info("- Getting participants from incidence")
#getting participants for survival analysis
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_parts" ,
  cohortDateRange = c(as.Date(study_start), as.Date("2023-01-01")),
  ageGroup =list(
    c(18, 150)),
  sex = c("Both"),
  daysPriorObservation = 365
)

# add year of birth onto denominator
cdm$denominator_parts <- cdm$denominator_parts %>% 
  dplyr::left_join(cdm$person %>%
                     select("person_id",  "year_of_birth") %>%
                     distinct(),
                   by = c("subject_id"= "person_id"))

# create groups based on year of birth
cdm$denominator_parts <- cdm$denominator_parts %>% 
  PatientProfiles::addCategories(
    variable = "year_of_birth",
    categories = list("birth_year_group" = list(
      "1889 to 1893" = c(1889, 1893),
      "1894 to 1898" = c(1894, 1898),
      "1899 to 1903" = c(1899, 1903),
      "1904 to 1908" = c(1904, 1908),
      "1909 to 1913" = c(1909, 1913),
      "1914 to 1918" = c(1914, 1918),
      "1919 to 1923" = c(1919, 1923),
      "1924 to 1928" = c(1924, 1928),
      "1929 to 1933" = c(1929, 1933),
      "1934 to 1938" = c(1934, 1938),
      "1939 to 1943" = c(1939, 1943),
      "1944 to 1948" = c(1944, 1948),
      "1949 to 1953" = c(1949, 1953),
      "1954 to 1958" = c(1954, 1958),
      "1959 to 1963" = c(1959, 1963),
      "1964 to 1968" = c(1964, 1968),
      "1969 to 1973" = c(1969, 1973),
      "1974 to 1978" = c(1974, 1978),
      "1979 to 1983" = c(1979, 1983),
      "1984 to 1988" = c(1984, 1988),
      "1989 to 1993" = c(1989, 1993),
      "1994 to 1998" = c(1994, 1998),
      "1999 to 2003" = c(1999, 2003),
      "2004 to 2008" = c(2004, 2008),
      "2009 to 2013" = c(2009, 2013),
      "2014 to 2018" = c(2014, 2018),
      "2019 to 2023" = c(2019, 2023)                            
      
    )
    )
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
  returnParticipants = TRUE
)

cli::cli_alert_info("- Carry out age standardization for incidence using european standard population")
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
agestandardizedinc <- list()

# filter out to only include rates
inc_std <- inc %>% 
  filter(denominator_age_group != "18 to 150",
         denominator_sex == "Both",
         strata_name == "Overall" ,
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events ,                       
    person_years,                
    incidence_100000_pys ,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group ,
    age_standard ))

#create a loop for each cancer phenotype
agestandardizedincf <- list()

inc_std_F <- inc %>% 
  filter(denominator_age_group != "18 to 150",
         denominator_sex == "Female",
         strata_name == "Overall" ,
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events ,                       
    person_years,                
    incidence_100000_pys ,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group ,
    age_standard ))


#create a loop for each cancer phenotype
agestandardizedincm <- list()

inc_std_M <- inc %>% 
  filter(denominator_age_group != "18 to 150",
         denominator_sex == "Male",
         strata_name == "Overall" ,
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events ,                       
    person_years,                
    incidence_100000_pys ,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group ,
    age_standard ))

# overall population
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

# females
for(i in 1:length(table(inc_std_F$outcome_cohort_name))){
  
  incidence_estimates_i <- inc_std_F %>%
    filter(outcome_cohort_name == names(table(inc_std_M$outcome_cohort_name)[i]))
  
  agestandardizedincf[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedincf[[i]] <- agestandardizedincf[[i]] %>% 
    mutate(outcome_cohort_name = names(table(inc_std_F$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- european age standardization for ", names(table(inc_std_F$outcome_cohort_name)[i]), " FEMALES complete"))
  
}

# males
for(i in 1:length(table(inc_std_M$outcome_cohort_name))){
  
  incidence_estimates_i <- inc_std_M %>%
    filter(outcome_cohort_name == names(table(inc_std_M$outcome_cohort_name)[i]))
  
  agestandardizedincm[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedincm[[i]] <- agestandardizedincm[[i]] %>% 
    mutate(outcome_cohort_name = names(table(inc_std_M$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- european age standardization for ", names(table(inc_std_M$outcome_cohort_name)[i]), " MALES complete"))
  
}


agestandardizedinc_final_esp <- bind_rows(agestandardizedinc) %>% 
  mutate(cdm_name = db_name) %>% 
  rename(incidence_100000_pys = `Std Rate (per 1e+05)`,
         incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
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


agestandardizedinc_final_espf <- bind_rows(agestandardizedincf) %>% 
  mutate(cdm_name = db_name) %>% 
  rename(incidence_100000_pys = `Std Rate (per 1e+05)`,
         incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
         incidence_start_date = Subgroup,
         person_years = Denominator ,
         n_events = Numerator ) %>% 
  mutate(denominator_sex = "Female",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1e+05)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`)) %>% 
  mutate(age_standard = "European Standard Population")

agestandardizedinc_final_espm <- bind_rows(agestandardizedincm) %>% 
  mutate(cdm_name = db_name) %>% 
  rename(incidence_100000_pys = `Std Rate (per 1e+05)`,
         incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
         incidence_start_date = Subgroup,
         person_years = Denominator ,
         n_events = Numerator ) %>% 
  mutate(denominator_sex = "Male",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1e+05)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`)) %>% 
  mutate(age_standard = "European Standard Population")

agestandardizedinc_final_esp <- bind_rows(agestandardizedinc_final_esp,
                                          agestandardizedinc_final_espf,
                                          agestandardizedinc_final_espm)

cli::cli_alert_info("- Age standardization for incidence using european standard population completed")


cli::cli_alert_info("- Carry out age standardization for incidence using world standard population")
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
agestandardizedinc_wsp <- list()

# overall
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

# females
#create a loop for each cancer phenotype
agestandardizedinc_wspf <- list()

for(i in 1:length(table(inc_std_F$outcome_cohort_name))){
  
  incidence_estimates_i <- inc_std_F %>%
    filter(outcome_cohort_name == names(table(inc_std_F$outcome_cohort_name)[i]))
  
  agestandardizedinc_wspf[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = WSP2000_2025_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedinc_wspf[[i]] <- agestandardizedinc_wspf[[i]] %>% 
    mutate(outcome_cohort_name = names(table(inc_std_F$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- world age standardization for ", names(table(inc_std_F$outcome_cohort_name)[i]), " FEMALES complete"))
  
}

# males
agestandardizedinc_wspm <- list()

for(i in 1:length(table(inc_std_M$outcome_cohort_name))){
  
  incidence_estimates_i <- inc_std_M %>%
    filter(outcome_cohort_name == names(table(inc_std_M$outcome_cohort_name)[i]))
  
  agestandardizedinc_wspm[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = WSP2000_2025_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedinc_wspm[[i]] <- agestandardizedinc_wspm[[i]] %>% 
    mutate(outcome_cohort_name = names(table(inc_std_M$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- world age standardization for ", names(table(inc_std_M$outcome_cohort_name)[i]), " MALES complete"))
  
}


agestandardizedinc_wsp_final <- bind_rows(agestandardizedinc_wsp) %>% 
  rename(incidence_100000_pys = `Std Rate (per 1e+05)`,
         incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
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

agestandardizedinc_wsp_finalf <- bind_rows(agestandardizedinc_wspf) %>% 
  rename(incidence_100000_pys = `Std Rate (per 1e+05)`,
         incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
         incidence_start_date = Subgroup,
         person_years = Denominator ,
         n_events = Numerator ) %>% 
  mutate(denominator_sex = "Female",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1e+05)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`))  %>% 
  mutate(age_standard = "World Standard Population")

agestandardizedinc_wsp_finalm <- bind_rows(agestandardizedinc_wspm) %>% 
  rename(incidence_100000_pys = `Std Rate (per 1e+05)`,
         incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
         incidence_start_date = Subgroup,
         person_years = Denominator ,
         n_events = Numerator ) %>% 
  mutate(denominator_sex = "Male",
         denominator_age_group = "18 to 150",
         cdm_name = db_name ) %>% 
  as_tibble() %>% 
  select(!c(
    `Crude Rate (per 1e+05)`,
    `95% LCL (Crude)`,
    `95% UCL (Crude)`))  %>% 
  mutate(age_standard = "World Standard Population")


agestandardizedinc_wsp_final <- bind_rows(agestandardizedinc_wsp_final,
                                          agestandardizedinc_wsp_finalf,
                                          agestandardizedinc_wsp_finalm)


cli::cli_alert_success("- Age standardization for incidence using world standard population completed")


# bind the results from the age standardisation together with crude estimates

inc_crude <- inc %>% 
  filter(denominator_age_group == "18 to 150",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events ,                       
    person_years,                
    incidence_100000_pys ,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,                  
    denominator_sex     ,                   
    denominator_age_group  ,
    age_standard ))


agestandardized_results <- bind_rows(
  inc_crude,
  agestandardizedinc_final_esp,
  agestandardizedinc_wsp_final
)


# age standardization per year of birth cohorts
agestandardizedinc_birth <- list()

# filter out to only include rates
inc_std_birth <- inc %>% 
  filter(denominator_age_group != "18 to 150",
         denominator_sex == "Both",
         strata_name != "Overall" ,
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events ,                       
    person_years,                
    incidence_100000_pys ,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name    ,            
    cdm_name  ,  
    strata_level ,
    denominator_sex     ,                   
    denominator_age_group ,
    age_standard ))

# overall population
for(i in 1:length(table(inc_std_birth$outcome_cohort_name))){
  
  incidence_estimates_i <- inc_std_birth %>%
    filter(outcome_cohort_name == names(table(inc_std_birth$outcome_cohort_name)[i]))
  
  birthcohort_temp <- list()
  
  for(j in 1:length(table(inc_std_birth$strata_level))){ 
    
  incidence_estimates_ii <- incidence_estimates_i %>%
      filter(strata_level == names(table(inc_std_birth$strata_level)[j]))
  
  birthcohort_temp[[j]] <- dsr::dsr(
    data = incidence_estimates_ii,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  birthcohort_temp[[j]] <- birthcohort_temp[[j]] %>% 
    mutate(strata_level = names(table(inc_std_birth$strata_level)[j])) 
  
  cli::cli_alert_info(paste0("- european age standardization for birth cohorts ", names(table(inc_std_birth$strata_level)[j]), " complete"))
  
  }
  
  birthcohort_temp <- bind_rows(birthcohort_temp)
  
  agestandardizedinc_birth[[i]] <- birthcohort_temp %>% 
    mutate(outcome_cohort_name = names(table(inc_std_birth$outcome_cohort_name)[i])) 
  
  cli::cli_alert_info(paste0("- european age standardization for birth cohorts ", names(table(inc_std_birth$outcome_cohort_name)[i]), " complete"))
  
}

agestandardizedinc_esp__birthfinal <- bind_rows(agestandardizedinc_birth) %>% 
  rename(incidence_100000_pys = `Std Rate (per 1e+05)`,
         incidence_100000_pys_95CI_lower = `95% LCL (Std)`,
         incidence_100000_pys_95CI_upper = `95% UCL (Std)`,
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
  mutate(age_standard = "European Standard Population")

# to add in other results from wsp and crude and bind


cli::cli_alert_success("- Age standardization for incidence completed")



# Export the results -----
cli::cli_alert_info("- Getting incidence attrition")
write.csv(attrition(inc), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_attrition.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting incidence settings")
write.csv(settings(inc), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_settings.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting incidence results")
write.csv(inc, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_estimates.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting age standardized incidence results")
write.csv(agestandardized_results, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_age_std_incidence_estimates.csv")), row.names = FALSE)

cli::cli_alert_success("Incidence Analysis Complete")


}