# code for getting denominator and estimating incidence below

if(isTRUE(run_incidence)){
  #get denominator ------
  cli::cli_alert_info("- Getting denominator")
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator" ,
    cohortDateRange = c(as.Date(study_start), as.Date("2022-12-31")),
    requirementInteractions = TRUE,
    ageGroup =list(
      c(18, 150),
      c(18, 49),
      c(50, 59),
      c(60, 69),
      c(70, 79),
      c(80, 89),
      c(90, 150)
    ),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 365
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
    completeDatabaseIntervals = TRUE
  ) 
  

  # get the incidence estimates
  inc_tidy <- inc %>% 
    visOmopResults::splitAdditional() %>% 
    visOmopResults::splitGroup() %>% 
    visOmopResults::addSettings() %>% 
    filter(variable_name == "Outcome") %>% 
    omopgenerics::pivotEstimates(pivotEstimatesBy = "estimate_name")
  
  # if count = 0 and incidence = make upper confidence interval = 0
  inc_tidy <- inc_tidy %>%
    mutate(incidence_100000_pys_95CI_upper = if_else(
      outcome_count == 0 & incidence_100000_pys == 0 & incidence_100000_pys_95CI_lower == 0,
      0,
      incidence_100000_pys_95CI_upper
    ))
  
  
  
  # get the denominator related results which contain the person years and denominator count
  inc_tidy1 <- inc %>% 
    visOmopResults::splitAdditional() %>% 
    visOmopResults::splitGroup() %>% 
    visOmopResults::addSettings() %>% 
    filter(variable_name == "Denominator") %>% 
    omopgenerics::pivotEstimates(pivotEstimatesBy = "estimate_name") %>% 
    select(
      person_years,
      denominator_count
    )
  
  inc_tidy <- bind_cols(inc_tidy, inc_tidy1)
  
  
  cli::cli_alert_success("- Got incidence")
  
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
    add_row(Agegroup = "80 to 89", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '80-84'| Agegroup == '85-89']))) %>% 
    add_row(Agegroup = "90 to 150", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '90+']))) %>% 
    filter(Agegroup == "18 to 49" | Agegroup == "50 to 59" | Agegroup == "60 to 69" |
             Agegroup == "70 to 79" |
             Agegroup == "80 to 89" |
             Agegroup == "90 to 150" ) 
  
  #rename ESP column to pop (needs to be pop otherwise will not work)
  ESP13_updated <- ESP13_updated %>% 
    rename(pop = ESP2013,
           denominator_age_group = Agegroup)
  
  
  #create a loop for each cancer phenotype
  agestandardizedinc <- list()
  
  # filter out to only include rates
  inc_std <- inc_tidy %>% 
    filter(denominator_age_group != "18 to 150",
           denominator_sex == "Both",
           analysis_interval == "years",
           incidence_start_date != "overall") %>% 
    mutate(age_standard = "Crude") %>% 
    select(c(
      incidence_start_date,            
      outcome_count ,                       
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
  
  inc_std_F <- inc_tidy %>% 
    filter(denominator_age_group != "18 to 150",
           denominator_sex == "Female",
           analysis_interval == "years",
           incidence_start_date != "overall") %>% 
    mutate(age_standard = "Crude") %>% 
    select(c(
      incidence_start_date,            
      outcome_count ,                       
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
  
  inc_std_M <- inc_tidy %>% 
    filter(denominator_age_group != "18 to 150",
           denominator_sex == "Male",
           analysis_interval == "years",
           incidence_start_date != "overall") %>% 
    mutate(age_standard = "Crude") %>% 
    select(c(
      incidence_start_date,            
      outcome_count ,                       
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
    
    agestandardizedinc[[i]] <- dsr(
      data = incidence_estimates_i,  # specify object containing number of deaths per stratum
      event = outcome_count ,       # column containing number of deaths per stratum 
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
      filter(outcome_cohort_name == names(table(inc_std_F$outcome_cohort_name)[i]))
    
    agestandardizedincf[[i]] <- dsr(
      data = incidence_estimates_i,  # specify object containing number of deaths per stratum
      event = outcome_count ,        # column containing number of deaths per stratum 
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
    
    agestandardizedincm[[i]] <- dsr(
      data = incidence_estimates_i,  # specify object containing number of deaths per stratum
      event = outcome_count ,        # column containing number of deaths per stratum 
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
           outcome_count = Numerator ) %>% 
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
           outcome_count = Numerator ) %>% 
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
           outcome_count = Numerator ) %>% 
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
    add_row(Agegroup = "80 to 89", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '80-84'| Agegroup == '85-89']))) %>% 
    add_row(Agegroup = "90 to 150", WSP2000_2025 = with(WSP2000_2025, sum(WSP2000_2025[Agegroup == '90 +']))) %>% 
    filter(Agegroup == "18 to 49" | Agegroup == "50 to 59" | Agegroup == "60 to 69" |
             Agegroup == "70 to 79" |
             Agegroup == "80 to 89" |
             Agegroup == "90 to 150" ) 
  
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
    
    agestandardizedinc_wsp[[i]] <- dsr(
      data = incidence_estimates_i,  # specify object containing number of deaths per stratum
      event = outcome_count ,        # column containing number of deaths per stratum 
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
    
    agestandardizedinc_wspf[[i]] <- dsr(
      data = incidence_estimates_i,  # specify object containing number of deaths per stratum
      event = outcome_count ,       # column containing number of deaths per stratum 
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
    
    agestandardizedinc_wspm[[i]] <- dsr(
      data = incidence_estimates_i,  # specify object containing number of deaths per stratum
      event = outcome_count ,      # column containing number of deaths per stratum 
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
           outcome_count = Numerator ) %>% 
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
           outcome_count = Numerator ) %>% 
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
           outcome_count = Numerator ) %>% 
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
  
  inc_crude <- inc_tidy %>% 
    filter(denominator_age_group == "18 to 150",
           incidence_start_date != "overall" ,
           analysis_interval == "years") %>% 
    mutate(age_standard = "Crude") %>% 
    select(c(
      incidence_start_date,            
      outcome_count ,                       
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
  
  cli::cli_alert_success("- Age standardization for incidence completed")
  
  # suppress the results
  
  if (grepl("THIN", db_name, ignore.case = TRUE)) {
  
  # incidence results suppress less than 10 but keep 0
  inc_tidy <- inc_tidy %>%
    mutate(
      outcome_count = if_else(outcome_count <= 10 & outcome_count != 0, NA_integer_, outcome_count),
      across(
        c(incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper), 
        ~ if_else(outcome_count <= 10 & outcome_count != 0, NA_real_, .)
      )
    )
  
  # age standardized results
  agestandardized_results <- agestandardized_results %>% 
    mutate(
      outcome_count = if_else(outcome_count <= 10 & outcome_count != 0, NA_integer_, outcome_count),
      across(
        c(incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper), 
        ~ if_else(outcome_count <= 10 & outcome_count != 0, NA_real_, .)
      )
    )
    
    
  
  } else {
    
    # incidence results suppress less than 5 but keep 0
    inc_tidy <- inc_tidy %>%
      mutate(
        outcome_count = if_else(outcome_count <= 5 & outcome_count != 0, NA_integer_, outcome_count),
        across(
          c(incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper), 
          ~ if_else(outcome_count <= 5 & outcome_count != 0, NA_real_, .)
        )
      )
    
    agestandardized_results <- agestandardized_results %>% 
    mutate(
      outcome_count = if_else(outcome_count <= 5 & outcome_count != 0, NA_integer_, outcome_count),
      across(
        c(incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper), 
        ~ if_else(outcome_count <= 5 & outcome_count != 0, NA_real_, .)
      )
    )
    
  }

  
  # Export the results -----
  cli::cli_alert_info("- Getting incidence attrition")
  write_csv(tableIncidenceAttrition(inc, type = "tibble"), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_attrition.csv")))
  
  cli::cli_alert_info("- Getting incidence settings")
  write_csv(settings(inc), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_settings.csv")))
  
  cli::cli_alert_info("- Getting incidence results")
  write_csv(inc_tidy, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_estimates.csv")))
  
  cli::cli_alert_info("- Getting age standardized incidence results")
  write_csv(agestandardized_results, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_estimates_age_std_.csv")))
  
  cli::cli_alert_success("Incidence Analysis Complete")
  
  
  
}