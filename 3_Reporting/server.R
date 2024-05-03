#### SERVER ------
server <-	function(input, output, session) {
  
  
  # Markdown ----
  output$markdown <- renderUI({
    
    table <- cohort_set %>% 
      filter(cohort_name %in% input$cohort_set_input) %>% 
      pull(markdown) %>% 
      formatMarkdown()
  })
  # JSON ----
  output$verb <- renderPrint({
    
    json_content <- cohort_set %>% 
      filter(cohort_name %in% input$cohort_set_input) %>%
      pull(json) %>%
      unlist()
    
    cat(json_content)
    
  })
  
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy to clipboard",
      clipText = isolate(cohort_set %>%
                           filter(cohort_name %in% input$cohort_set_input) %>%
                           pull(json) %>%
                           unlist()),
      icon = icon("clipboard"),
      placement = "top",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  })

  
  # cdm snapshot------
  output$tbl_cdm_snaphot <- renderText(kable(snapshotcdm) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(snapshotcdm)
      gtsave(x, file)
    }
  )
  
  
  # incidence attrition -----
  get_table_attrition <-reactive({
    
    validate(need(input$attrition_database_name_selector != "", "Please select a database"))
    validate(need(input$attrition_outcome_selector != "", "Please select an outcome"))
    validate(need(input$attrition_sex_selector != "", "Please select sex group"))
    validate(need(input$attrition_age_selector != "", "Please select age group"))
    validate(need(input$attrition_time_selector != "", "Please select time period"))
    
    
    
    table <- incidence_attrition %>% 
      filter(cdm_name %in% input$attrition_database_name_selector) %>% 
      filter(outcome_cohort_name %in% input$attrition_outcome_selector) %>% 
      filter(denominator_sex %in% input$attrition_sex_selector) %>% 
      filter(denominator_age_group %in% input$attrition_age_selector) %>% 
      filter(analysis_interval %in% input$attrition_time_selector) %>% 
      select(-c(analysis_id,
                denominator_target_cohort_definition_id,
                denominator_target_cohort_name,
                denominator_end_date,
                denominator_start_date,
                outcome_cohort_id,
                analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_cohort_id,
                analysis_min_cell_count,
                analysis_repeated_events,
                denominator_cohort_name,
                denominator_days_prior_observation,
                denominator_target_cohort_name
      )) %>% 
      rename(`Persons (n)` = number_subjects,
             `Records (n)` = number_records,
             Reason = reason,
             `Reason ID` = reason_id,
             `Excluded Persons (n)` = excluded_subjects,
             `Excluded Records (n)` = excluded_records,
             Age = denominator_age_group,
             `Cohort Name` = outcome_cohort_name, 
             Sex = denominator_sex,
             `Time Interval`= analysis_interval,
             Database = cdm_name) %>% 
      relocate(`Reason ID`, Reason, .before = `Records (n)`)
    
    table
    
  }) 
  
  output$tbl_incidence_attrition <- renderText(kable(get_table_attrition()) %>%
                                             kable_styling("striped", full_width = F) )
  
  output$dt_incidence_attrition_word <- downloadHandler(
    filename = function() {
      "incidence_attrition.docx"
    },
    content = function(file) {
      x <- gt(get_table_attrition())
      gtsave(x, file)
    }
  )

  # prevalence attrition -----
  get_table_attritionp <-reactive({
    
    validate(need(input$attrition_database_name_selectorp != "", "Please select a database"))
    validate(need(input$attrition_outcome_selectorp != "", "Please select an outcome"))
    validate(need(input$attrition_sex_selectorp != "", "Please select sex group"))
    validate(need(input$attrition_age_selectorp != "", "Please select age group"))
    
    table <- prevalence_attrition %>% 
      filter(cdm_name %in% input$attrition_database_name_selectorp) %>% 
      filter(outcome_cohort_name %in% input$attrition_outcome_selectorp) %>% 
      filter(denominator_sex %in% input$attrition_sex_selectorp) %>% 
      filter(denominator_age_group %in% input$attrition_age_selectorp) %>% 
      select(-c(analysis_id,
                denominator_target_cohort_definition_id,
                denominator_target_cohort_name,
                denominator_end_date,
                denominator_start_date,
                outcome_cohort_id,
                analysis_complete_database_intervals,
                denominator_cohort_id,
                analysis_min_cell_count,
                analysis_time_point,
                analysis_full_contribution,
                denominator_cohort_name,
                denominator_days_prior_observation,
                denominator_target_cohort_name,
                analysis_interval,
                analysis_type
      )) %>% 
      rename(`Persons (n)` = number_subjects,
             `Records (n)` = number_records,
             Reason = reason,
             `Reason ID` = reason_id,
             `Excluded Persons (n)` = excluded_subjects,
             `Excluded Records (n)` = excluded_records,
             Age = denominator_age_group,
             `Cohort Name` = outcome_cohort_name, 
             Sex = denominator_sex,
             Database = cdm_name) %>% 
      relocate(`Reason ID`, Reason, .before = `Records (n)`)
    
    table
    
  }) 
  
  output$tbl_prevalence_attrition <- renderText(kable(get_table_attritionp()) %>%
                                                 kable_styling("striped", full_width = F) )
  
  output$dt_prevalence_attrition_word <- downloadHandler(
    filename = function() {
      "prevalence_attrition.docx"
    },
    content = function(file) {
      x <- gt(get_table_attritionp())
      gtsave(x, file)
    }
  )
  
  
  #patient_demographics ----
  get_demo_characteristics <- reactive({

    validate(
      need(input$demographics_cohort_selector != "", "Please select a cohort")
    )

    validate(
      need(input$demographics_sex_selector != "", "Please select a sex")
    )
    
    validate(
      need(input$demographics_age_selector != "", "Please select an age group")
    )
    
    validate(
      need(input$demographics_database_name_selector != "", "Please select a database")
    )

    demo_characteristics <- demo_characteristics %>%
      filter(sex %in% input$demographics_sex_selector) %>% 
      filter(age_group %in% input$demographics_age_selector) %>% 
      filter(group_level %in% input$demographics_cohort_selector) %>% 
      filter(cdm_name %in% input$demographics_database_name_selector) %>% 
      select(-c(sex, age_group))
    

    demo_characteristics
    
    
  })


  output$gt_demo_characteristics  <- render_gt({
    PatientProfiles::tableCharacteristics(get_demo_characteristics(),
                                          header = c("group", "cdm_name", "strata"))
  })


  output$gt_demo_characteristics_word <- downloadHandler(
    filename = function() {
      "demographics_characteristics.docx"
    },
    content = function(file) {
      gtsave(PatientProfiles::tableCharacteristics(get_demo_characteristics(),
                                                   header = c("group", "cdm_name", "strata")), file)
    }
  )

 
  #comorbidities_demographics ----
  get_comorb_characteristics <- reactive({
    
    validate(
      need(input$comorb_cohort_selector != "", "Please select a cohort"))
    
    validate(
      need(input$comorb_sex_selector != "", "Please select a sex")
    )
    
    validate(
      need(input$comorb_age_selector != "", "Please select an age group")
    )
    
    validate(
      need(input$comorb_time_selector != "", "Please select a demographic time period")
    )
    
    validate(
      need(input$comorb_database_name_selector != "", "Please select a database")
    )
    
    comorb_characteristics <- comorb_characteristics %>%
      filter(sex %in% input$comorb_sex_selector) %>%
      filter(age_group %in% input$comorb_age_selector) %>%
      filter(group_level %in% input$comorb_cohort_selector) %>% 
      filter(window %in% input$comorb_time_selector) %>% 
      filter(cdm_name %in% input$comorb_database_name_selector) %>% 
      select(-c(sex, age_group, table, window, value))

    
    comorb_characteristics
    
  })
  
  
  output$gt_comorb_characteristics  <- render_gt({
    PatientProfiles::tableCharacteristics(get_comorb_characteristics(),
                                        
                                          header = c("group", "cdm_name", "strata", "Window"),
                                          split = c("group", "strata", "additional"),
                                          excludeColumns = c("result_id", "estimate_type",
                                                             "result_type", "value","table",
                                                             "package_name", "package_version",
                                                             "variable_name")
                                          )
  })
  
  
  output$gt_comorb_characteristics_word <- downloadHandler(
    filename = function() {
      "comorbidities_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(PatientProfiles::tableCharacteristics(get_comorb_characteristics(),
                                                       
                                                       header = c("group", "cdm_name", "strata", "Window"),
                                                       split = c("group", "strata", "additional"),
                                                       excludeColumns = c("result_id", "estimate_type",
                                                                          "result_type", "value","table",
                                                                          "package_name", "package_version",
                                                                          "variable_name")
      ), file)
    }
  )
  
  
  
  #medications_demographics ----
  get_med_characteristics <- reactive({
    
    validate(
      need(input$med_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$med_sex_selector != "", "Please select a sex")
    )
    
    validate(
      need(input$med_age_selector != "", "Please select an age group")
    )
    
    validate(
      need(input$med_time_selector != "", "Please select a demographic time period")
    )
    
    validate(
      need(input$med_database_name_selector != "", "Please select a database")
    )
    
    med_characteristics <- med_characteristics %>%
      filter(sex %in% input$med_sex_selector) %>%
      filter(age_group %in% input$med_age_selector) %>%
      filter(group_level %in% input$med_cohort_selector) %>% 
      filter(window %in% input$med_time_selector) %>% 
      filter(cdm_name %in% input$med_database_name_selector) %>% 
      select(-c(sex, age_group, table, window, value))
    
    med_characteristics
    
  })
  
  
  output$gt_med_characteristics  <- render_gt({
    PatientProfiles::tableCharacteristics(get_med_characteristics(),
                                          header = c("group", "cdm_name", "strata", "Window"),
                                          split = c("group", "strata", "additional"),
                                          excludeColumns = c("result_id", "estimate_type",
                                                              "result_type", "value","table",
                                                             "package_name", "package_version",
                                                             "variable_name")
                                          )
  })
  
  
  output$gt_med_characteristics_word <- downloadHandler(
    filename = function() {
      "medications_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(    PatientProfiles::tableCharacteristics(get_med_characteristics(),
                                                       header = c("group", "cdm_name", "strata", "Window"),
                                                       split = c("group", "strata", "additional"),
                                                       excludeColumns = c("result_id", "estimate_type",
                                                                          "result_type", "value","table",
                                                                          "package_name", "package_version",
                                                                          "variable_name")
      ), file)
    }
  )
  
  
   

  #surv risk table --------
  get_risk_table <- reactive({


    validate(
      need(input$risk_table_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$risk_table_database_name_selector != "", "Please select a database")
    )


    table <- survival_risk_table %>%
      filter(outcome_cohort_name %in% input$risk_table_cohort_name_selector) %>%
      filter(cdm_name %in% input$risk_table_database_name_selector)

    table

  })


  output$dt_risk_table <- renderText(kable(get_risk_table()) %>%
                                       kable_styling("striped", full_width = F) )


  output$gt_risk_table_word <- downloadHandler(
    filename = function() {
      "risk_table.docx"
    },
    content = function(file) {
      x <- gt(get_risk_table())
      gtsave(x, file)
    }
  )
  
  
  # surv stats --------
  # get_surv_stats_table <- reactive({
  # 
  # 
  #   validate(
  #     need(input$median_cohort_name_selector != "", "Please select a cohort")
  #   )
  #   validate(
  #     need(input$median_database_name_selector != "", "Please select a database")
  #   )
  # 
  #   validate(
  #     need(input$median_demo_selector != "", "Please select a demographic")
  #   )
  # 
  # 
  #   table <- survival_median_table %>%
  #     filter(group_level %in% input$median_cohort_name_selector) %>%
  #     filter(cdm_name %in% input$median_database_name_selector) %>%
  #     filter(strata_level %in% input$median_demo_selector)
  # 
  #   table
  # 
  # })
  # 
  # output$dt_surv_stat <- renderText(kable(get_surv_stats_table()) %>%
  #                                         kable_styling("striped", full_width = F) )
  # 
  # 
  # output$dt_surv_stat_word <- downloadHandler(
  #   filename = function() {
  #     "summary_survival_statistics.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(get_surv_stats_table())
  #     gtsave(x, file)
  #   }
  # )


  # incidence stats -------- 
  get_inc_estimates_table <- reactive({
    
    
    validate(
      need(input$inc_estimates_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$inc_est_database_selector != "", "Please select a database")
    )

    validate(
      need(input$inc_est_analysis_selector != "", "Please select analysis interval")
    )
    
    validate(
      need(input$inc_est_sex_selector != "", "Please select sex group")
    )
    
    validate(
      need(input$inc_est_age_selector != "", "Please select age group")
    )
    
    
    table <- incidence_estimates %>%
      filter(outcome_cohort_name %in% input$inc_estimates_cohort_selector) %>%
      filter(analysis_interval %in% input$inc_est_analysis_selector) %>% 
      filter(denominator_sex %in% input$inc_est_sex_selector) %>% 
      filter(denominator_age_group %in% input$inc_est_age_selector) %>% 
      filter(cdm_name %in% input$inc_est_database_selector) %>% 
      relocate(outcome_cohort_name) %>% 
      mutate(incidence_100000_pys=nice.num2(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num2(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num2(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, ")"))) %>% 
      select(-c(outcome_cohort_id,
                incidence_100000_pys_95CI_lower,
                incidence_100000_pys_95CI_upper,
                analysis_repeated_events,
                analysis_min_cell_count,
                denominator_target_cohort_name,
                denominator_cohort_name,
                analysis_id, 
                person_days,
                denominator_days_prior_observation,   
                denominator_start_date,
                denominator_end_date,
                denominator_target_cohort_definition_id,
                analysis_complete_database_intervals,
                analysis_outcome_washout,
                denominator_cohort_id,
                cohort_obscured,
                result_obscured
                )) %>% 
      rename(`Start Date` = incidence_start_date,
             `End Date` = incidence_end_date,
             `Persons (n)` = n_persons,
             `Person Years`= person_years,
             `Events (n)` = n_events,
             `Incidence (100,000 pys)` = incidence_100000_pys,
             `Time Interval` = analysis_interval,
             Age = denominator_age_group,
             `Cohort Name` = outcome_cohort_name, 
             Sex = denominator_sex,
             Database = cdm_name)
    
    table
    
  })
  
  
  output$dt_inc_est_table <- renderText(kable(get_inc_estimates_table()) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$dt_inc_est_table_word <- downloadHandler(
    filename = function() {
      "incidence_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_inc_estimates_table())
      gtsave(x, file)
    }
  )
  
  
  
  # inc age std stats --------
  get_inc_estimates_table_std <- reactive({
    
    
    validate(
      need(input$inc_estimates_cohort_selector_std != "", "Please select a cohort")
    )
    
    validate(
      need(input$inc_estimates_sex_selector_std != "", "Please select sex group")
    )
    
    
    validate(
      need(input$inc_estimates_database_selector_std != "", "Please select database")
    )
    
    
    
    table <- incidence_estimates_std %>%
      filter(outcome_cohort_name %in% input$inc_estimates_cohort_selector_std) %>%
      filter(denominator_sex %in% input$inc_estimates_sex_selector_std) %>%
      filter(cdm_name %in% input$inc_estimates_database_selector_std) %>%
      relocate(outcome_cohort_name) %>% 
      select(-c(denominator_age_group)) %>% 
      mutate(incidence_100000_pys=nice.num2(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num2(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num2(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, ")"))) %>% 
      select(-c(person_years,
                n_events,
                incidence_100000_pys_95CI_lower,
                incidence_100000_pys_95CI_upper
      )) %>% 
      rename(`Start Date` = incidence_start_date,
             `Incidence (100,000 pys)` = incidence_100000_pys,
             `Population Age Standard` = age_standard,
             `Cohort Name` = outcome_cohort_name, 
             Sex = denominator_sex,
             Database = cdm_name) %>%
      pivot_wider(names_from = `Population Age Standard`, values_from = `Incidence (100,000 pys)`)
   
    
    table
    
  })
  
  
  output$dt_inc_est_table_std <- renderText(kable(get_inc_estimates_table_std()) %>%
                                          kable_styling("striped", full_width = F) )
  
  
  output$dt_inc_est_table_word_std <- downloadHandler(
    filename = function() {
      "std_incidence_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_inc_estimates_table_std())
      gtsave(x, file)
    }
  )
  
  
  
  # prev stats -------- 
  get_prev_estimates_table <- reactive({
    
    
    validate(
      need(input$prev_estimates_cohort_selector != "", "Please select a cohort")
    )
    validate(
      need(input$prev_estimates_cdm_selector != "", "Please select database")
    )
    
    validate(
      need(input$prev_estimates_sex_selector != "", "Please select sex group")
    )
    
    validate(
      need(input$prev_estimates_age_selector != "", "Please select age group")
    )
    
    
    
    table <- prevalence_estimates %>%
      filter(outcome_cohort_name %in% input$prev_estimates_cohort_selector) %>%
      filter(cdm_name %in% input$prev_estimates_cdm_selector) %>% 
      filter(denominator_sex %in% input$prev_estimates_sex_selector) %>% 
      filter(denominator_age_group %in% input$prev_estimates_age_selector) %>% 
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>% 
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>% 
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>% 
      mutate(prevalence = ifelse(!is.na(prevalence),
                                paste0(prevalence, " (",
                                       prevalence_95CI_lower," to ", 
                                       prevalence_95CI_upper, ")"))) %>% 
      
      select(!c(prevalence_95CI_lower, 
                prevalence_95CI_upper,
                result_obscured,
                analysis_min_cell_count,
                outcome_cohort_id,
                population_obscured,
                cases_obscured,
                result_obscured,
                analysis_id,
                analysis_type,
                analysis_interval,
                analysis_complete_database_intervals,
                analysis_time_point,
                analysis_full_contribution,
                analysis_min_cell_count,
                denominator_cohort_id,
                denominator_cohort_name,
                denominator_days_prior_observation,
                denominator_start_date,
                denominator_end_date,
                denominator_target_cohort_definition_id,
                denominator_target_cohort_name
                
                )) %>%
      relocate(prevalence, .before = outcome_cohort_name) %>% 
      rename(`Start Date` = prevalence_start_date,
             `Prevalence (95% CI)` = prevalence,
             `End Date` = prevalence_end_date,
             `Population (n)` = n_population,
             `Cases (n)` = n_cases,
             `Cohort Name` = outcome_cohort_name,
             Age = denominator_age_group,
             Sex = denominator_sex,
             Database = cdm_name)
    
    
    table
    
  })
  
  
  output$dt_prev_est_table <- renderText(kable(get_prev_estimates_table()) %>%
                                          kable_styling("striped", full_width = F) )
  
  
  output$dt_prev_est_table_word <- downloadHandler(
    filename = function() {
      "prevalence_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_prev_estimates_table())
      gtsave(x, file)
    }
  )
  
  
  
  
  
  # survival plots -------
  get_surv_plot <- reactive({
    
    validate(need(input$survival_cohort_name_selector != "", "Please select a cohort") )
    validate(need(input$survival_database_selector != "", "Please select a database"))
    validate(need(input$survival_demo_selector != "", "Please select a demographic"))
    validate(need(input$surv_plot_group != "", "Please select a group to colour by"))
    validate(need(input$surv_plot_facet != "", "Please select a group to facet by")
    )

    
    plot_data <- survival_estimates %>%
      filter(cdm_name %in% input$survival_database_selector) %>%
      filter(group_level %in% input$survival_cohort_name_selector) %>% 
      filter(strata_level %in% input$survival_demo_selector) 
    
    if (input$show_ci) {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                     ymin = estimate_95CI_lower, 
                     ymax = estimate_95CI_upper, 
                     group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          geom_ribbon(aes(ymin = estimate_95CI_lower, ymax = estimate_95CI_upper),
                      alpha = 0.1) +
          geom_line(size = 0.25) +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")+
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            text = element_text(size = 15))
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                     ymin = estimate_95CI_lower, 
                     ymax = estimate_95CI_upper, 
                     group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          geom_ribbon(aes(ymin = estimate_95CI_lower, ymax = estimate_95CI_upper),
                      alpha = 0.1) +
          geom_line(size = 0.25) +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            text = element_text(size = 15))
        
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                     ymin = estimate_95CI_lower, 
                     ymax = estimate_95CI_upper)) +
          scale_y_continuous( labels = label_percent() ) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          geom_ribbon(aes(ymin = estimate_95CI_lower, ymax = estimate_95CI_upper),
                      alpha = 0.1) +
          geom_line(size = 0.25) +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")+
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            text = element_text(size = 15))
        
        
      } else {
          plot <- plot_data %>%
            ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                       ymin = estimate_95CI_lower, 
                       ymax = estimate_95CI_upper, 
                       group = Group, colour = Group, fill = Group)) +
            scale_y_continuous( labels = label_percent() ) +
            xlab("Time (Years)") +
            ylab("Survival Function (%)") +
            geom_ribbon(aes(ymin = estimate_95CI_lower, ymax = estimate_95CI_upper),
                        alpha = 0.1) +
            geom_line(size = 0.25) +
            theme(
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
          
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot  +
        theme(strip.text = element_text(size = 15, face = "bold"))
      
      plot 
      
    } else {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                     ymin = estimate_95CI_lower, 
                     ymax = estimate_95CI_upper, 
                     group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          geom_line(size = 0.25) +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")+
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            text = element_text(size = 15))
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                     ymin = estimate_95CI_lower, 
                     ymax = estimate_95CI_upper, 
                     group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          geom_line(size = 0.25) +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            text = element_text(size = 15))
        
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                     ymin = estimate_95CI_lower, 
                     ymax = estimate_95CI_upper)) +
          scale_y_continuous( labels = label_percent() ) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          geom_line(size = 0.25) +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")+
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            text = element_text(size = 15))
        
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = as.numeric(time)/365.25, y = estimate,
                     ymin = estimate_95CI_lower, 
                     ymax = estimate_95CI_upper, 
                     group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          geom_line(size = 0.25) +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            text = element_text(size = 15))
        
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot  +
        theme(strip.text = element_text(size = 15, face = "bold"))
      
      plot 
      
    }
    
    
    
    
    
  })
  
  output$survivalPlot <- renderPlot(
    get_surv_plot()
  )
  
  output$survival_download_plot <- downloadHandler(
    filename = function() {
      "Survival_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_surv_plot(),
        width = as.numeric(input$survival_download_width),
        height = as.numeric(input$survival_download_height),
        dpi = as.numeric(input$survival_download_dpi),
        units = "cm"
      )
    }
  )
 
  
  
  # surv risk table CY --------
  get_risk_tablecy <- reactive({
    
    
    validate(
      need(input$risk_table_cohort_name_selectorcy != "", "Please select a cohort")
    )
    validate(
      need(input$risk_table_database_name_selectorcy != "", "Please select a database")
    )

    
    table <- survival_risk_cy_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selectorcy) %>%
      filter(Database %in% input$risk_table_database_name_selectorcy) 
    
    table
    
  })
  
  
  output$dt_risk_tablecy <- renderText(kable(get_risk_tablecy()) %>%
                                       kable_styling("striped", full_width = F) )
  
  
  output$gt_risk_tablecy_word <- downloadHandler(
    filename = function() {
      "risk_table_calendar_year.docx"
    },
    content = function(file) {
      x <- gt(get_risk_tablecy())
      gtsave(x, file)
    }
  )  
  
  
  get_incidence_plot <- reactive({
    
    validate(need(input$incidence_cohort_name_selector != "", "Please select a cohort"))
    validate(need(input$incidence_database_selector != "", "Please select a database"))
    validate(need(input$incidence_sex_selector != "", "Please select sex"))
    validate(need(input$incidence_age_selector != "", "Please select age group"))
    validate(need(input$incidence_plot_group != "", "Please select a group to colour by") )
    validate(need(input$incidence_plot_facet != "", "Please select a group to facet by"))
    validate(need(input$incidence_start_date_selector != "", "Please select incidence dates"))
    
    
    plot_data <- incidence_estimates %>%
        # first deselect settings which did not vary for this study
        select(!c(analysis_id,
                  analysis_complete_database_intervals,
                  denominator_start_date,
                  denominator_days_prior_observation,
                  analysis_outcome_washout,
                  denominator_target_cohort_definition_id,
                  analysis_repeated_events,
                  analysis_min_cell_count,
                  denominator_target_cohort_name,
                  cohort_obscured,
                  result_obscured,
                  outcome_cohort_id,
                  denominator_cohort_name,
                  denominator_cohort_id,
                  denominator_end_date)) %>%
      filter(cdm_name %in% input$incidence_database_selector)  %>%
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector)  %>%
      filter(outcome_cohort_name %in% input$incidence_cohort_name_selector)  %>%
      filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector) %>% 
      filter(denominator_sex %in% input$incidence_sex_selector) %>% 
      filter(denominator_age_group %in% input$incidence_age_selector) 
    
    
    if (input$show_error_bars1) {
      
      if (!is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group = "Group",
                            colour = "Group", fill = "Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))

        
      } else if (!is.null(input$incidence_plot_group) && is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group", fill = "Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))
        
      } else if (is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))
        
      } else {
        plot <- plot_data %>%

          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))
        
      }
      
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot + 
        theme(strip.text = element_text(size = 15, face = "bold")
              
        )
      
      plot
      


      } else {
        
        
        if (!is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
            unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x = "incidence_start_date", y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group = "Group", colour = "Group")) +
            geom_point(position = position_dodge(width = 1)) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black", size = 0.6),
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  text = element_text(size = 15)) +
            facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
          
        } else if (!is.null(input$incidence_plot_group) && is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x = "incidence_start_date", y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group = "Group", colour = "Group")) +
            geom_point(position = position_dodge(width = 1)) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black", size = 0.6),
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  text = element_text(size = 15)) 
            
          
        } else if (is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x = "incidence_start_date", y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group = "Group", colour = "Group")) +
            geom_point(position = position_dodge(width = 1)) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black", size = 0.6),
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  text = element_text(size = 15)) +
            facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
            
          
        } else {
          plot <- plot_data %>%
            ggplot(aes_string(x = "incidence_start_date", y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper")) +
            geom_point(position = position_dodge(width = 1)) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black", size = 0.6),
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  text = element_text(size = 15)) +
            facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
          
          
        }
        
        
        # Move scale_y_continuous outside of ggplot
        plot <- plot + 
          theme(strip.text = element_text(size = 15, face = "bold")
                
          )
        
        plot
        
      
        
      }
      
    
  })
  
  output$incidencePlot <- renderPlot(
    get_incidence_plot()
  )
  
  output$incidence_download_plot <- downloadHandler(
    filename = function() {
      "Crude_incidence_estimates_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_incidence_plot(),
        width = as.numeric(input$incidence_download_width),
        height = as.numeric(input$incidence_download_height),
        dpi = as.numeric(input$incidence_download_dpi),
        units = "cm"
      )
    }
  )
  
  
  
  get_prevalence_plot <- reactive({
    
    validate(need(input$prevalence_cohort_name_selector != "", "Please select a cohort"))
    validate(need(input$prevalence_database_selector != "", "Please select a database"))
    validate(need(input$prevalence_sex_selector != "", "Please select sex"))
    validate(need(input$prevalence_age_selector != "", "Please select age group"))
    validate(need(input$prevalence_plot_group != "", "Please select a group to colour by") )
    validate(need(input$prevalence_plot_facet != "", "Please select a group to facet by"))
    validate(need(input$prevalence_start_date_selector != "", "Please select prevalence dates"))
    
    
    plot_data <- prevalence_estimates %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_days_prior_observation,
                denominator_target_cohort_definition_id,
                analysis_min_cell_count,
                denominator_target_cohort_name,
                result_obscured,
                outcome_cohort_id,
                denominator_cohort_name,
                denominator_cohort_id,
                denominator_end_date)) %>%
      filter(cdm_name %in% input$prevalence_database_selector)  %>%
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector)  %>%
      filter(outcome_cohort_name %in% input$prevalence_cohort_name_selector)  %>%
      filter(denominator_sex %in% input$prevalence_sex_selector) %>% 
      filter(denominator_age_group %in% input$prevalence_age_selector) 
    
    
    if (input$show_error_bars) {
      
      if (!is.null(input$prevalence_plot_group) && !is.null(input$prevalence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="prevalence_start_date", y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group = "Group",
                            colour = "Group", fill = "Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          facet_wrap(vars(facet_var),ncol = 3, scales = "free_y") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))
        
        
      } else if (!is.null(input$prevalence_plot_group) && is.null(input$prevalence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="prevalence_start_date", y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group", fill = "Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))
        
      } else if (is.null(input$prevalence_plot_group) && !is.null(input$prevalence_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="prevalence_start_date", y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          facet_wrap(vars(facet_var),ncol = 3, scales = "free_y") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))
        
      } else {
        plot <- plot_data %>%
          
          ggplot(aes_string(x="prevalence_start_date", y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), 
                      alpha = 0.1) + 
          geom_line(size = 0.25) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15))
        
      }
      
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot + 
        theme(strip.text = element_text(size = 15, face = "bold")
              
        )
      
      plot
      
      
      
    } else {
      
      
      if (!is.null(input$prevalence_plot_group) && !is.null(input$prevalence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "prevalence_start_date", y = "prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group = "Group", colour = "Group")) +
          geom_point(position = position_dodge(width = 1)) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          geom_errorbar(width = 0, position = position_dodge(width = 1)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.6),
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15)) +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
        
      } else if (!is.null(input$prevalence_plot_group) && is.null(input$prevalence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "prevalence_start_date", y = "prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group = "Group", colour = "Group")) +
          geom_point(position = position_dodge(width = 1)) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          geom_errorbar(width = 0, position = position_dodge(width = 1)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.6),
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15)) 
        
        
      } else if (is.null(input$prevalence_plot_group) && !is.null(input$prevalence_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "prevalence_start_date", y = "prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group = "Group", colour = "Group")) +
          geom_point(position = position_dodge(width = 1)) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          geom_errorbar(width = 0, position = position_dodge(width = 1)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.6),
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15)) +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
        
        
      } else {
        plot <- plot_data %>%
          ggplot(aes_string(x = "prevalence_start_date", y = "prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position = position_dodge(width = 1)) +
          labs(x = "Calendar Year", y = "Prevalence (%)") +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)) +
          geom_errorbar(width = 0, position = position_dodge(width = 1)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.6),
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                text = element_text(size = 15)) +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
        
        
      }
      
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot + 
        theme(strip.text = element_text(size = 15, face = "bold")
              
        )
      
      plot
      
      
      
    }
    
    
  })
  
  output$prevalencePlot <- renderPlot(
    get_prevalence_plot()
  )
  
  output$prevalence_download_plot <- downloadHandler(
    filename = function() {
      "prevalence_estimates_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_incidence_plot(),
        width = as.numeric(input$prevalence_download_width),
        height = as.numeric(input$prevalence_download_height),
        dpi = as.numeric(input$prevalence_download_dpi),
        units = "cm"
      )
    }
  )
  
  
  
  get_incidence_plot_std <- reactive({
  
    validate(need(input$incidence_cohort_name_selector_std != "", "Please select a cohort"))
    validate(need(input$incidence_database_selector_std != "", "Please select a database"))
    validate(need(input$incidence_sex_selector_std != "", "Please select sex"))
    validate(need(input$incidence_plot_group_std != "", "Please select a group to colour by") )
    validate(need(input$incidence_plot_facet_std != "", "Please select a group to facet by"))
    validate(need(input$incidence_start_date_selector_std != "", "Please select incidence dates"))
    validate(need(input$incidence_std_method != "", "Please select incidence standardization method"))
    
  
  plot_data <- incidence_estimates_std %>%
    filter(outcome_cohort_name %in% input$incidence_cohort_name_selector_std) %>% 
    filter(age_standard %in% input$incidence_std_method) %>% 
    filter(cdm_name %in% input$incidence_database_selector_std)  %>%
    filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector_std)  %>%
    filter(denominator_sex %in% input$incidence_sex_selector_std) 


  
  
  if (input$show_error_bars_std) {
    
    if (!is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        unite("facet_var", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                    alpha = 0.1) + 
        geom_line(size = 0.25) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
      
    } else if (!is.null(input$incidence_plot_group_std) && is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                    alpha = 0.1) + 
        geom_line(size = 0.25) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
    } else if (is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("facet_var", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                    alpha = 0.1) + 
        geom_line(size = 0.25) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
    } else {
      plot <- plot_data %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(position=position_dodge(width=1))+
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                    alpha = 0.1) + 
        geom_line(size = 0.25) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
    }
    
    
    # Move scale_y_continuous outside of ggplot
    plot <- plot + 
      theme(strip.text = element_text(size = 15, face = "bold")
            
      )
    
    plot
    
    
    
  } else {
    
    
    if (!is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        unite("facet_var", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width = 0, position = position_dodge(width = 1)) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
      
    } else if (!is.null(input$incidence_plot_group_std) && is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width = 0, position = position_dodge(width = 1)) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
      
    } else if (is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("facet_var", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width = 0, position = position_dodge(width = 1)) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
      
    } else {
      plot <- plot_data %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width = 0, position = position_dodge(width = 1)) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              text = element_text(size = 15))
      
      
    }
    
    
    # Move scale_y_continuous outside of ggplot
    plot <- plot + 
      theme(strip.text = element_text(size = 15, face = "bold")
            
      )
    
    plot
    
    
    
  }
  
  
})

output$incidencePlot_std <- renderPlot(
  get_incidence_plot_std()
)

output$incidence_download_plot_std <- downloadHandler(
  filename = function() {
    "Std_incidence_estimates_plot.png"
  },
  content = function(file) {
    ggsave(
      file,
      get_incidence_plot_std(),
      width = as.numeric(input$incidence_download_widthstd),
      height = as.numeric(input$incidence_download_heightstd),
      dpi = as.numeric(input$incidence_download_dpistd),
      units = "cm"
    )
  }
)
  

# attrition --------
get_attrition <- reactive({
  
  validate(
    need(input$attrition_cohort_name_selector != "", "Please select a cohort")
  )
  
  validate(
    need(input$attrition_database_name_selector != "", "Please select a database")
  )
  
  table <- survival_attrition %>%
    filter(outcome_cohort_name %in% input$attrition_cohort_name_selector) %>%
    filter(cdm_name %in% input$attrition_database_name_selector) 
  
  table
  
})


get_attrition1 <- reactive({
  
  validate(
    need(input$attrition_cohort_name_selector1 != "", "Please select a cohort")
  )
  
  validate(
    need(input$attrition_database_name_selector1 != "", "Please select a database")
  )
  
  
  table <- survival_attrition %>%
    filter(outcome_cohort_name %in% input$attrition_cohort_name_selector1) %>%
    filter(cdm_name %in% input$attrition_database_name_selector1) 
  
  table
  
})

output$attrition_diagram <- renderGrViz({
  table <- get_attrition1()
  validate(need(nrow(table) > 0, "No results for selected inputs"))
  render_graph(attritionChart(table))
})

output$cohort_attrition_download_figure <- downloadHandler(
  filename = function() {
    paste0(
      "cohort_attrition_", input$attrition_database_name_selector1, "_", 
      input$attrition_cohort_name_selector1, ".png"
    )
  },
  content = function(file) {
    table <- get_attrition1()
    export_graph(
      graph = attritionChart(table),
      file_name = file,
      file_type = "png",
      width = input$attrition_download_width |> as.numeric()
    )
  }
)


output$dt_attrition <- renderText(kable(get_attrition()) %>%
                                    kable_styling("striped", full_width = F) )


output$gt_attrition_word <- downloadHandler(
  filename = function() {
    "cohort_attrition.docx"
  },
  content = function(file) {
    x <- gt(get_attrition())
    gtsave(x, file)
  }
)

  



# clinical codelists ----------------
get_surv_summary <- reactive({
  
  validate(need(input$median_cohort_name_selector != "", "Please select a cohort"))
  validate(need(input$median_database_name_selector != "", "Please select a database"))
  validate(need(input$median_demo_selector != "", "Please select a database"))
  
#  tryCatch({
    
  table <- survival_median_table %>%
    filter(group_level %in% input$median_cohort_name_selector) %>%
    filter(cdm_name %in% input$median_database_name_selector) %>% 
    filter(strata_level %in% input$median_demo_selector) 
  
  table
  
  # Return the filtered table
#   return(table)
# }, error = function(e) {
#   # If an error occurs (e.g., due to missing data), return NULL
#   return(NULL)
# })

  
})


output$dt_surv_stats <- renderText(kable(get_surv_summary()) %>%
                                     kable_styling("striped", full_width = F) )


output$dt_surv_stat_word <- downloadHandler(
  filename = function() {
    "survival_summary.docx"
  },
  content = function(file) {
    x <- gt(get_surv_summary())
    gtsave(x, file)
  }
)









   
}