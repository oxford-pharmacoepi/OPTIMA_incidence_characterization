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
    
    # validate(need(input$attrition_outcome_selector != "", "Please select an outcome"))
    # validate(need(input$attrition_database_name_selector != "", "Please select a database"))
    # validate(need(input$attrition_sex_selector != "", "Please select sex group"))
    # validate(need(input$attrition_age_selector != "", "Please select age group"))
    # validate(need(input$attrition_time_selector != "", "Please select time period"))
    
    
    
    table <- incidence_attrition %>% 
      filter(Variable_level %in% input$attrition_outcome_selector) %>% 
      filter(CDM_name %in% input$attrition_database_name_selector) %>% 
      filter(Denominator_sex %in% input$attrition_sex_selector) %>%
      filter(Denominator_age_group %in% input$attrition_age_selector) %>%
      filter(Analysis_interval %in% input$attrition_time_selector)  %>%
      select(-c(Denominator_cohort_name,
                Result_type,
                Package_name,
                Package_version,
                Analysis_outcome_washout,
                Analysis_complete_database_intervals,
                Analysis_repeated_events,
                Denominator_days_prior_observation,
                Denominator_target_cohort_name,
                Min_cell_count,
                Outcome_cohort_name,
                Denominator_time_at_risk,
                Denominator_start_date,
                Denominator_end_date
      )) %>% 
    relocate(Reason_id , Reason, Number_records, Number_subjects, Excluded_records, Excluded_subjects, .before = 1)
    
    table
    
  }) 
  
  output$tbl_incidence_attrition <- DT::renderDataTable({
    DT::datatable(get_table_attrition(), 
                  options = list(
                    pageLength = 25,   # Set the default number of rows to display
                    scrollX = TRUE
                  ),
                  rownames = FALSE     # Remove row numbers
    )
  })
  
  
  output$dt_incidence_attrition_word <- downloadHandler(
    filename = function() {
      "incidence_attrition.docx"
    },
    content = function(file) {
      x <- gt(get_table_attrition())
      gtsave(x, file)
    }
  )

 
  
  #concepts_sets ----
  get_concepts_sets <- reactive({
    
    validate(
      need(input$cohort_set_input != "", "Please select a cohort")
    )
    
  concept_sets_final <- concept_sets_final %>% 
    filter(name %in% input$cohort_set_input) 
    
  concept_sets_final
  
  })
  
  
  output$tbl_concept_sets <- DT::renderDataTable({
    DT::datatable(get_concepts_sets(), 
                  options = list(scrollX = TRUE))
  })
  
  
  output$dt_concept_sets_word <- downloadHandler(
    filename = function() {
      "concept_sets.docx"
    },
    content = function(file) {
      x <- gt(get_concepts_sets())
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
      visOmopResults::splitStrata() %>%
      visOmopResults::splitGroup() %>%
      filter(sex %in% input$demographics_sex_selector) %>% 
      filter(age_group %in% input$demographics_age_selector) %>% 
      filter(cohort_name %in% input$demographics_cohort_selector) %>% 
      filter(cdm_name %in% input$demographics_database_name_selector) %>% 
      visOmopResults::uniteGroup("cohort_name") %>% 
      visOmopResults::uniteStrata(c("sex", "age_group")) 
    

    demo_characteristics
    
    
  })


  output$gt_demo_characteristics  <- render_gt({
    CohortCharacteristics::tableCharacteristics(get_demo_characteristics(),
                                          header = c("group", "cdm_name", "strata"))
  })


  output$gt_demo_characteristics_word <- downloadHandler(
    filename = function() {
      "demographics_characteristics.docx"
    },
    content = function(file) {
      gtsave(CohortCharacteristics::tableCharacteristics(get_demo_characteristics(),
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
      visOmopResults::splitAll() %>% 
      filter(sex %in% input$comorb_sex_selector) %>%
      filter(age_group %in% input$comorb_age_selector) %>%
      filter(cohort_name %in% input$comorb_cohort_selector) %>% 
      filter(window %in% input$comorb_time_selector) %>% 
      filter(cdm_name %in% input$comorb_database_name_selector) %>% 
      visOmopResults::uniteAdditional(c("table", "window", "value")) %>% 
      visOmopResults::uniteGroup("cohort_name") %>% 
      visOmopResults::uniteStrata(c("sex", "age_group")) 

    
    comorb_characteristics
    
  })
  
  
  output$gt_comorb_characteristics  <- render_gt({
    CohortCharacteristics::tableCharacteristics(get_comorb_characteristics(),
                                        
                                          header = c("group", "cdm_name", "strata", "Window"),
                                          hide = c("result_id", "estimate_type",
                                                             "value","table", "variable_name")
                                          )
  })
  
  
  output$gt_comorb_characteristics_word <- downloadHandler(
    filename = function() {
      "comorbidities_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(CohortCharacteristics::tableCharacteristics(get_comorb_characteristics(),
                                                       
                                                       header = c("group", "cdm_name", "strata", "Window"),
                                                       hide = c("result_id", "estimate_type",
                                                                "value","table", "variable_name")
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
      visOmopResults::splitAll() %>% 
      filter(sex %in% input$med_sex_selector) %>%
      filter(age_group %in% input$med_age_selector) %>%
      filter(cohort_name %in% input$med_cohort_selector) %>% 
      filter(window %in% input$med_time_selector) %>% 
      filter(cdm_name %in% input$med_database_name_selector) %>% 
      visOmopResults::uniteAdditional(c("table", "window", "value")) %>% 
      visOmopResults::uniteGroup("cohort_name") %>% 
      visOmopResults::uniteStrata(c("sex", "age_group")) 
    
    med_characteristics
    
  })
  
  
  output$gt_med_characteristics  <- render_gt({
    CohortCharacteristics::tableCharacteristics(get_med_characteristics(),
                                          header = c("group", "cdm_name", "strata", "Window"),
                                         
                                          hide = c("result_id", "estimate_type",
                                                              "value","table", "variable_name")
                                          )
  })
  
  
  output$gt_med_characteristics_word <- downloadHandler(
    filename = function() {
      "medications_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(CohortCharacteristics::tableCharacteristics(get_med_characteristics(),
                                                       header = c("group", "cdm_name", "strata", "Window"),
                                                       hide = c("result_id", "estimate_type",
                                                                          "value","table", "variable_name")
      ), file)
    }
  )
  
  
   

 

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
      filter(result_type == "incidence") %>% 
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
      select(-c(result_id,
                group_name,
                group_level,
                strata_level,
                strata_name,
                variable_name,
                result_type,
                count,
                package_name,
                package_version,
                incidence_100000_pys_95CI_lower,
                incidence_100000_pys_95CI_upper,
                analysis_repeated_events,
                min_cell_count,
                denominator_target_cohort_name,
                person_days,
                denominator_days_prior_observation,   
                denominator_start_date,
                denominator_end_date,
                analysis_complete_database_intervals,
                analysis_outcome_washout
                
                
                )) %>% 
      rename(`Start Date` = incidence_start_date,
             `End Date` = incidence_end_date,
             `Persons (n)` = denominator_count,
             `Person Years`= person_years,
             `Events (n)` = outcome_count,
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
                outcome_count,
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
  
  
  


# incidence plot ------  
  
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
        select(!c(analysis_complete_database_intervals,
                  denominator_start_date,
                  denominator_days_prior_observation,
                  analysis_outcome_washout,
                  analysis_repeated_events)) %>%
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
          geom_line(size = 0.5, colour = "black") +
          geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.1,colour = NA) + 

          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                strip.text.x = element_text(face = "bold", size = 30),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 30))

        
      } else if (!is.null(input$incidence_plot_group) && is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group", fill = "Group")) +
          geom_point(shape = 21, position = position_dodge(width = 1)) +
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
                strip.text.x = element_text(face = "bold", size = 30),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 30))
        
      } else if (is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(shape = 21, position = position_dodge(width = 1)) +
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
                strip.text.x = element_text(face = "bold", size = 30),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 30))
        
      } else {
        plot <- plot_data %>%

          ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(shape = 21, position = position_dodge(width = 1)) +
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
                strip.text.x = element_text(face = "bold", size = 30),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 30))
        
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
                              colour = "Group", fill = "Group")) +
            geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            theme(axis.text.x = element_text(angle = 45, hjust=1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6) ,
                  panel.background = element_blank() ,
                  axis.line = element_line(colour = "black", size = 0.6) ,
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  strip.text.x = element_text(face = "bold", size = 30),
                  legend.key = element_rect(fill = "white"),
                  text = element_text(size = 30)) +
            facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
          
        } else if (!is.null(input$incidence_plot_group) && is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x = "incidence_start_date", y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group = "Group", colour = "Group")) +
            geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust=1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6) ,
                  panel.background = element_blank() ,
                  axis.line = element_line(colour = "black", size = 0.6) ,
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  strip.text.x = element_text(face = "bold", size = 30),
                  legend.key = element_rect(fill = "white"),
                  text = element_text(size = 30))
            
          
        } else if (is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x = "incidence_start_date", y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group = "Group", colour = "Group")) +
            geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust=1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6) ,
                  panel.background = element_blank() ,
                  axis.line = element_line(colour = "black", size = 0.6) ,
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  strip.text.x = element_text(face = "bold", size = 30),
                  legend.key = element_rect(fill = "white"),
                  text = element_text(size = 30)) +
            facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
            
          
        } else {
          plot <- plot_data %>%
            ggplot(aes_string(x = "incidence_start_date", y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper")) +
            geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust=1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6) ,
                  panel.background = element_blank() ,
                  axis.line = element_line(colour = "black", size = 0.6) ,
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  strip.text.x = element_text(face = "bold", size = 30),
                  legend.key = element_rect(fill = "white"),
                  text = element_text(size = 30)) +
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
        
        geom_line(size = 0.5, colour = "black") +
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                    alpha = 0.1, colour = NA) + 
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        facet_wrap(vars(facet_var),ncol = 3)+
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
      
    } else if (!is.null(input$incidence_plot_group_std) && is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
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
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
    } else if (is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("facet_var", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
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
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
    } else {
      plot <- plot_data %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
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
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
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
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
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
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
      
    } else if (!is.null(input$incidence_plot_group_std) && is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group = "Group",
                          colour = "Group", fill = "Group")) +
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
        geom_errorbar(width = 0, position = position_dodge(width = 1)) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
      
    } else if (is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("facet_var", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
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
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
      
    } else {
      plot <- plot_data %>%
        ggplot(aes_string(x="incidence_start_date", y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(shape = 21, colour = "black", position=position_dodge(width=1), size = 7) +
        geom_errorbar(width = 0, position = position_dodge(width = 1)) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              strip.text.x = element_text(face = "bold", size = 30),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 30))
      
      
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
  




   
}