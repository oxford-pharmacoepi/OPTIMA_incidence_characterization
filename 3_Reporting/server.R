#### SERVER ------
server <-	function(input, output, session) {

  
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
    
    table <- incidence_attrition %>% 
      filter(outcome_cohort_name %in% input$attrition_cohort_name_selector) %>% 
      filter(analysis_interval %in% input$attrition_time_selector)
    
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
  
  # clinical codelists ----------------
  get_codelists <- reactive({
    
    validate(
      need(input$codelist_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$codelist_vocab_selector != "", "Please select a vocab")
    )
    
    table <- concepts_lists %>%
      filter(Vocabulary %in% input$codelist_vocab_selector) %>%
      filter(Cancer %in% input$codelist_cohort_selector)
    
    table
    
  })
  
  
  output$tbl_codelists <- renderText(kable(get_codelists()) %>%
                                       kable_styling("striped", full_width = F) )
  
  
  output$gt_codelists_word <- downloadHandler(
    filename = function() {
      "concept_lists.docx"
    },
    content = function(file) {
      x <- gt(get_codelists())
      gtsave(x, file)
    }
  )
  
  
  #patient_demographics ----
  get_demo_characteristics <- reactive({

    validate(
      need(input$demographics_cohort_selector != "", "Please select a cohort")
    )

    validate(
      need(input$demographics_selector != "", "Please select a demographic")
    )

    demo_characteristics <- demo_characteristics %>%
      filter(strata_level %in% input$demographics_selector) %>%
      filter(group_level %in% input$demographics_cohort_selector)


    demo_characteristics
  })


  output$gt_demo_characteristics  <- render_gt({
    PatientProfiles::formatCharacteristics(get_demo_characteristics())
  })


  output$gt_demo_characteristics_word <- downloadHandler(
    filename = function() {
      "demographics_characteristics.docx"
    },
    content = function(file) {

      gtsave(PatientProfiles::formatCharacteristics(get_demo_characteristics()), file)
    }
  )

 
  #comorbidities_demographics ----
  get_comorb_characteristics <- reactive({
    
    validate(
      need(input$comorb_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$comorb_selector != "", "Please select a demographic")
    )
    
    comorb_characteristics <- comorb_characteristics %>%
      filter(strata_level %in% input$comorb_selector) %>%
      filter(group_level %in% input$comorb_cohort_selector)
    
    
    comorb_characteristics
  })
  
  
  output$gt_comorb_characteristics  <- render_gt({
    PatientProfiles::formatCharacteristics(get_comorb_characteristics())
  })
  
  
  output$gt_comorb_characteristics_word <- downloadHandler(
    filename = function() {
      "comorbidities_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(PatientProfiles::formatCharacteristics(get_comorb_characteristics()), file)
    }
  )
  
  
  
  #medications_demographics ----
  get_med_characteristics <- reactive({
    
    validate(
      need(input$med_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$med_selector != "", "Please select a demographic")
    )
    
    med_characteristics <- med_characteristics %>%
      filter(strata_level %in% input$med_selector) %>%
      filter(group_level %in% input$med_cohort_selector)
    
    
    med_characteristics
  })
  
  
  output$gt_med_characteristics  <- render_gt({
    PatientProfiles::formatCharacteristics(get_med_characteristics())
  })
  
  
  output$gt_med_characteristics_word <- downloadHandler(
    filename = function() {
      "medications_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(PatientProfiles::formatCharacteristics(get_med_characteristics()), file)
    }
  )
  
  
   

  # surv risk table --------
  # get_risk_table <- reactive({
  #   
  #   
  #   validate(
  #     need(input$risk_table_cohort_name_selector != "", "Please select a cohort")
  #   )
  #   validate(
  #     need(input$risk_table_database_name_selector != "", "Please select a database")
  #   )
  #   
  # 
  #   table <- survival_risk_table %>%
  #     filter(outcome_cohort_name %in% input$risk_table_cohort_name_selector) %>%
  #     filter(cdm_name %in% input$risk_table_database_name_selector) 
  #   
  #   table
  #   
  # })
  # 
  # 
  # output$dt_risk_table <- renderText(kable(get_risk_table()) %>%
  #                                      kable_styling("striped", full_width = F) )
  # 
  # 
  # output$gt_risk_table_word <- downloadHandler(
  #   filename = function() {
  #     "risk_table.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(get_risk_table())
  #     gtsave(x, file)
  #   }
  # )  
  
  
  # surv stats --------
  get_surv_stats_table <- reactive({


    validate(
      need(input$median_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$median_database_name_selector != "", "Please select a database")
    )


    table <- survival_median_table %>%
      filter(group_level %in% input$median_cohort_name_selector) %>%
      filter(cdm_name %in% input$median_database_name_selector)

    table

  })



  output$gt_surv_stats  <- render_gt({
    PatientProfiles::formatCharacteristics(get_surv_stats_table())
  })
  
  
  output$gt_surv_stat_word <- downloadHandler(
    filename = function() {
      "summary_survival_statistics.docx"
    },
    content = function(file) {
      
      gtsave(PatientProfiles::formatCharacteristics(get_surv_stats_table()), file)
    }
  )
  
  
  
  get_inc_estimates_table <- reactive({
    
    
    validate(
      need(input$inc_estimates_cohort_selector != "", "Please select a cohort")
    )
    validate(
      need(input$inc_est_analysis_selector != "", "Please select analysis interval")
    )
    
    
    table <- incidence_estimates %>%
      filter(outcome_cohort_name %in% input$inc_estimates_cohort_selector) %>%
      filter(analysis_interval %in% input$inc_est_analysis_selector) %>% 
      relocate(outcome_cohort_name) %>% 
      select(-c(analysis_id,
                outcome_cohort_id,
                analysis_repeated_events,
                analysis_min_cell_count,
                denominator_target_cohort_name,
                denominator_cohort_name,
                denominator_days_prior_observation,   
                denominator_start_date,
                denominator_end_date,
                denominator_target_cohort_definition_id,
                analysis_complete_database_intervals,
                analysis_outcome_washout,
                denominator_cohort_id,
                analysis_interval,
                cohort_obscured,
                result_obscured
                
                ))
    
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
  
  get_inc_estimates_table_std <- reactive({
    
    
    validate(
      need(input$inc_estimates_cohort_selector_std != "", "Please select a cohort")
    )
    
    table <- incidence_estimates_std %>%
      filter(outcome_cohort_name %in% input$inc_estimates_cohort_selector_std) %>%
      relocate(outcome_cohort_name) 
   
    
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
  
  
  # surv stats CY --------
  get_surv_stats_tablecy <- reactive({
    
    
    validate(
      need(input$median_cohort_name_selectorcy != "", "Please select a cohort")
    )
    validate(
      need(input$median_database_name_selectorcy != "", "Please select a database")
    )
    
    
    table <- survival_median_table %>%
      filter(strata_name == diag_yr_gp) %>% 
      filter(group_level %in% input$median_cohort_name_selectorcy) %>%
      filter(cdm_name %in% input$median_database_name_selectorcy) 
    
    table
    
  })
  
  
  output$dt_median_tablecy <- renderText(kable(get_surv_stats_tablecy()) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$gt_median_tablecy_word <- downloadHandler(
    filename = function() {
      "survival_statistics.docx"
    },
    content = function(file) {
      x <- gt(get_surv_stats_tablecy())
      gtsave(x, file)
    }
  )
  
  
  # survival plots CY -------
  get_surv_plot_cy <- reactive({

    validate(
      need(input$survival_cohort_name_selectorcy != "", "Please select a cohort")
    )
    validate(
      need(input$survival_database_selectorcy != "", "Please select a database")
    )
    
    validate(
      need(input$surv_plot_facetcy != "", "Please select a group to colour by")
    )
    
    validate(
      need(input$surv_plot_facetcy != "", "Please select a group to facet by")
    )


    plot_data <- survival_estimates %>%
      filter(strata_name == diag_yr_gp) %>% 
      filter(cdm_name %in% input$survival_database_selectorcy) %>%
      filter(group_level %in% input$survival_cohort_name_selectorcy) %>% 
      filter(estimate_name == "estimate")

    if (input$show_ci_cy) {

      if (!is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = nrow(plot_data),
                     y = estimate_value,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          geom_ribbon(aes(ymin = lcl, 
                          ymax = ucl, 
                          fill = strata_level), alpha = .15, color = NA, show.legend = FALSE) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = strata_level),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ group_level, scales = "free_y", ncol = 3)



      } else if (!is.null(input$surv_plot_groupcy) && is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = nrow(plot_data),
                     y = estimate_value,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          geom_ribbon(aes(ymin = lcl, 
                          ymax = ucl, 
                          fill = strata_level), alpha = .15, color = NA, show.legend = FALSE) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = strata_level),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ strata_level, scales = "free_y", ncol = 3)

      } else if (is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = nrow(plot_data),
                     y = estimate_value,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          geom_ribbon(aes(ymin = lcl, 
                          ymax = ucl, 
                          fill = strata_level), alpha = .15, color = NA, show.legend = FALSE) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) 

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time,
                     y = est,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          geom_ribbon(aes(ymin = lcl, 
                          ymax = ucl, 
                          fill = strata_level), alpha = .15, color = NA, show.legend = FALSE) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = strata_level),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)

      }

      # Move scale_y_continuous outside of ggplot
      plot <- plot  +
        theme(strip.text = element_text(size = 15, face = "bold"))

      plot

    } else {

      if (!is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = strata_level),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)



      } else if (!is.null(input$surv_plot_groupcy) && is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = strata_level),size = 0.85) +
          xlim(0, 2.5) 

      } else if (is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = strata_level),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time,
                     y = est,
                     group = strata_level,
                     col = strata_level )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = strata_level),size = 0.85) +
          xlim(0, 2.5) 

      }

      # Move scale_y_continuous outside of ggplot
      plot <- plot + 
        theme(strip.text = element_text(size = 15, face = "bold")

      )

      plot


    }





  })

  output$survivalPlotcy <- renderPlot(
    get_surv_plot_cy()
  )

  output$survival_cy_download_plot <- downloadHandler(
    filename = function() {
      "Survival_calendar_time_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_surv_plot_cy(),
        width = as.numeric(input$survival_download_widthcy),
        height = as.numeric(input$survival_download_heightcy),
        dpi = as.numeric(input$survival_download_dpicy),
        units = "cm"
      )
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
    
    
    if (input$show_error_bars) {
      
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

  
   
}