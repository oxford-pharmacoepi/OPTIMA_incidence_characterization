server <- function(input, output, session) { 
  # Markdown ----
  output$markdown <- renderUI({
    
    table <- cohort_set %>% 
      filter(cohort_name %in% input$cohort_set_input) %>% 
      pull(markdown) %>% 
      formatMarkdown()
  })
  # JSON ----
  # output$verb <- renderPrint({
  #   
  #   json_content <- cohort_set %>% 
  #     filter(cohort_name %in% input$cohort_set_input) %>%
  #     pull(json) %>%
  #     unlist()
  #   
  #   cat(json_content)
  #   
  # })

  
  output$downloadLungCancerjson <- downloadHandler(
    filename = function() {
      "all_lung_cancer_end.json"
    },
    content = function(file) {
      file.copy("www/Cohorts/all_lung_cancer_end.json", file)
    }
  )
  
  
  output$downloadLungCancerbroadjson <- downloadHandler(
    filename = function() {
      "broad_lung_cancer_end.json"
    },
    content = function(file) {
      file.copy("www/Cohorts/broad_lung_cancer_end.json", file)
    }
  )
  
  output$downloadLungCancernarrowjson <- downloadHandler(
    filename = function() {
      "narrow_lung_cancer_end.json"
    },
    content = function(file) {
      file.copy("www/Cohorts/narrow_lung_cancer_end.json", file)
    }
  )
  
  output$downloadscLungCancerjson <- downloadHandler(
    filename = function() {
      "small_cell_lung_cancer.json"
    },
    content = function(file) {
      file.copy("www/Cohorts/small_cell_lung_cancer.json", file)
    }
  )

  
  output$downloadSmallCell <- downloadHandler(
    filename = function() {
      "small_cell_lung_cancer.docx"
    },
    content = function(file) {
      file.copy("www/small_cell_lung_cancer.docx", file)
    }
  )
  
  output$downloadLungCancer <- downloadHandler(
    filename = function() {
      "lung_cancer.docx"
    },
    content = function(file) {
      file.copy("www/lung_cancer.docx", file)
    }
  )

  
  #concepts_sets ----
  get_concepts_sets <- reactive({
    
    validate(
      need(input$cohort_set_input != "", "Please select a cohort")
    )
    
    table <- concept_sets_final %>% 
      filter(name %in% input$cohort_set_input) 
    
    table
    
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
  
  # Cohort counts ----
  output$tidy_counts <- renderDataTable({
    datatable(
      filterData(data$cohort_count, "counts", input) %>% 
        select(cdm_name, cohort_name, number_records, number_subjects) %>% 
        niceColumnNames(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  # Code counts ----
  getCodeCount <- reactive({
    filterData(data$code_counts, "code_counts", input) %>% 
      niceColumnNames() %>% 
      select(input$select_code_count_columns)
  })
  output$tidy_code_counts <- renderDataTable({
    datatable(
      getCodeCount(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  
  
  # snapshot
  output$cdm_snapshot_tidy <- renderDataTable({
    datatable(data$cdm_snapshot,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "cdm_snapshot"))
              ))
  })
  
  
  # Orphan counts ----
  getOrphanCount <- reactive({
    filterData(data$orphan_counts, "orphan", input) %>% 
      niceColumnNames() %>% 
      select(input$select_orphan_count_columns)
  })
  output$tidy_orphan_counts <- renderDataTable({
    datatable(
      getOrphanCount(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  # Index date ----
  getIndexDate <- reactive({
    filterData(data$index_events, "index", input) %>% 
      niceColumnNames() %>% 
      select(input$select_index_columns)
  })
  output$index_date_tidy <- renderDataTable({
    datatable(
      getIndexDate(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  # Overlap ----

  getOverlapTable <- reactive({
    
    validate(
      need(data$cohort_overlap, "No cohort overlap carried out", "No cohort overlap carried out")
    )
    
    filterData(data$cohort_overlap, "overlap", input) %>% 
      mutate(
        total_counts = subject_counts_x + subject_counts_y,
        intersect_percentage = intersect_counts*2/total_counts * 100,
        subject_percentage_x = (subject_counts_x-intersect_counts)/total_counts * 100,
        subject_percentage_y = (subject_counts_y-intersect_counts)/total_counts * 100,
        subject_counts_only_in_x = paste0(niceNum(subject_counts_x-intersect_counts), " (", niceNum(subject_percentage_x), "%)"),
        subject_counts_only_in_y = paste0(niceNum(subject_counts_y-intersect_counts), " (", niceNum(subject_percentage_y), "%)"),
        intersect_counts = paste0(niceNum(intersect_counts), " (", niceNum(intersect_percentage), "%)")
      ) %>% 
      select(cdm_name, cohort_name_x, cohort_name_y, subject_counts_only_in_x, subject_counts_only_in_y, intersect_counts) %>% 
      niceColumnNames()
  })
  output$overlap_tidy <- renderDataTable({
    datatable(
      getOverlapTable(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  getOverlapPlot <- reactive({
    filterData(data$cohort_overlap, "overlap", input) %>% 
      mutate(
        total_counts = subject_counts_x + subject_counts_y - intersect_counts,
        intersect_percentage = intersect_counts/total_counts * 100,
        subject_percentage_x = subject_counts_x/total_counts * 100,
        subject_percentage_y = subject_counts_y/total_counts * 100,
        total_percentage = 100,
        comparison_name = paste0(cohort_name_x, "; ", cohort_name_y)
      ) 
  })
  output$overlap_plot <- renderPlotly({
    table <- getOverlapPlot() 
    table$y_pos <- seq(0, nrow(table)-1, 1) 
    type <- tolower(input$plot_overlap_type)
    if (type == "percentage") {
      x_breaks <- c(0, 25, 50, 75, 100)
      x_labels <- c("0%", "25%", "50%", "75%", "100%")
    } else {
      x_breaks <- round(c(0, max(table$total_counts)/4, max(table$total_counts)/2,
                          max(table$total_counts)*3/4, max(table$total_counts)))
      x_labels <- x_breaks
    }
    table %>% 
      ggplot() + 
      # x
      geom_rect(aes_string(xmin = 0, xmax = paste0("subject_", type, "_x"), 
                           ymin = "y_pos-0.35", ymax = "y_pos+0.35"),
                fill = "#669bbc", alpha = 0.5) +
      # y
      geom_rect(aes_string(xmin = paste0("total_", type, "-subject_", type, "_y"), 
                           xmax = paste0("total_", type), 
                           ymin = "y_pos-0.35", ymax = "y_pos+0.35"),
                fill = "#cd5d67", alpha = 0.4) +
      scale_y_continuous(breaks = table$y_pos, labels = table$comparison_name) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, NA)) +
      theme_bw() +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11)) +
      facet_wrap(vars(cdm_name)) +
      geom_col(data = tibble("CohortX" = 0, "CohortY" = 0, to_fill = c("Cohort X", "Cohort Y", "Overlap")),
                 aes_string(x = "CohortX", y = "CohortY", fill = "to_fill")) +
      scale_fill_manual(
        "Legend",
        # breaks = c("Cohort X", "Cohort Y"),
        values=c('#B6CDDE', '#E7BEC2', "#BBA0AE"),
        labels = c('Cohort X', 'Cohort Y', "Overlap") 
      ) +
      guides(fill = guide_legend(order = 3, override.aes = list(fill = c('#B6CDDE', '#E7BEC2', "#BBA0AE")))) +
      ylab("") + xlab("")
  })
  # Age ----
  getAgeDistribution <- reactive({
    filterData(data$age_distribution, "age", input) %>% 
      select(age_group, sex, cdm_name, cohort_name, n) %>% 
      niceColumnNames()
  })
  output$age_tidy_table <- renderDataTable({
    datatable(
      getAgeDistribution(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    ) 
  })
  # Time ----
  getTimeDistribution <- reactive({
    filterData(data$time_distribution, "time", input) %>% 
      niceColumnNames()
  })
  output$time_tidy_table <- renderDataTable({
    datatable(
      getTimeDistribution(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  # Prevalence ----
  output$prevalence_table <- renderDataTable({
    datatable(
      filterData(data$prevalence, "prevalence", input) %>% 
        niceColumnNames() %>% 
        select(input$select_prevalence_columns),
    options = list(
      scrollX = TRUE, 
      scrollCollapse = TRUE,
      lengthChange = FALSE, 
      searching = FALSE, 
      ordering = FALSE, 
      paging = FALSE
    )
  )
  })
  output$prevalence_plot <- renderPlotly({
    table <- filterData(data$prevalence, "prevalence", input) 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$plot_prevalence_color)){
      if(!is.null(input$plot_prevalence_facet_by)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$plot_prevalence_facet_by)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x= "prevalence_start_date", y="prevalence")) +
          geom_point(position=position_dodge(width=1))+
          facet_wrap(vars(facet_var),nrow = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x= "prevalence_start_date", y="prevalence")) +
          geom_point(position=position_dodge(width=1))+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    if(!is.null(input$plot_prevalence_color) ){ 
      
      if(is.null(input$plot_prevalence_facet_by) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$plot_prevalence_color)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x= "prevalence_start_date", y="prevalence",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          theme_bw()
      }
      
      if(!is.null(input$plot_prevalence_facet_by) ){
        if(!is.null(input$plot_prevalence_color) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$plot_prevalence_color)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$plot_prevalence_facet_by)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x= "prevalence_start_date", y="prevalence",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })  
  # Incidence ----
  output$incidence_table <- renderDataTable({
    datatable(
      filterData(data$incidence, "incidence", input) %>% 
        niceColumnNames() %>% 
        select(input$select_incident_columns),
      options = list(
        scrollX = TRUE, 
        scrollCollapse = TRUE,
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$incidence_plot <- renderPlotly({
    
    table <-  filterData(data$incidence, "incidence", input)
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$plot_incidence_color)){
      if(!is.null(input$plot_incidence_facet_by)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$plot_incidence_facet_by)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x= "incidence_start_date", y="incidence_100000_pys")) +
          geom_point(position=position_dodge(width=1))+
          facet_wrap(vars(facet_var),nrow = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x= "incidence_start_date", y="incidence_100000_pys")) +
          geom_point(position=position_dodge(width=1))+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$plot_incidence_color) ){ 
      
      if(is.null(input$plot_incidence_facet_by) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$plot_incidence_color)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x= "incidence_start_date", y="incidence_100000_pys",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          theme_bw()
      }
      
      if(!is.null(input$plot_incidence_facet_by) ){
        if(!is.null(input$plot_incidence_color) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$plot_incidence_color)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$plot_incidence_facet_by)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x= "incidence_start_date", y="incidence_100000_pys",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
  })
  # LSC  ----
  output$lsc_table <- renderDataTable({

    DT::datatable(
    
    filterData(data$lsc_table, "lsc", input) %>% 
      niceColumnNames() %>%
      arrange(`Cohort name`, `Cdm name`, desc(`Difference percentage`)) %>%
      select(input$select_lsc_columns) ,
    
    options = list(scrollX = TRUE),
    rownames = FALSE  
    
    )
    
  })
  

    
    output$lsc_plot <- renderPlotly({
     
     table <- data$lsc_table %>% 
       mutate(label = paste0(concept_name, "; ", window))
    
    if(!is.null(input$plsc_facet)){
      p <- table %>%
        unite("facet_var",
              c(all_of(input$plsc_facet)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x = "sample_percentage", y = "matched_percentage",
                          group= "label",
                          fill = "label",
                          color = "label"
        )) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1) +
        facet_wrap(vars(facet_var),nrow = 2) +
        theme_bw() +
        theme(legend.position = "none")
    } else{
      p <- table %>%
        ggplot(aes_string(x = "sample_percentage", y = "matched_percentage",
                          group= "label",
                          fill = "label",
                          color = "label")) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1) +
        theme_bw() +
        theme(legend.position = "none")
    }
    
    p + xlab("Sample") + ylab("Matched") +
      scale_x_continuous(limits = c(0, 100)) +
      scale_y_continuous(limits = c(0, 100))
    
  })
  # log ----
  output$log <- renderUI({
    filterData(data$log, "log", input) %>% 
      pull(log) %>% 
      formatLog()
  })
}