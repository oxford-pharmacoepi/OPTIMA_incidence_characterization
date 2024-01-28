# server shiny ----
server <- function(input, output, session) {

 ## cdm snapshot ----
  output$tbl_cdm_snaphot <- renderText(kable(cdm_snapshot) %>%
                                          kable_styling("striped", full_width = F) )
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(cdm_snapshot)
      gtsave(x, file)
    }
  )
  
  # cohort_attrition ----
  getAttrition <- reactive({
    cohort_attrition %>% 
      filter(cohort_name %in% input$attr_cohort_name) %>% 
      select(contains(c("cohort_name", "reason")) | 
               contains(input$attr_cdm))

  })
  
  output$dt_cohort_attrition <- renderDataTable({
  datatable(getAttrition(),
    rownames = FALSE,
    extensions = "Buttons",
    options = list(scrollX = TRUE, scrollCollapse = TRUE)
  )
  })
  
  output$gt_cohort_attrition_word <- downloadHandler(
    filename = function() {
      "cohort_attrition.docx"
    },
    content = function(file) {
      x <- gt(getAttrition())
      gtsave(x, file)
    }
  )
  
  # demograhics ----
  getsummaryDemographics <- reactive({
    results_summaryDemographics %>% 
      filter(cdm_name %in% input$dems_cdm) %>% 
      filter(group_level %in% input$dems_cohort_name) 
  })
  
  getNewNames <- reactive({
    working_result <- results_summaryDemographics %>% 
      filter(cdm_name %in% input$dems_cdm) %>% 
      filter(group_level %in% input$dems_cohort_name)
    
    paste0(rep(unique(working_result$group_level), 
                      times = 
                 length(unique(working_result$cdm_name))
                 )," (",
           rep(unique(working_result$cdm_name),
               each = 
                 length(unique(working_result$group_level))
           ), ")")
    
  })
  
  getgtsummaryDemographics <- reactive({
    chars_gt <- gtCharacteristics(summarisedCharacteristics = 
                                    getsummaryDemographics())

    chars_gt$`_data`<-   bind_rows(
    chars_gt$`_data` %>% 
      filter(Variable %in% c("Number subjects","Number records",
                             "Cohort start date",
                             "Cohort end date", "Age", "Age group", "") & 
               Level!= "Male" ),
    chars_gt$`_data` %>% 
      filter(Level %in%  c("Male", "Female", "None" )),
    chars_gt$`_data` %>% 
      filter(!Variable %in% c("Number subjects","Number records", "Cohort start date",
                             "Cohort end date", "Age", "Age group", "") & 
               Variable!= "Sex" ))
    chars_gt$`_data`[13,1] <-""
    
  new_names <-  getNewNames()     
  
  
  for(i in seq_along(new_names)){
    chars_gt <- chars_gt %>% 
      cols_label(
        !!paste0("cohort", i) := new_names[i],
      )
  }
  
  rm_spanners(chars_gt)
  })
  
  output$gt_summaryDemographics <- render_gt({
    getgtsummaryDemographics()
  })
  
  output$gt_summaryDemographics_word <- downloadHandler(
    filename = function() {
      "summaryCharacteristicsTable.docx"
    },
    content = function(file) {
      x <- getgtsummaryDemographics()
      gtsave(x, file)
    }
  )
  
  
  
  # comorbidity ----
  
  getsummaryComorbidity  <- reactive({
    results_summaryComorbidity %>% 
      filter(cdm_name %in% input$cmorbs_cdm) %>% 
      filter(cohort_name %in% input$cmorbs_cohort_name)  %>% 
      filter(time_window %in% input$cmorbs_time_window)   %>% 
      filter(variable_level %in% input$cmorbs_variable_level) %>% 
      filter(strata_level %in% input$cmorbs_strata_level)  %>% 
      pivot_wider(names_from = c("cdm_name", "time_window"),
                  values_from = c("count", "percentage", "count_percentage"), 
                  names_glue = "{cdm_name} [{time_window}] {.value}",
                  names_vary = "slowest")
  })
  
  output$dt_summaryComorbidity  <- DT::renderDataTable({
    table_data <- getsummaryComorbidity()
    round_cols<- names(table_data[str_detect(names(table_data), "percentage")])
    round_cols<- round_cols[str_detect(round_cols, "count_percentage", 
                                       negate = TRUE)]
    
    datatable(table_data, rownames= FALSE) %>%
      formatRound(columns=c(round_cols), digits=2)
  })
  
  output$dt_summaryComorbidity_csv <- downloadHandler(
    filename = function() {
      "summarysummaryComorbidity.csv"
    },
    content = function(file) {
      x <- getsummaryComorbidity()
      write.csv(x, 
                file, 
                row.names=FALSE)
    }
  )
  
  # medications ---- 
  getsummaryMedications <- reactive({
    results_summaryMedications %>% 
      filter(cdm_name %in% input$meds_cdm) %>% 
      filter(cohort_name %in% input$meds_cohort_name)  %>% 
      filter(time_window %in% input$meds_time_window)   %>% 
      filter(variable_level %in% input$meds_variable_level) %>% 
      filter(strata_level %in% input$meds_strata_level)   %>% 
      pivot_wider(names_from = c("cdm_name", "time_window"),
                  values_from = c("count", "percentage", "count_percentage"), 
                  names_glue = "{cdm_name} [{time_window}] {.value}",
                  names_vary = "slowest")
    
  }) 
  
  output$dt_summaryMedications  <- DT::renderDataTable({
    table_data <- getsummaryMedications()
    round_cols<- names(table_data[str_detect(names(table_data), "percentage")])
    round_cols<- round_cols[str_detect(round_cols, "count_percentage", 
                                             negate = TRUE)]
    
    datatable(table_data, rownames= FALSE) %>%
      formatRound(columns=c(round_cols), digits=2)
  })
  
  output$dt_summaryMedications_csv <- downloadHandler(
    filename = function() {
      "summaryMedications.csv"
    },
    content = function(file) {
      x <- getsummaryMedications()
      write.csv(x, 
                file, 
                row.names=FALSE)
    }
  )
  
  # surv ----
  
  get_surv_table_data <- reactive({
   working_results_summary_survival <- results_summary_survival %>% 
      filter(cdm_name %in% input$surv_cdm) %>% 
      filter(strata_level %in% input$surv_strata_level) 
    names(working_results_summary_survival)<- str_replace_all(names(working_results_summary_survival), "_", " ")
    names(working_results_summary_survival)<-str_to_sentence(names(working_results_summary_survival))
    
    working_results_summary_survival
  }) 
  
  output$dt_surv <- DT::renderDataTable({
    datatable(get_surv_table_data(), rownames= FALSE) %>%
      formatRound(columns=c('Restricted mean survival (years)',
                            'Median survival (years)'
                            ), digits=2)
  })
  
  
  get_surv_plot <- reactive({
    plot_data <- results_survival %>% 
      filter(cdm_name %in% input$surv_cdm) %>% 
      filter(strata_level %in% input$surv_strata_level)
    

    
    if(isFALSE(input$surv_cumulative)){
    if(any(input$surv_facet == "none") & any(input$surv_colour == "none")){
      plot <-   plotSurvival(plot_data,
                   xscale = "years")+theme_bw()
    } else if(any(input$surv_facet != "none") & any(input$surv_colour == "none")){
      plot <-  plotSurvival(plot_data, facet = input$surv_facet,
                   xscale = "years")
    } else if(any(input$surv_facet == "none") & any(input$surv_colour != "none")){
      plot <-  plotSurvival(plot_data, colour = input$surv_colour,
                   xscale = "years")
    } else {
      plot <-  plotSurvival(plot_data, facet = input$surv_facet, colour = input$surv_colour,
                   xscale = "years")
    }
      
      }   else {
        if(any(input$surv_facet == "none") & any(input$surv_colour == "none")){
          plot <-   plotCumulativeIncidence(plot_data,
                       xscale = "years")+theme_bw()
        } else if(any(input$surv_facet != "none") & any(input$surv_colour == "none")){
          plot <-   plotCumulativeIncidence(plot_data, facet = input$surv_facet,
                       xscale = "years")
        } else if(any(input$surv_facet == "none") & any(input$surv_colour != "none")){
          plot <-  plotCumulativeIncidence(plot_data, colour = input$surv_colour,
                       xscale = "years")
        } else {
          plot <-  plotCumulativeIncidence(plot_data, facet = input$surv_facet, colour = input$surv_colour,
                       xscale = "years")
        }      
      
      
      }
    
    if(any(input$surv_colour != "none")){
    if(all(input$surv_strata_level %in% c("18 to 44","45 to 59", "60 to 69", ">=70" , "0 to 17"))){
      
      plot <- plot +
        scale_colour_discrete(breaks=c("0 to 17","18 to 44","45 to 59", "60 to 69", ">=70")) +
        scale_linetype_discrete(breaks=c("0 to 17","18 to 44","45 to 59", "60 to 69", ">=70"))
      
    }}
    
    plot
  }) 
  
  output$surv_plot <- renderPlot(
    
      get_surv_plot() 
   
      
    
    
  )
  
  
  output$surv_download_plot <- downloadHandler(
    filename = function() {
      "survival.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_surv_plot(),
        width = as.numeric(input$surv_download_width),
        height = as.numeric(input$surv_download_height),
        dpi = 300,
        units = "cm"
      )
    }
  )
  
  
  ## TreatmentPatterns ----

  filterData <- reactive({
    med_tp  %>%
      filter(
        .data$cdm_name %in% input$tp_cdm,
        # .data$strata_name %in% input$tp_strata_name,
        .data$strata_level %in% input$tp_strata_level)
  })
  
  output$treatmentPathways <- DT::renderDataTable(filterData(), 
                                                  rownames= FALSE)

  output$sankey <- renderUI({
        # tags$iframe(seamless="seamless", 
        #             src= "tmpuser/foo.html", 
        #             width=800, 
        #             height=800)

    tags$iframe(seamless="seamless", 
                src = paste0("sankey_", input$tp_strata_level, "_",
                             input$tp_cdm, ".html"), 
                             style='width:100vw;height:100vh;')
  })

  getSunburstPath <- reactive({
    paste0("sunburst_", input$tp_strata_level, "_",
                       input$tp_cdm, ".png")
  })

  output$sunburst <- renderUI({
    img(src = getSunburstPath())
  })
  

}

