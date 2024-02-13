# #### UI -----

# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(
    title = div("Menu", style = "text-align: left;"),  # Align title to the left
    titleWidth = 250  # Adjust the width as needed
  ),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background",
        icon = shiny::icon("book")
      ),

      menuItem(
        text = "Database",
        tabName = "dbs",
        icon = shiny::icon("database"),
        menuSubItem(
          text = "Snapshot",
          tabName = "snapshotcdm"
        )
        
      ),
      
      menuItem(
        text = "Cohorts",
        tabName = "cohorts",
        icon = shiny::icon("person"),
        menuSubItem(
          text = "Cohort concepts",
          tabName = "cohort_concepts"
        )
        # ,
        # menuSubItem(
        #   text = "Cohort Attrition Table",
        #   tabName = "cohort_attrition"
        # ),
        # menuSubItem(
        #   text = "Cohort Attrition Figures",
        #   tabName = "cohort_attr_fig"
        # )
      ),
     
      menuItem(
        text = "Characteristics",
        tabName = "char",
        icon = shiny::icon("hospital-user"),
        menuSubItem(
          text = "Demographics",
          tabName = "demographics"
        ),
        menuSubItem(
          text = "Medications",
          tabName = "medications"
        ),
        menuSubItem(
          text = "Comorbidities",
          tabName = "comorbidities"
        )
        ),

      
      menuItem(
        text = "Incidence",
        tabName = "incidence",
        icon = shiny::icon("chart-line") ,
        menuSubItem(
          text = "Crude Plots",
          tabName = "inc_plots"
        ),
        menuSubItem(
          text = "Standardized Plots",
          tabName = "inc_plots_std"
        ),
        menuSubItem(
          text = "Crude Incidence Estimates",
          tabName = "inc_rates"
        ),
        menuSubItem(
          text = "Std Incidence Estimates",
          tabName = "inc_rates_std"
        ),
        menuSubItem(
          text = "Attrition Table",
          tabName = "inc_attrition"
        ),
        menuSubItem(
          text = "Attrition Figure",
          tabName = "inc_attrition_fig"
        )
        
      ),
      
      
      menuItem(
        text = "Overall Survival",
        tabName = "os",
        icon = shiny::icon("life-ring") ,
        menuSubItem(
          text = "Survival Plots",
          tabName = "survival_results"
        ),
        menuSubItem(
          text = "Risk Table",
          tabName = "risk_results"
        ),
        
        menuSubItem(
          text = "Survival Summary",
          tabName = "stats_results"
        ) ,
      
      menuSubItem(
        text = "Attrition Table",
        tabName = "cohort_attrition"
      ),
      menuSubItem(
        text = "Attrition Figure",
        tabName = "cohort_attr_fig"
      )
    ),
      
      menuItem(
        text = "Calendar Time Survival",
        tabName = "os",
        icon = shiny::icon("calendar") ,
        menuSubItem(
          text = "Survival Plots",
          tabName = "survival_resultscy"
        ),
        menuSubItem(
          text = "Risk Table",
          tabName = "risk_resultscy"
        ),
        
        menuSubItem(
          text = "Survival Summary",
          tabName = "stats_resultscy"
        ) 
      ),
      
      
      # Logo 
      tags$div(
        style = "position: relative; margin-top: -10px; text-align: center; margin-bottom: 0;",
        a(img(
          src = "logoOxford.png",  # Replace with the correct file name and extension
          height = "150px",  # Adjust the height as needed
          width = "auto"     # Let the width adjust proportionally
        ),
        href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
        target = "_blank"
        )
      )
    )
  ),
  
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("Incidence and Survival of Lung Cancer: a multinational cohort study"),
        tags$h4(tags$strong("Please note, the results presented here should be considered as
                                                preliminary and subject to change.")),
        
        tags$h5(
          tags$span("Background:", style = "font-weight: bold;"),
          "TBC"
        ),

        tags$h5(
          tags$span(" Methods:", style = "font-weight: bold;"),
          "TBC"
          
          ),
        
        tags$h5(
          tags$span(" Results:", style = "font-weight: bold;"),
          "TBC"
          
        ),
        
        tags$h5(
          tags$span("Funding:" , style = "font-weight: bold;"),
                "This research was funded by Optimal treatment for patients with solid tumours in Europe through Artificial Intelligence (OPTIMA) initiative (grant number 101034347)."
        ),
        
        tags$h5("The results of this study are published in the following journal:"
        ),
        tags$ol(
          tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi", "here"),
                ".The cohort diagnostics including the clinical codelists for lung cancer phenotypes can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi", "here")
                
        ),
        
        tags$h5("Any questions regarding this shiny app please contact",
                tags$a(href="mailto:danielle.newby@ndorms.ox.ac.uk", "Danielle Newby"), "and any questions regarding this study please contact the corresponding author",
                tags$a(href="mailto:daniel.prietoalhambra@ndorms.ox.ac.uk", "Professor Daniel Prieto Alhambra")
                
        ),
        
        
        
        tags$hr()
        
        
      ),
      
      # cdm snapshot ------
      tabItem(
        tags$h5("Snapshot of the cdm from database"),
        tabName = "snapshotcdm",
        htmlOutput('tbl_cdm_snaphot'),
        tags$hr(),
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_cdm_snaphot_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      ) ,
      
      tabItem(
        tabName = "inc_attrition",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_cohort_name_selector",
            label = "Study cohort",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = "Lung",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        

        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_time_selector",
            label = "Time",
            choices = unique(incidence_attrition$analysis_interval),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('tbl_incidence_attrition'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "dt_incidence_attrition_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      tabItem(
        tabName = "demographics",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_cohort_selector",
            label = "Cancer",
            choices = unique(demo_characteristics$group_level),
            selected = unique(demo_characteristics$group_level),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_selector",
            label = "Demographics",
            choices = unique(demo_characteristics$strata_level),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
       # tags$hr(),
        gt_output("gt_patient_characteristics") %>% 
          withSpinner() ,
        

        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_patient_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")

      ) ,
      
      
      tabItem(
        tags$h5("The clinical codelists for each cancer used in this study are presented below:"),
        tabName = "cohort_concepts",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "codelist_cohort_selector",
            label = "Cancer",
            choices = unique(concepts_lists$Cancer),
            selected = "Lung",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "codelist_vocab_selector",
            label = "Vocabulary",
            choices = unique(concepts_lists$Vocabulary),
            selected = unique(concepts_lists$Vocabulary),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('tbl_codelists'),
        
        tags$hr(),
        
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_codelists_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      ) , 
      
      
      
      tabItem(
        tabName = "inc_rates",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_cohort_selector",
            label = "Cancer",
            choices = unique(incidence_estimates$outcome_cohort_name),
            selected = "lung",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_analysis_selector",
            label = "Analysis Interval",
            choices = unique(incidence_estimates$analysis_interval),
            selected = "years",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_inc_est_table'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "dt_inc_est_table_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      tabItem(
        tabName = "inc_rates_std",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_cohort_selector_std",
            label = "Cancer",
            choices = unique(incidence_estimates_std$outcome_cohort_name),
            selected = "lung",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_inc_est_table_std'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "dt_inc_est_table_word_std",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      tabItem(
        tabName = "risk_results",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "risk_table_cohort_name_selector",
            label = "Study cohort",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = "lung",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "risk_table_database_name_selector",
            label = "Database",
            choices = unique(incidence_attrition$cdm_name),
            selected = unique(incidence_attrition$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_risk_table'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_risk_table_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      
      tabItem(
        tabName = "stats_results",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "median_cohort_name_selector",
            label = "Study cohort",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = "lung",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "median_database_name_selector",
            label = "Database",
            choices = unique(incidence_attrition$cdm_name),
            selected = unique(incidence_attrition$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_median_table'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_median_table_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),   
      
      # tabItem(
      #   tabName = "risk_resultscy",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "risk_table_cohort_name_selectorcy",
      #       label = "Study cohort",
      #       choices = unique(survival_risk_cy_table$Cancer),
      #       selected = "Breast",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "risk_table_database_name_selectorcy",
      #       label = "Database",
      #       choices = unique(survival_risk_cy_table$Database),
      #       selected = unique(survival_risk_cy_table$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   htmlOutput('dt_risk_tablecy'),
      #   
      #   div(style="display:inline-block",
      #       downloadButton(
      #         outputId = "gt_risk_tablecy_word",
      #         label = "Download table as word"
      #       ), 
      #       style="display:inline-block; float:right")
      #   
      # ),
      # 
      # 
      # tabItem(
      #   tabName = "stats_resultscy",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "median_cohort_name_selectorcy",
      #       label = "Study cohort",
      #       choices = unique(survival_median_table_cy$Cancer),
      #       selected = "Breast",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "median_database_name_selectorcy",
      #       label = "Database",
      #       choices = unique(survival_median_table_cy$Database),
      #       selected = unique(survival_median_table_cy$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   htmlOutput('dt_median_tablecy'),
      #   
      #   div(style="display:inline-block",
      #       downloadButton(
      #         outputId = "gt_median_tablecy_word",
      #         label = "Download table as word"
      #       ), 
      #       style="display:inline-block; float:right")
      #   
      # ),   
      # 
      # tabItem(
      #   tabName = "survival_resultscy",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "survival_database_selectorcy",
      #       label = "Database",
      #       choices = unique(survival_estimates$Database),
      #       selected = unique(survival_estimates$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "survival_cohort_name_selectorcy",
      #       label = "Cancer",
      #       choices = unique(survival_estimates$Cancer),
      #       selected = unique(survival_estimates$Cancer),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   div(style="display: inline-block;vertical-align:top; width: 150px;",
      #       pickerInput(inputId = "surv_plot_facetcy",
      #                   label = "Facet by",
      #                   choices = c("Cancer"
      #                   ),
      #                   selected = c("Cancer" ),
      #                   options = list(
      #                     `actions-box` = TRUE,
      #                     size = 10,
      #                     `selected-text-format` = "count > 3"),
      #                   multiple = TRUE,)
      #   ),
      #   
      #   div(style="display: inline-block;vertical-align:top; width: 150px;",
      #       pickerInput(inputId = "surv_plot_groupcy",
      #                   label = "Colour by",
      #                   choices = c("Cancer"),
      #                   selected = c("Cancer"),
      #                   options = list(
      #                     `actions-box` = TRUE,
      #                     size = 10,
      #                     `selected-text-format` = "count > 3"),
      #                   multiple = TRUE,)
      #       
      #       
      #   ),
      # 
      #   div(
      #     style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
      #     checkboxInput("show_ci_cy", "Show Confidence Intervals", value = TRUE)
      #   ),
      #   
      #   div(
      #     style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
      #     plotOutput("survivalPlotcy",
      #                height = "800px"
      #     ) %>%
      #       withSpinner(),
      #     h4("Download Figure"),
      #     div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
      #     div(
      #       style = "display: inline-block;",
      #       textInput("survival_download_heightcy", "", 30, width = "50px")
      #     ),
      #     div("cm", style = "display: inline-block; margin-right: 25px;"),
      #     div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
      #     div(
      #       style = "display: inline-block;",
      #       textInput("survival_download_widthcy", "", 35, width = "50px")
      #     ),
      #     div("cm", style = "display: inline-block; margin-right: 25px;"),
      #     div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
      #     div(
      #       style = "display: inline-block; margin-right:",
      #       textInput("survival_download_dpicy", "", 600, width = "50px")
      #     ),
      #     downloadButton("survival_cy_download_plot", "Download plot")
      #   )
      #   
      #   
      # ),
      # 
      
      
      tabItem(
        tabName = "inc_plots",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_database_selector",
            label = "Database",
            choices = unique(incidence_estimates$cdm_name),
            selected = unique(incidence_estimates$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_cohort_name_selector",
            label = "Cancer",
            choices = unique(incidence_estimates$outcome_cohort_name),
            selected = unique(incidence_estimates$outcome_cohort_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_denominator_analysis_interval_selector",
            label = "Analysis Interval",
            choices = unique(incidence_estimates$analysis_interval),
            selected = "years",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),

        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_start_date_selector",
            label = "Incidence Start Date",
            choices = as.character(unique(incidence_estimates$incidence_start_date)),
            selected = as.character(unique(incidence_estimates$incidence_start_date)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_x_axis",
                        label = "X axis",
                        choices = c("incidence_start_date"),
                        selected = "incidence_start_date",
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = FALSE,)
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_facet",
                        label = "Facet by",
                        choices = c("Cancer"),
                        selected = c("Cancer"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_group",
                        label = "Colour by",
                        choices = c("Cancer"),
                        selected = c("Cancer"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
            
            
        ),
        
        div(
          style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
          checkboxInput("show_error_bars", "Show Ribbons", value = TRUE)
        ),
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          plotOutput("incidencePlot",
                     height = "800px"
          ) %>%
            withSpinner(),
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("incidence_download_height", "", 30, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("incidence_download_width", "", 35, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("incidence_download_dpi", "", 600, width = "50px")
          ),
          downloadButton("incidence_download_plot", "Download plot")
        )
        
        
      ),
      
      tabItem(
        tabName = "inc_plots_std",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_database_selector_std",
            label = "Database",
            choices = unique(incidence_estimates_std$cdm_name),
            selected = unique(incidence_estimates_std$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_cohort_name_selector_std",
            label = "Cancer",
            choices = unique(incidence_estimates_std$Cancer),
            selected = unique(incidence_estimates_std$Cancer),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_start_date_selector_std",
            label = "Incidence Start Date",
            choices = as.character(unique(incidence_estimates_std$Subgroup)),
            selected = as.character(unique(incidence_estimates_std$Subgroup)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_x_axis_std",
                        label = "X axis",
                        choices = c("Subgroup"),
                        selected = "Subgroup",
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = FALSE,)
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_facet_std",
                        label = "Facet by",
                        choices = c("Cancer"),
                        selected = c("Cancer"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_group_std",
                        label = "Colour by",
                        choices = c("Cancer"),
                        selected = c("Cancer"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
            
            
        ),
        
        div(
          style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
          checkboxInput("show_error_bars_std", "Show Ribbons", value = TRUE)
        ),
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          plotOutput("incidencePlotstd",
                     height = "800px"
          ) %>%
            withSpinner(),
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("incidence_download_heightstd", "", 30, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("incidence_download_widthstd", "", 35, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("incidence_download_dpistd", "", 600, width = "50px")
          ),
          downloadButton("incidence_download_plotstd", "Download plot")
        )
        
        
      ),
      
      
      
      tabItem(
        tabName = "survival_results",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_database_selector",
            label = "Database",
            choices = unique(survival_estimates$Database),
            selected = unique(survival_estimates$Database),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_cohort_name_selector",
            label = "Cancer",
            choices = unique(survival_estimates$Cancer),
            selected = unique(survival_estimates$Cancer),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "surv_plot_facet",
                        label = "Facet by",
                        choices = c("Cancer"),
                        selected = c("Cancer" ),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "surv_plot_group",
                        label = "Colour by",
                        choices = c("Cancer"),
                        selected = c("Cancer"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)


    ),
    div(
      style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
      checkboxInput("show_ci", "Show Confidence Intervals", value = TRUE)
    ),

    
    div(
      style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
      plotOutput("survivalPlot",
                 height = "800px"
      ) %>%
        withSpinner(),
      h4("Download Figure"),
      div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
      div(
        style = "display: inline-block;",
        textInput("survival_download_height", "", 30, width = "50px")
      ),
      div("cm", style = "display: inline-block; margin-right: 25px;"),
      div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
      div(
        style = "display: inline-block;",
        textInput("survival_download_width", "", 35, width = "50px")
      ),
      div("cm", style = "display: inline-block; margin-right: 25px;"),
      div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
      div(
        style = "display: inline-block; margin-right:",
        textInput("survival_download_dpi", "", 600, width = "50px")
      ),
      downloadButton("survival_download_plot", "Download plot")
    )
    
)

      
      # more tabs here
    )
    
  )  
  
  
)


