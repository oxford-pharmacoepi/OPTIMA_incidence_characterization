ui <- dashboardPage(
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$a(href = "https://github.com/OHDSI/PhenotypeR",  # Replace with your desired URL
             tags$img(src = "phenotypeR.png", height = "50px", style = "margin-right: 10px;")  # Adjusted height
      ),
      tags$span("PhenotypeR", style = "margin-left: 10px; font-size: 24px;")  # Optional: Adjust title font size
    ),
    titleWidth = "100%"
  ),
  
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Databases",
        tabName = "cdm_snapshot"
      ),
      menuItem(
        text = "Cohorts",
        tabName = "cohorts"
      ),
      menuItem(
        text = "Cohort counts",
        tabName = "counts"
      ),
      menuItem(
        text = "Code counts",
        tabName = "code_counts"
      ),
      menuItem(
        text = "Orphan codes",
        tabName = "orphan"
      ),
      menuItem(
        text = "Index events",
        tabName = "index"
      ),
      menuItem(
        text = "Cohort overlap",
        tabName = "overlap"
      ),
      menuItem(
        text = "Age distribution",
        tabName = "age"
      ),
      menuItem(
        text = "Time & age distribution",
        tabName = "time"
      ),
      menuItem(
        text = "Population prevalence",
        tabName = "prevalence"
      ),
      menuItem(
        text = "Population incidence",
        tabName = "incidence"
      ),
      menuItem(
        text = "Large scale characterisation",
        tabName = "large_scale_characterisation"
      ),
      menuItem(
        text = "Execution log",
        tabName = "log"
      )
    ),
    
    tags$div(
      style = "position: relative; margin-top: 20px; text-align: center; margin-bottom: 0;",
      a(img(
        src = "Logo_HDS.png",  # Replace with the correct file name and extension
        height = "150px",  # Adjust the height as needed
        width = "auto"     # Let the width adjust proportionally
      ),
      href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
      target = "_blank"
      )
    ) ,
    
    # Logo 
    tags$div(
      style = "position: relative; margin-top: -20px; text-align: center; margin-bottom: 0;",
      a(img(
        src = "logoOxford.png",  # Replace with the correct file name and extension
        height = "150px",  # Adjust the height as needed
        width = "auto"     # Let the width adjust proportionally
      ),
      href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
      target = "_blank"
      )
    )
    
  ),
  
  
  # ## body ----
  dashboardBody(
    use_theme(DUtheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("PhenotypeR"),
        h5("PhenotypeR is a package to assess feasibility of clinical phenotypes (cohorts and/or codelists) to be used for studies. The package is under developement however it relies on other publically available packages as a tool to help phenotyping. The developement version of the PhenotypeR package can be found " , a("here", href = "https://github.com/OHDSI/PhenotypeR")),
      ),
      
      #cdm snapshot ------
      tabItem(
        tabName = "cdm_snapshot",
        h4("Database Summary."),
        selectors(data$cdm_snapshot, "cdm_snapshot", c("cdm_name")),
        #downloadButton("cdm_snapshot_tidy", "Download csv"),
        DTOutput("cdm_snapshot_tidy") %>% withSpinner()
      ),
      # cohort definition ------
      tabItem(
        tabName = "cohorts",
        # h4("Cohort definitions."),
        pickerInput(
          inputId = "cohort_set_input",
          label = "Cohort Set",
          choices = unique(cohort_set$cohort_name),
          selected = unique(cohort_set$cohort_name)[1],
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
          multiple = FALSE
        ),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Cohort definition",
            h4("Below is the cohort definition. We created 4 definitions 1) All lung cancers - this includes all lung cancer codes 2) Broad lung cancer - this includes broad concept terms including maligant neoplasm of respiratory tract but excludes small cell lung cancer concepts 3) Narrow lung cancer - this excludes broad concepts such as malignant neoplasm of the respiratory tract and excludes concepts for small cell lung cancer 4) Small cell lung cancer"),
            uiOutput("markdown")
          ),
          tabPanel(
            "JSON",
            h4("Below is the json files which can be downloaded and exported into ATLAS"),
            downloadButton("downloadLungCancerjson", "All Lung Cancer"),
            downloadButton("downloadLungCancerbroadjson", "Broad Lung Cancer"),
            downloadButton("downloadLungCancernarrowjson", "Narrow Lung Cancer"),
            downloadButton("downloadscLungCancerjson", "Small Cell Lung Cancer"),
            # rclipboardSetup(),
            # uiOutput("clip"),
            # verbatimTextOutput("verb"),
          ) ,
          
          tabPanel(
            "Clinical Definition",
            h4("The clinical description for each phenotype can be downloaded below and then reviewed:"),
            downloadButton("downloadLungCancer", "Lung Cancer"),
            downloadButton("downloadSmallCell", "Small Cell Lung Cancer"),
          ) ,
          
          tabPanel(
            "Concept sets",
            h4("Below are the concept sets used for selected phenotype:"),
            
            DT::dataTableOutput('tbl_concept_sets'),
            
            div(style="display:inline-block",
                downloadButton(
                  outputId = "dt_concept_sets_word",
                  label = "Download table as word"
                ), 
                style="display:inline-block; float:right")
            
          ),
          
          
        )
      ),
      
      # cohort_counts ----
      tabItem(
        tabName = "counts",
        selectors(data$cohort_count, "counts", c("cdm_name", "cohort_name" ), multiple = TRUE, default = list()),
        #selectors(data$cohort_count, "counts", c("cohort_name"), multiple = FALSE, default = list()),
        DTOutput("tidy_counts")
      ),
      tabItem(
        tabName = "code_counts",
        selectors(data$code_counts, "code_counts", c("cdm_name", "cohort"), multiple = FALSE, default = list()),
        pickerInput(
          inputId = "select_code_count_columns",
          label = "Columns to display",
          choices = c("Standard concept id", "Standard concept name", "Cohort", 
                      "Cdm name", "Record count", "Person count"),
          selected = c("Standard concept id", "Standard concept name", "Cohort", 
                       "Cdm name", "Record count", "Person count"),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        ),
        DTOutput("tidy_code_counts")
      ),
      # orphan ----
      tabItem(
        tabName = "orphan",
        selectors(data$orphan_counts, "orphan", c("cdm_name", "cohort"), multiple = FALSE, default = list()),
        pickerInput(
          inputId = "select_orphan_count_columns",
          label = "Columns to display",
          choices = c("Standard concept id", "Standard concept name", "Relationship id", "Cohort", 
                      "Cdm name", "Record count", "Person count"),
          selected = c("Standard concept id", "Standard concept name", "Relationship id", "Cohort", 
                       "Cdm name", "Record count", "Person count"),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        ),
        DTOutput("tidy_orphan_counts")
      ),
      # index ----
      tabItem(
        tabName = "index",
        selectors(data$index_events, 
                  "index", 
                  c("cdm_name", "cohort_name"), 
                  multiple = FALSE, default = list()),
        selectors(data$index_events, 
                  "index", 
                  c("codelist_name",  "domain_id", "group_name"), 
                  multiple = TRUE, default = list()),
        h5(),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "select_index_columns",
            label = "Columns to display",
            choices = c("Standard concept id", "Standard concept name", "Source concept name", 
                        "Source concept id", "Domain id", "Codelist name", "Cohort name", 
                        "Cdm name", "Record count", "Person count"),
            selected = c("Standard concept id", "Standard concept name", "Source concept id", 
                         "Source concept name", "Domain id", "Codelist name", "Cohort name", 
                         "Cdm name", "Record count", "Person count"),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        DTOutput("index_date_tidy")
      ),
      # overlap ----
      tabItem(
        tabName = "overlap",
        selectors(data$cohort_overlap, "overlap", columns = c("cdm_name", "cohort_name_x", "cohort_name_y")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            DTOutput("overlap_tidy")
          ),
          tabPanel(
            "Plot",
            h4(),
            radioButtons(
              inputId = "plot_overlap_type",
              label = "Plot type",
              choices = c("Percentage", "Counts"),
              selected = "Percentage"
            ),
            plotlyOutput("overlap_plot") %>% withSpinner()
          )
        )
      ),
      # age ----
      tabItem(
        tabName = "age",
        selectors(data$age_distribution, "age", c("cdm_name", "cohort_name", "sex", "age_group")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            DTOutput("age_tidy_table")
            # ),
            # tabPanel(
            #   "Formatted table",
            #   h4(),
            #   gt_output("age_format_table")
          )
        )
      ),
      # time ----
      tabItem(
        tabName = "time",
        selectors(data$time_distribution, "time", c("cdm_name", "cohort_name", "sex", "covariate", "estimate_type")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            DTOutput("time_tidy_table")
            # ),
            # tabPanel(
            #   "Formatted table",
            #   h4(),
            #   gt_output("time_format_table")
          )
        )
      ),
      # prevalence ----
      tabItem(
        tabName = "prevalence",
        selectors(data$prevalence, "prevalence", 
                  c("cdm_name", "outcome_cohort_name", 
                    "denominator_age_group", "denominator_sex")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            h5(),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "select_prevalence_columns",
                label = "Columns to display",
                choices = stringr::str_to_sentence(
                  gsub("_", " ", 
                       c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                         "outcome_cohort_name", "prevalence_start_date", "prevalence_end_date",                    
                         "n_cases", "n_population", "prevalence",                             
                         "prevalence_95CI_lower", "prevalence_95CI_upper", "population_obscured",                    
                         "cases_obscured", "result_obscured", "analysis_type", "analysis_interval",                    
                         "analysis_complete_database_intervals", "analysis_time_point", "analysis_full_contribution"))),
                selected = stringr::str_to_sentence(
                  gsub("_", " ", 
                       c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                         "outcome_cohort_name", "prevalence_start_date", "prevalence_end_date",                    
                         "n_cases", "n_population", "prevalence",                             
                         "prevalence_95CI_lower", "prevalence_95CI_upper", "population_obscured",                    
                         "cases_obscured", "result_obscured", "analysis_type", "analysis_interval",                    
                         "analysis_complete_database_intervals", "analysis_time_point", "analysis_full_contribution"))),
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
              )
            ),
            DTOutput("prevalence_table")
          ),
          tabPanel(
            "Plot",
            h4(),
            plotSelectors(prefix = "plot_prevalence", 
                          choices = c("cdm_name", "denominator_age_group", "denominator_sex", "outcome_cohort_name"),
                          default = list(color = "cdm_name", facet_by = "outcome_cohort_name")),
            plotlyOutput("prevalence_plot")
          )
        )
      ),
      # incidence ----
      tabItem(
        tabName = "incidence",
        selectors(data$incidence, "incidence", 
                  c("cdm_name", "outcome_cohort_name", 
                    "denominator_age_group", "denominator_sex")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            h5(),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "select_incident_columns",
                label = "Columns to display",
                choices = stringr::str_to_sentence(
                  gsub("_", " ", 
                       c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                         "outcome_cohort_name", "incidence_start_date", "incidence_end_date",                    
                         "n_events", "n_persons", "person_years", "incidence_100000_pys",                             
                         "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
                         "cohort_obscured", "result_obscured","analysis_interval", "analysis_complete_database_intervals"))),
                selected = stringr::str_to_sentence(
                  gsub("_", " ", 
                       c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                         "outcome_cohort_name", "incidence_start_date", "incidence_end_date",                    
                         "n_events", "n_persons", "person_years", "incidence_100000_pys",                             
                         "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
                         "cohort_obscured", "result_obscured","analysis_interval", "analysis_complete_database_intervals"))),
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
              )
            ),
            DTOutput("incidence_table")
          ),
          tabPanel(
            "Plot",
            h4(),
            plotSelectors(prefix = "plot_incidence",
                          choices = c("cdm_name", "denominator_age_group", "denominator_sex", "outcome_cohort_name"),
                          default = list(color = "cdm_name", facet_by = "outcome_cohort_name")),
            plotlyOutput("incidence_plot")
          )
        )
      ),
      # large_scale_characterisation ----
      
      tabItem(
        tabName = "large_scale_characterisation",
        selectors(data$lsc_table, "lsc", c("cdm_name", "cohort_name", "window")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            h4(),
            pickerInput(
              inputId = "select_lsc_columns",
              label = "Columns to display",
              choices = c("Cdm name", "Cohort name", "Concept name",
                          "Window", "Matched integer", "Matched percentage", "Sample integer",
                          "Sample percentage", "Difference integer", "Difference percentage"),
              selected = c("Cdm name", "Cohort name", "Concept name",
                           "Window", "Matched integer", "Matched percentage", "Sample integer",
                           "Sample percentage", "Difference integer", "Difference percentage"),
              
              # 
              # choices = c("cdm_name", "cohort_name", "concept_name",
              #             "window", "matched_integer", "matched_percentage", "sample_integer",
              #             "sample_percentage", "difference_integer", "difference_percentage"),
              # selected = c("cdm_name", "cohort_name", "concept_name",
              #              "window", "matched_integer", "matched_percentage", "sample_integer",
              #              "sample_percentage", "difference_integer", "difference_percentage"),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            ),
           # DTOutput("lsc_table")
            
            DT::dataTableOutput('lsc_table'),
            
            
          ),
          # tabPanel(
          #   "Plot",
          #   h4(),
          # 
          # 
          #   plotSelectors1(prefix = "plot_lsc",
          #                 choices = c("cdm_name", "cohort_name", "window"),
          #                 default = list(facet_by = c("cdm_name", "cohort_name", "window"))),
          # 
          # 
          # 
          #   plotlyOutput('lsc_plot', height = "800px") %>% withSpinner()
          # )
        )
      ),
      
      # log ----
      tabItem(
        tabName = "log",
        selectors(data$log, "log", c("cdm_name"), multiple = FALSE),
        uiOutput("log")
      )
      # end ----
    )
  )
)
