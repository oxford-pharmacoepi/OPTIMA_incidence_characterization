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
          text = "Cohort Concepts",
          tabName = "cohort_concepts"
        ),
        menuSubItem(
          text = "Clinical Cohort Descriptions",
          tabName = "cohort_description"
        )

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
        icon = shiny::icon("shower") ,
        menuSubItem(
          text = "Crude Plots",
          tabName = "inc_plots"
        ),
        menuSubItem(
          text = "Standardized Plots",
          tabName = "inc_plots_std"
        ),
        menuSubItem(
          text = "Crude Estimates",
          tabName = "inc_rates"
        ),
        menuSubItem(
          text = "Age Standardized Estimates",
          tabName = "inc_rates_std"
        ),
        menuSubItem(
          text = "Attrition Table",
          tabName = "inc_attrition"
        )
        # menuSubItem(
        #   text = "Attrition Figure",
        #   tabName = "inc_attrition_fig"
        # )

      ),
      
      menuItem(
        text = "Prevalence",
        tabName = "prevalence",
        icon = shiny::icon("bath") ,
        menuSubItem(
          text = "Plots",
          tabName = "prev_plots"
        ),
        menuSubItem(
          text = "Prevalence Estimates",
          tabName = "prev_rates"
        ),
        menuSubItem(
          text = "Attrition Table",
          tabName = "prev_attrition"
        )
        # menuSubItem(
        #   text = "Attrition Figure",
        #   tabName = "prev_attrition_fig"
        # )

      ),
      
      menuItem(
        text = "Survival",
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
    
    
    )
  ),
  
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("Worldwide incidence of lung cancer: a multinational cohort study"),
        tags$h4(tags$strong("Please note, the results presented here should be considered as
                                                preliminary and subject to change.")),
        
        tags$h5(
          tags$span("Background:", style = "font-weight: bold;"),
          "Lung cancer is the leading cause of cancer-associated mortality worldwide. New diagnoses are predicted to nearly double by 2070 meaning it will continue to be a major cause of morbidity and mortality globally. While lung cancer incidence rates have been declining in some high-income countries due to reductions in smoking and advancements in screening and treatment, the incidence continues to rise in many low- and middle-income countries. This disparity underscores the importance of understanding the regional variations in lung cancer burden and identifying the underlying factors driving these trends. The aim of this study is to estimate lung cancer incidence in a variety of different data sources across the globe."
          
        ),

        tags$h5(
          tags$span(" Methods:", style = "font-weight: bold;"),
          "We performed a population-based cohort study using the real world databases from primary care and claims. The study period was from January 1, 2003 (or the earliest available data) until database exit, death, or the end of the study on 1st January, 2023. Participants aged 18+ years, with a diagnoses of primary lung cancer, with one-year of prior data availability, were included. We estimated overall lung cancer incidence rates (IR) and stratified by sex and age groups using the",
          tags$a(href="https://darwin-eu.github.io/IncidencePrevalence/", "IncidencePrevalence R package"),
 "Crude IRs were age standardized using European and World Standard populations."
          
          ),
        
        tags$h5(
          tags$span(" Results:", style = "font-weight: bold;"),
          "TBC"
          
        ),
        
        tags$h5(
          tags$span("Funding:" , style = "font-weight: bold;"),
                "This research was funded by Optimal treatment for patients with solid tumours in Europe through Artificial Intelligence (",
          tags$a(href="https://www.optima-oncology.eu/", "OPTIMA"),
          ") which has received funding from the Innovative Medicines Initiative 2 (IMI2) Joint Undertaking under grant agreement No. 101034347. IMI2 receives support from the European Union Horizon 2020 research and innovation programme and European Federation of Pharmaceutical Industries and Associations (EFPIA). The sponsors of the study did not have any involvement in the writing of the manuscript or the decision to submit it for publication. Additionally, there was partial support from the Oxford NIHR Biomedical Research Centre. The corresponding author had full access to all the data in the study and had final responsibility for the decision to submit for publication."
        ),
        
        tags$h5("The results of this study are published in the following journal:"
        ),
        tags$ol(
          tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi/OPTIMA_incidence_survival", "here"),
                ".The cohort diagnostics for lung cancer phenotypes can be found",
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
        tags$h5("Below is the clinical description of the phenotypes used in this study:"),
        tabName = "cohort_description",
        tags$h5("TBC") ,
        tags$hr(),

      ) ,
      
      tabItem(
        tabName = "inc_attrition",
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_outcome_selector",
            label = "Cohort Name",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = unique(incidence_attrition$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_sex_selector",
            label = "Sex",
            choices = unique(incidence_attrition$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_age_selector",
            label = "Age Group",
            choices = unique(incidence_attrition$denominator_age_group),
            selected = unique(incidence_attrition$denominator_age_group)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_database_name_selector",
            label = "Database",
            choices = unique(incidence_attrition$cdm_name),
            selected = unique(incidence_attrition$cdm_name)[1],
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
        tabName = "prev_attrition",
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_outcome_selectorp",
            label = "Cohort Name",
            choices = unique(prevalence_attrition$outcome_cohort_name),
            selected = unique(prevalence_attrition$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_sex_selectorp",
            label = "Sex",
            choices = unique(prevalence_attrition$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_age_selectorp",
            label = "Age Group",
            choices = unique(prevalence_attrition$denominator_age_group),
            selected = unique(prevalence_attrition$denominator_age_group)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_database_name_selectorp",
            label = "Database",
            choices = unique(prevalence_attrition$cdm_name),
            selected = unique(prevalence_attrition$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        htmlOutput('tbl_prevalence_attrition'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "dt_prevalence_attrition_word",
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
            label = "Cohort Name",
            choices = unique(demo_characteristics$group_level),
            selected = unique(demo_characteristics$group_level)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_database_name_selector",
            label = "Database",
            choices = unique(demo_characteristics$cdm_name),
            selected = unique(demo_characteristics$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),

        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_sex_selector",
            label = "Sex",
            choices = unique(demo_characteristics$sex),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_age_selector",
            label = "Age Group",
            choices = unique(demo_characteristics$age_group),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
       # tags$hr(),
        gt_output("gt_demo_characteristics") %>% 
          withSpinner() ,
        

        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_demo_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")

      ) ,
      
      tabItem(
        tabName = "comorbidities",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "comorb_cohort_selector",
            label = "Cohort Name",
            choices = unique(comorb_characteristics$group_level),
            selected = unique(comorb_characteristics$group_level)[2],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "comorb_database_name_selector",
            label = "Database",
            choices = unique(comorb_characteristics$cdm_name),
            selected = unique(comorb_characteristics$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "comorb_sex_selector",
            label = "Sex",
            choices = unique(comorb_characteristics$sex),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "comorb_age_selector",
            label = "Age Group",
            choices = unique(comorb_characteristics$age_group),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "comorb_time_selector",
            label = "Time",
            choices = comorb_characteristics %>%
              filter(window != "overall") %>%
              pull(window) %>%
              unique(),
            selected = "-999999 to -1",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        # tags$hr(),
        gt_output("gt_comorb_characteristics") %>% 
          withSpinner() ,
        
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_comorb_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      ) ,
      
      tabItem(
        tabName = "medications",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "med_cohort_selector",
            label = "Cohort Name",
            choices = unique(med_characteristics$group_level),
            selected = unique(med_characteristics$group_level)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "med_database_name_selector",
            label = "Database",
            choices = unique(med_characteristics$cdm_name),
            selected = unique(med_characteristics$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "med_sex_selector",
            label = "Sex",
            choices = unique(med_characteristics$sex),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "med_age_selector",
            label = "Age Group",
            choices = unique(med_characteristics$age_group),
            selected = "overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "med_time_selector",
            label = "Time Window",
            choices = med_characteristics %>%
              filter(window != "overall") %>%
              pull(window) %>%
              unique(),
            selected = "-365 to -1",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        # tags$hr(),
        gt_output("gt_med_characteristics") %>% 
          withSpinner() ,
        
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_med_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      ) ,
        
        # cohort definition ------
        tabItem(
          tabName = "cohort_concepts",
          
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
              uiOutput("markdown")
            ),
            tabPanel(
              "JSON",
              h4(),
              rclipboardSetup(),
              uiOutput("clip"),
              verbatimTextOutput("verb"),
            ) ,
            tabPanel(
            "Concept sets",
             )
          )
        ),
      
      tabItem(
        tabName = "inc_rates",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_cohort_selector",
            label = "Cohort Name",
            choices = unique(incidence_estimates$outcome_cohort_name),
            selected = unique(incidence_estimates$outcome_cohort_name)[1],
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
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_database_selector",
            label = "Database",
            choices = unique(incidence_estimates$cdm_name),
            selected = unique(incidence_estimates$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_sex_selector",
            label = "Sex",
            choices = unique(incidence_estimates$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_age_selector",
            label = "Age Group",
            choices = unique(incidence_estimates$denominator_age_group),
            selected = "18 to 150",
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
            label = "Cohort Name",
            choices = unique(incidence_estimates_std$outcome_cohort_name),
            selected = unique(incidence_estimates_std$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_sex_selector_std",
            label = "Sex",
            choices = unique(incidence_estimates_std$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_database_selector_std",
            label = "Database",
            choices = unique(incidence_estimates_std$cdm_name),
            selected = unique(incidence_estimates_std$cdm_name)[1],
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
            label = "Cohort Name",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = unique(incidence_attrition$outcome_cohort_name)[1],
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
            label = "Cohort Name",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = unique(incidence_attrition$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        # div(
        #   style = "display: inline-block;vertical-align:top; width: 150px;",
        #   pickerInput(
        #     inputId = "median_demo_selector",
        #     label = "Demographics",
        #     choices = unique(survival_median_table$strata_level),
        #     selected = unique(survival_median_table$strata_level)[1],
        #     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        #     multiple = TRUE
        #   )
        # ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "median_demo_selector",
            label = "Demographics",
            choices = {
              tryCatch({
                unique_vals <- unique(survival_median_table$strata_level)
                if (!is.null(unique_vals)) {
                  return(unique_vals)
                } else {
                  return("No data available")
                }
              }, error = function(e) {
                return("Error retrieving data")
              })
            },
            selected = {
              tryCatch({
                unique_vals <- unique(survival_median_table$strata_level)
                if (!is.null(unique_vals)) {
                  return(unique_vals[1])
                } else {
                  return(NULL)
                }
              }, error = function(e) {
                return(NULL)
              })
            },
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ) ,
        
        
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
        
  
        htmlOutput("dt_surv_stats"),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "dt_surv_stat_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),   
      
      
      tabItem(
        tabName = "prev_rates",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prev_estimates_cohort_selector",
            label = "Cohort Name",
            choices = unique(prevalence_estimates$outcome_cohort_name),
            selected = unique(prevalence_estimates$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prev_estimates_cdm_selector",
            label = "Database",
            choices = unique(prevalence_estimates$cdm_name),
            selected = unique(prevalence_estimates$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prev_estimates_sex_selector",
            label = "Sex",
            choices = unique(prevalence_estimates$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prev_estimates_age_selector",
            label = "Age Group",
            choices = unique(prevalence_estimates$denominator_age_group),
            selected = "18 to 150",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_prev_est_table'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "dt_prev_est_table_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      tabItem(
        tabName = "prev_plots",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_database_selector",
            label = "Database",
            choices = unique(prevalence_estimates$cdm_name),
            selected = unique(prevalence_estimates$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_cohort_name_selector",
            label = "Cohort Name",
            choices = unique(prevalence_estimates$outcome_cohort_name),
            selected = unique(prevalence_estimates$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_start_date_selector",
            label = "Prevalence Start Date",
            choices = as.character(unique(prevalence_estimates$prevalence_start_date)),
            selected = as.character(unique(prevalence_estimates$prevalence_start_date)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_sex_selector",
            label = "Sex",
            choices = unique(prevalence_estimates$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_age_selector",
            label = "Age Group",
            choices = unique(prevalence_estimates$denominator_age_group),
            selected = "18 to 150",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "prevalence_plot_facet",
                        label = "Facet by",
                        choices = c("outcome_cohort_name", 
                                    "denominator_sex",
                                    "cdm_name" ,
                                    "denominator_age_group"),
                        selected = c("outcome_cohort_name"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "prevalence_plot_group",
                        label = "Colour by",
                        choices = c("outcome_cohort_name", 
                                    "denominator_sex",
                                    "cdm_name" ,
                                    "denominator_age_group"
                        ),
                        selected = c("cdm_name"),
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
          plotOutput("prevalencePlot",
                     height = "800px"
          ) %>%
            withSpinner(),
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("prevalence_download_height", "", 30, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("prevalence_download_width", "", 35, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("prevalence_download_dpi", "", 600, width = "50px")
          ),
          downloadButton("prevalence_download_plot", "Download plot")
        )
        
        
      ),
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
            label = "Cohort Name",
            choices = unique(incidence_estimates$outcome_cohort_name),
            selected = unique(incidence_estimates$outcome_cohort_name)[1],
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
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_sex_selector",
            label = "Sex",
            choices = unique(incidence_estimates$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_age_selector",
            label = "Age Group",
            choices = unique(incidence_estimates$denominator_age_group),
            selected = "18 to 150",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_facet",
                        label = "Facet by",
                        choices = c("outcome_cohort_name", 
                                    "denominator_sex",
                                    "cdm_name" ,
                                    "denominator_age_group"),
                        selected = c("outcome_cohort_name"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_group",
                        label = "Colour by",
                        choices = c("outcome_cohort_name", 
                                    "denominator_sex",
                                    "cdm_name" ,
                                    "denominator_age_group"
                                    ),
                        selected = c("cdm_name"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
            
            
        ),
        
        
        
        div(
          style = "width: 80vh; height: 5vh;",  # Set width to 100% for responsive design
          checkboxInput("show_error_bars1", "Show Ribbons", value = TRUE)
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
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_cohort_name_selector_std",
            label = "Cohort Name",
            choices = unique(incidence_estimates_std$outcome_cohort_name),
            selected = unique(incidence_estimates_std$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_start_date_selector_std",
            label = "Incidence Start Date",
            choices = as.character(unique(incidence_estimates_std$incidence_start_date)),
            selected = as.character(unique(incidence_estimates_std$incidence_start_date)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_std_method",
            label = "Standardization Method",
            choices = unique(incidence_estimates_std$age_standard),
            selected = unique(incidence_estimates_std$age_standard)[c(2,3)],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_sex_selector_std",
            label = "Sex",
            choices = unique(incidence_estimates_std$denominator_sex),
            selected = "Both",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),

        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_facet_std",
                        label = "Facet by",
                        choices = c("outcome_cohort_name", 
                                    "denominator_sex",
                                    "cdm_name",
                                    "age_standard"),
                        selected = c("outcome_cohort_name", "age_standard"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "incidence_plot_group_std",
                        label = "Colour by",
                        choices = c("outcome_cohort_name", 
                                    "denominator_sex",
                                    "cdm_name",
                                    "age_standard"
                        ),
                        selected = c("cdm_name"),
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
          plotOutput("incidencePlot_std",
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
        tabName = "cohort_attr_fig",
        tags$h5("Attrition Diagrams for study populations:"),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_database_name_selector1",
            label = "Database",
            choices = unique(incidence_attrition$cdm_name),
            selected = unique(incidence_attrition$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_cohort_name_selector1",
            label = "Cohort Name",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = unique(incidence_attrition$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          grVizOutput("attrition_diagram", width = "400px", height = "100%") %>%
            withSpinner(),
          h4("Download Figure"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("attrition_download_width", "", 600, width = "50px")
          ),
          div("pixels", style = "display: inline-block; margin-right: 25px;"),
          downloadButton("cohort_attrition_download_figure", "Download plot")
        )
        
      ),
      
      tabItem(
        tags$h5("The cohort attrition showing how the final study populations were obtained are presented below:"),
        tabName = "cohort_attrition",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_database_name_selector",
            label = "Database",
            choices = unique(incidence_attrition$cdm_name),
            selected = unique(incidence_attrition$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_cohort_name_selector",
            label = "Cohort Name",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = unique(incidence_attrition$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        htmlOutput('dt_attrition'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_attrition_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
      
      
      tabItem(
        tabName = "survival_results",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_database_selector",
            label = "Database",
            choices = unique(incidence_attrition$cdm_name),
            selected = unique(incidence_attrition$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_cohort_name_selector",
            label = "Cohort Name",
            choices = unique(incidence_attrition$outcome_cohort_name),
            selected = unique(incidence_attrition$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "survival_demo_selector",
            label = "Demographics",
            choices = unique(demo_characteristics$group_level),
            selected = "Overall",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "surv_plot_facet",
                        label = "Facet by",
                        choices = c("group_level",
                                    "strata_level"),
                        selected = c("group_level" ),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "surv_plot_group",
                        label = "Colour by",
                        choices = c("group_level", "strata_level"),
                        selected = c("group_level", "strata_level"),
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


