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
        )


      ),
     
      menuItem(
        text = "Table One Characteristics",
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
        text = "Large Scale Characteristics",
        tabName = "lsc",
        icon = shiny::icon("scale-balanced") ,
        
        
        menuSubItem(
          text = "Lsc",
          tabName = "lsc"
        ),
        
        menuSubItem(
          text = "Matched Lsc",
          tabName = "lsc_estimates"
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
    ),
    
    # Logo 
    tags$div(
      style = "position: relative; margin-top: -20px; text-align: center; margin-bottom: 0;",
      a(img(
        src = "optima.png",  # Replace with the correct file name and extension
        height = "auto",  # Adjust the height as needed
        width = "80%"     # Let the width adjust proportionally
      ),
      href = "https://www.optima-oncology.eu/",
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
          "We performed a population-based cohort study using the real world databases from primary care and claims. The study period was from January 1, 2003 (or the earliest available data) until database exit, death, or the end of the study on 31st December, 2022. Participants aged 18+ years, with a diagnoses of primary lung cancer, with one-year of prior data availability, were included. We estimated overall lung cancer incidence rates (IR) and stratified by sex and age groups using the",
          tags$a(href="https://darwin-eu.github.io/IncidencePrevalence/", "IncidencePrevalence R package"),
 "Crude IRs were age standardized using European and World Standard populations. Comorbidities and medication usage for lung cancer patients were characterised before, on and after diagnosis. Large scale characteristics of lung cancer patients was performed using age and sex matched controls."
          
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
                tags$a(href="https://github.com/oxford-pharmacoepi/OPTIMA_incidence_characterization", "here"),
                ". The cohort diagnostics for lung cancer phenotypes can be found",
                tags$a(href="https://dpa-pde-oxford.shinyapps.io/PhenotypeR_OPTIMA_lung_cancer/", "here")
                
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
            choices = if (exists("incidence_attrition") && !is.null(incidence_attrition$Outcome_cohort_name)) {
              unique(incidence_attrition$Outcome_cohort_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_attrition") && !is.null(incidence_attrition$Outcome_cohort_name)) {
              unique(incidence_attrition$Outcome_cohort_name)[1]
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_sex_selector",
            label = "Sex",
            
            choices = if (exists("incidence_attrition") && !is.null(incidence_attrition$Denominator_sex)) {
              unique(incidence_attrition$Denominator_sex)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_attrition") && !is.null(incidence_attrition$Denominator_sex)) {
              "Both"
            } else {
              "No data available"
            },
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_age_selector",
            label = "Age Group",

            choices = if (exists("incidence_attrition") && !is.null(incidence_attrition$Denominator_age_group)) {
              unique(incidence_attrition$Denominator_age_group)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_attrition") && !is.null(incidence_attrition$Denominator_age_group)) {
              unique(incidence_attrition$Denominator_age_group)[1]
            } else {
              "No data available"
            },
            
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_database_name_selector",
            label = "Database",

            
            
            choices = if (exists("incidence_attrition") && !is.null(incidence_attrition$CDM_name)) {
              unique(incidence_attrition$CDM_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_attrition") && !is.null(incidence_attrition$CDM_name)) {
              unique(incidence_attrition$CDM_name)[1]
            } else {
              "No data available"
            },
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        DT::dataTableOutput('tbl_incidence_attrition'),
        
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
            label = "Cohort Name",
            choices = demo_characteristics %>%
              mutate(group_level = str_remove(group_level, " Matched$")) %>%
              mutate(group_level = str_remove(group_level, " Sampled$")) %>%
              distinct(group_level) %>%
              pull(group_level),
            selected = demo_characteristics %>%
              mutate(group_level = str_remove(group_level, " Matched$")) %>%
              mutate(group_level = str_remove(group_level, " Sampled$")) %>%
              distinct(group_level) %>%
              pull(group_level) %>% 
              first(),
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          ),
    
          shinyWidgets::prettyCheckbox(
            inputId = "summarise_characteristics_include_matched", 
            label = "Show matched cohorts", 
            value = FALSE)
          
          
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
            choices = demo_characteristics %>%
              filter(strata_name == "sex" | strata_name == "overall") %>%
              distinct(strata_level) %>% 
              pull(),
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
            choices = demo_characteristics %>%
              filter(strata_name == "age_group" | strata_name == "overall") %>%
              distinct(strata_level) %>% 
              pull(),
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

            choices = comorb_characteristics %>%
              mutate(group_level = str_remove(group_level, " Matched$")) %>%
              mutate(group_level = str_remove(group_level, " Sampled$")) %>%
              distinct(group_level) %>%
              pull(group_level),
            selected = comorb_characteristics %>%
              mutate(group_level = str_remove(group_level, " Matched$")) %>%
              mutate(group_level = str_remove(group_level, " Sampled$")) %>%
              distinct(group_level) %>%
              pull(group_level) %>% 
              first(),
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          ),
          
          shinyWidgets::prettyCheckbox(
            inputId = "summarise_characteristics_include_matched1", 
            label = "Show matched cohorts", 
            value = FALSE)
          
          
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
            choices = comorb_characteristics %>%
              filter(strata_name == "sex" | strata_name == "overall") %>%
              distinct(strata_level) %>% 
              pull(),
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
            choices = comorb_characteristics %>%
              filter(strata_name == "age_group" | strata_name == "overall") %>%
              distinct(strata_level) %>% 
              pull(),
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
              visOmopResults::splitAdditional() %>% 
              filter(window != "overall") %>%
              pull(window) %>%
              unique(),
            selected = "-Inf to -1",
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
            choices = med_characteristics %>%
              mutate(group_level = str_remove(group_level, " Matched$")) %>%
              mutate(group_level = str_remove(group_level, " Sampled$")) %>%
              distinct(group_level) %>%
              pull(group_level),
            selected = med_characteristics %>%
              mutate(group_level = str_remove(group_level, " Matched$")) %>%
              mutate(group_level = str_remove(group_level, " Sampled$")) %>%
              distinct(group_level) %>%
              pull(group_level) %>% 
              first(),

            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          ),
          
          shinyWidgets::prettyCheckbox(
            inputId = "summarise_characteristics_include_matched2", 
            label = "Show matched cohorts", 
            value = FALSE)
          
          
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
            choices = med_characteristics %>%
              filter(strata_name == "sex" | strata_name == "overall") %>%
              distinct(strata_level) %>% 
              pull(),
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
            choices = comorb_characteristics %>%
              filter(strata_name == "age_group" | strata_name == "overall") %>%
              distinct(strata_level) %>% 
              pull(),
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
              visOmopResults::splitAdditional() %>% 
              filter(window != "overall") %>%
              pull(window) %>%
              unique(),
            selected = "-365 to -1",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
          
        ),
        
        gt_output("gt_med_characteristics") %>% 
          withSpinner() ,
        
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_med_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      ) ,
 
 
 
 tabItem(
   tabName = "lsc_estimates",
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_cohort_selector",
       label = "Cohort Name",
       choices = lsc_characteristics %>%
         mutate(group_level = str_remove(group_level, " Matched$")) %>%
         mutate(group_level = str_remove(group_level, " Sampled$")) %>%
         distinct(group_level) %>%
         pull(group_level),
       selected = lsc_characteristics %>%
         mutate(group_level = str_remove(group_level, " Matched$")) %>%
         mutate(group_level = str_remove(group_level, " Sampled$")) %>%
         distinct(group_level) %>%
         pull(group_level) %>% 
         first(),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_database_name_selector",
       label = "Database",
       choices = unique(lsc_characteristics$cdm_name),
       selected = unique(lsc_characteristics$cdm_name),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
  
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_time_selector",
       label = "Time",
       choices = 
         lsc_characteristics %>% 
         filter(!(variable_level %in% c("Female", "Male", NA))) %>% 
         pull(variable_level) %>% 
         unique(),
       selected = "-inf to -366",
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   
   
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_domain_selector",
       label = "Domain",
       choices = c("condition_occurrence", 
                   "drug_era", 
                   "measurement",
                   "procedure_occurrence",
                   "visit_occurrence",
                   "observation"),
       selected = "condition_occurrence",
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   

   DT::dataTableOutput('gt_lsc_characteristics'),
   
   div(style="display:inline-block",
       downloadButton(
         outputId = "gt_lsc_characteristics_word",
         label = "Download table as word"
       ),
       style="display:inline-block; float:right")
   
 ) ,
 
 
 tabItem(
   tabName = "lsc",
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_cohort_selector1",
       label = "Cohort Name",
       choices = lsc_characteristics %>%
         mutate(group_level = str_remove(group_level, " Matched$")) %>%
         mutate(group_level = str_remove(group_level, " Sampled$")) %>%
         distinct(group_level) %>%
         pull(group_level),
       selected = lsc_characteristics %>%
         mutate(group_level = str_remove(group_level, " Matched$")) %>%
         mutate(group_level = str_remove(group_level, " Sampled$")) %>%
         distinct(group_level) %>%
         pull(group_level) %>% 
         first(),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_database_name_selector1",
       label = "Database",
       choices = unique(lsc_characteristics$cdm_name),
       selected = unique(lsc_characteristics$cdm_name),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_time_selector1",
       label = "Time",
       choices = 
         lsc_characteristics %>% 
         filter(!(variable_level %in% c("Female", "Male", NA))) %>% 
         pull(variable_level) %>% 
         unique(),
       selected = "-inf to -366",
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   
   
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "lsc_domain_selector1",
       label = "Domain",
       choices = c("condition_occurrence", 
                   "drug_era", 
                   "measurement",
                   "procedure_occurrence",
                   "visit_occurrence",
                   "observation"),
       selected = "condition_occurrence",
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   

   DT::dataTableOutput('gt_lsc_characteristics1'),
   
   div(style="display:inline-block",
       downloadButton(
         outputId = "gt_lsc_characteristics_word1",
         label = "Download table as word"
       ),
       style="display:inline-block; float:right")
   
 ) ,
 
 
 # tabItem(
 #   tabName = "lsc",
 #   div(
 #     style = "display: inline-block;vertical-align:top; width: 150px;",
 #     pickerInput(
 #       inputId = "lsco_cohort_selector",
 #       label = "Cohort Name",
 #       choices = lsc_characteristics_original %>%
 #         visOmopResults::splitAll() %>% 
 #         distinct(cohort_name) %>% 
 #         pull(),
 #       selected = lsc_characteristics_original %>%
 #         visOmopResults::splitAll() %>% 
 #         distinct(cohort_name) %>% 
 #         pull() %>% 
 #         first(),
 #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
 #       multiple = TRUE
 #     )
 #   ),
 #   
 #   div(
 #     style = "display: inline-block;vertical-align:top; width: 150px;",
 #     pickerInput(
 #       inputId = "lsco_database_name_selector",
 #       label = "Database",
 #       choices = unique(lsc_characteristics_original$cdm_name),
 #       selected = unique(lsc_characteristics_original$cdm_name),
 #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
 #       multiple = TRUE
 #     )
 #   ),
 #   
 #   div(
 #     style = "display: inline-block;vertical-align:top; width: 150px;",
 #     pickerInput(
 #       inputId = "lsco_time_selector",
 #       label = "Time",
 #       choices = 
 #         lsc_characteristics_original %>% 
 #         filter(!(variable_level %in% c("Female", "Male", NA))) %>% 
 #         pull(variable_level) %>% 
 #         unique(),
 #       selected = "-inf to -366",
 #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
 #       multiple = TRUE
 #     )
 #   ),
 #   
 #   
 #   div(
 #     style = "display: inline-block;vertical-align:top; width: 150px;",
 #     pickerInput(
 #       inputId = "lsco_domain_selector",
 #       label = "Domain",
 #       choices = c("condition_occurrence", 
 #                   "drug_exposure", 
 #                   "measurement",
 #                   "procedure_occurrence",
 #                   "visit_occurrence",
 #                   "observation"),
 #       selected = "condition_occurrence",
 #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
 #       multiple = TRUE
 #     )
 #   ),
 #   
 #   
 #   DT::dataTableOutput('gt_lsco_characteristics'),
 #   
 #   div(style="display:inline-block",
 #       downloadButton(
 #         outputId = "gt_lsco_characteristics_word",
 #         label = "Download table as word"
 #       ),
 #       style="display:inline-block; float:right")
 #   
 # ) ,
 
 
 
 
 
 
 
        
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
      
      tabItem(
        tabName = "inc_rates",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_cohort_selector",
            label = "Cohort Name",

            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$outcome_cohort_name)) {
              unique(incidence_estimates$outcome_cohort_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$outcome_cohort_name)) {
              unique(incidence_estimates$outcome_cohort_name)[1]
            } else {
              "No data available"
            },
            
            
            
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_analysis_selector",
            label = "Analysis Interval",
            # choices = unique(incidence_estimates$analysis_interval),
            # selected = "years",
            
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$analysis_interval)) {
              unique(incidence_estimates$analysis_interval)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$analysis_interval)) {
              "years"
            } else {
              "No data available"
            },
            
            
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_database_selector",
            label = "Database",
            # choices = unique(incidence_estimates$cdm_name),
            # selected = unique(incidence_estimates$cdm_name)[1],
            
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$cdm_name)) {
              unique(incidence_estimates$cdm_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$cdm_name)) {
              unique(incidence_estimates$cdm_name)[1]
            } else {
              "No data available"
            },
            
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_sex_selector",
            label = "Sex",
            # choices = unique(incidence_estimates$denominator_sex),
            # selected = "Both",
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_sex)) {
              unique(incidence_estimates$denominator_sex)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_sex)) {
              "Both"
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_est_age_selector",
            label = "Age Group",
            # choices = unique(incidence_estimates$denominator_age_group),
            # selected = "18 to 150",
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_age_group)) {
              unique(incidence_estimates$denominator_age_group)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_age_group)) {
              "18 to 150"
            } else {
              "No data available"
            },
            
            
            
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
            # choices = unique(incidence_estimates_std$outcome_cohort_name),
            # selected = unique(incidence_estimates_std$outcome_cohort_name)[1],
            
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$outcome_cohort_name)) {
              unique(incidence_estimates_std$outcome_cohort_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$outcome_cohort_name)) {
              unique(incidence_estimates_std$outcome_cohort_name)[1]
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_sex_selector_std",
            label = "Sex",
            # choices = unique(incidence_estimates_std$denominator_sex),
            # selected = "Both",
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$denominator_sex)) {
              unique(incidence_estimates_std$denominator_sex)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$denominator_sex)) {
              "Both"
            } else {
              "No data available"
            },
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "inc_estimates_database_selector_std",
            label = "Database",
            # choices = unique(incidence_estimates_std$cdm_name),
            # selected = unique(incidence_estimates_std$cdm_name)[1],
            
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$cdm_name)) {
              unique(incidence_estimates_std$cdm_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$cdm_name)) {
              unique(incidence_estimates_std$cdm_name)[1]
            } else {
              "No data available"
            },
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
        tabName = "inc_plots",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_database_selector",
            label = "Database",
            # choices = unique(incidence_estimates$cdm_name),
            # selected = unique(incidence_estimates$cdm_name),
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$cdm_name)) {
              unique(incidence_estimates_std$cdm_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$cdm_name)) {
              unique(incidence_estimates$cdm_name)[1]
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_cohort_name_selector",
            label = "Cohort Name",
            # choices = unique(incidence_estimates$outcome_cohort_name),
            # selected = unique(incidence_estimates$outcome_cohort_name)[1],
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$outcome_cohort_name)) {
              unique(incidence_estimates$outcome_cohort_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$outcome_cohort_name)) {
              unique(incidence_estimates$outcome_cohort_name)[1]
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_denominator_analysis_interval_selector",
            label = "Analysis Interval",
            # choices = unique(incidence_estimates$analysis_interval),
            # selected = "years",
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$analysis_interval)) {
              unique(incidence_estimates$analysis_interval)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$analysis_interval)) {
              "years"
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),

        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_start_date_selector",
            label = "Incidence Start Date",
            # choices = as.character(unique(incidence_estimates_std$incidence_start_date)) ,
            # selected = as.character(unique(incidence_estimates_std$incidence_start_date)) ,
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$incidence_start_date)) {
              as.character(unique(incidence_estimates_std$incidence_start_date))
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$incidence_start_date)) {
              as.character(unique(incidence_estimates_std$incidence_start_date))
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_sex_selector",
            label = "Sex",
            # choices = unique(incidence_estimates$denominator_sex),
            # selected = "Both",
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_sex)) {
              unique(incidence_estimates_std$denominator_sex)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_sex)) {
              "Both"
            } else {
              "No data available"
            },
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_age_selector",
            label = "Age Group",
            # choices = unique(incidence_estimates$denominator_age_group),
            # selected = "18 to 150",
            
            choices = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_age_group)) {
              unique(incidence_estimates_std$denominator_age_group)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates") && !is.null(incidence_estimates$denominator_age_group)) {
              "18 to 150"
            } else {
              "No data available"
            },
            
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
        tags$h5("In order to compare results across different data sources with different age population structures we have age standardized incidence rates to 1) European Standard Population 2013 and 2) World Standard Population (WHO 2000-2025).") ,
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_database_selector_std",
            label = "Database",
            # choices = unique(incidence_estimates_std$cdm_name),
            # selected = unique(incidence_estimates_std$cdm_name),
            
            
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$cdm_name)) {
              unique(incidence_estimates_std$cdm_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$cdm_name)) {
              unique(incidence_estimates_std$cdm_name)[1]
            } else {
              "No data available"
            },
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_cohort_name_selector_std",
            label = "Cohort Name",
            # choices = unique(incidence_estimates_std$outcome_cohort_name),
            # selected = unique(incidence_estimates_std$outcome_cohort_name)[1],
            
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$outcome_cohort_name)) {
              unique(incidence_estimates_std$outcome_cohort_name)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$outcome_cohort_name)) {
              unique(incidence_estimates_std$outcome_cohort_name)[1]
            } else {
              "No data available"
            },
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_start_date_selector_std",
            label = "Incidence Start Date",
            # choices = as.character(unique(incidence_estimates_std$incidence_start_date)),
            # selected = as.character(unique(incidence_estimates_std$incidence_start_date)),
            
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$incidence_start_date)) {
              as.character(unique(incidence_estimates_std$incidence_start_date))
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$incidence_start_date)) {
              as.character(unique(incidence_estimates_std$incidence_start_date))
            } else {
              "No data available"
            },
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_std_method",
            label = "Standardization Method",
            # choices = unique(incidence_estimates_std$age_standard),
            # selected = unique(incidence_estimates_std$age_standard)[c(2,3)],
            
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$age_standard)) {
              unique(incidence_estimates_std$age_standard)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$age_standard)) {
              unique(incidence_estimates_std$age_standard)[c(2,3)]
            } else {
              "No data available"
            },
            
            
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_sex_selector_std",
            label = "Sex",
            # choices = unique(incidence_estimates_std$denominator_sex),
            # selected = "Both",
            
            choices = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$denominator_sex)) {
              unique(incidence_estimates_std$denominator_sex)
            } else {
              c("No data available")
            },
            selected = if (exists("incidence_estimates_std") && !is.null(incidence_estimates_std$denominator_sex)) {
              "Both"
            } else {
              "No data available"
            },
            
            
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
                        selected = c("age_standard"),
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
            textInput("incidence_download_widthstd", "", 55, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("incidence_download_dpistd", "", 600, width = "50px")
          ),
          downloadButton("incidence_download_plot_std", "Download plot")
        )
        
        
      )
      

      
      # more tabs here
    )
    
  )  
  
  
)


