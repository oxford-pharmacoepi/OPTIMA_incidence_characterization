# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Databases",
        tabName = "dbs",
        menuSubItem(
          text = "Database details",
          tabName = "cdm_snapshot"
        )
      ),
      menuItem(
        text = "Cohorts",
        tabName = "cohorts",
        menuSubItem(
          text = "Cohort attrition",
          tabName = "cohort_attrition"
        )
      ),
      menuItem(
        text = "Characteristics",
        tabName = "chars",
        menuSubItem(
          text = "Demographics",
          tabName = "dems"
        ),
        menuSubItem(
          text = "Comorbidities",
          tabName = "cmorbs"
        ),
        menuSubItem(
          text = "Medications",
          tabName = "meds"
        ),
  menuItem(
    text = "Survival",
    tabName = "Survival",
    menuSubItem(
      text = "Survival",
      tabName = "Survival"
    )
  )
)
),

  ## body ----
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "background",
        h3("Lung Cancer: Incidence, survival and patient characterisation in the period 2000-2023"),
        h5("This app is a companion to a OPTIMA(R) study on lung cancer. 
           For more details see ",
           tags$a(href="https://www.encepp.eu/encepp/viewResource.htm?id=106363", 
                  "Here",
                  target="_blank")
           ),
        a(img(src="logo.png", align = "right",
              height="3%", width="30%"), href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
          target="_blank")
      ),
      tabItem(
        tabName = "cdm_snapshot",
        htmlOutput('tbl_cdm_snaphot'),
        tags$hr(),
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_cdm_snaphot_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
      ),
      tabItem(
        tabName = "cohort_attrition",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attr_cdm",
            label = "Database",
            choices = colnames(cohort_attrition)[3:8],
            selected = colnames(cohort_attrition)[3:8],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attr_cohort_name",
            label = "Study cohort",
            choices = unique(cohort_attrition$cohort_name),
            selected = "Multiple myeloma",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        DT::dataTableOutput(outputId = "dt_cohort_attrition"),
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_cohort_attrition_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
      ),
      tabItem(
        tabName = "dems",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "dems_cdm",
            label = "Database",
            choices = unique(results_summaryDemographics$cdm_name),
            selected = unique(results_summaryDemographics$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE, 
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "dems_cohort_name",
            label = "Study cohort",
            choices = unique(results_summaryDemographics$group_level),
            selected ="Multiple myeloma",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        gt_output("gt_summaryDemographics") %>% withSpinner(),
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_summaryDemographics_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      ),
      
   # cmorbs   ------
      tabItem(
        tabName = "cmorbs",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cmorbs_cdm",
            label = "Database",
            choices = unique(results_summaryComorbidity$cdm_name),
            selected = unique(results_summaryComorbidity$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cmorbs_cohort_name",
            label = "Study cohort",
            choices = unique(results_summaryComorbidity$cohort_name),
            selected = unique(results_summaryComorbidity$cohort_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cmorbs_time_window",
            label = "Time window",
            choices = levels(results_summaryComorbidity$time_window),
            selected = levels(results_summaryComorbidity$time_window)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cmorbs_strata_level",
            label = "Strata",
            choices = unique(results_summaryComorbidity$strata_level),
            selected = unique(results_summaryComorbidity$strata_level),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cmorbs_variable_level",
            label = "Variable",
            choices = sort(unique(results_summaryComorbidity$variable_level)),
            selected = sort(unique(results_summaryComorbidity$variable_level)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        DT::dataTableOutput("dt_summaryComorbidity") %>% withSpinner(),
        div(style="display:inline-block",
            downloadButton(
              outputId = "dt_summaryComorbidity_csv",
              label = "Download csv"
            ), 
            style="display:inline-block; float:right")
      ),
   
   # surv -----
   tabItem(
     tabName = "Survival",
     div(
       style = "display: inline-block;vertical-align:top; width: 150px;",
       pickerInput(
         inputId = "surv_cdm",
         label = "Database",
         choices = unique(results_survival$cdm_name[!is.na(results_survival$strata_level)]),
         selected = unique(results_survival$cdm_name[!is.na(results_survival$strata_level)]),
         options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
         multiple = TRUE
       )
     ),
     div(
       style = "display: inline-block;vertical-align:top; width: 150px;",
       pickerInput(
         inputId = "surv_strata_level",
         label = "Strata",
         choices = c("Overall","Female","Male", "0 to 17", "18 to 44","45 to 59","60 to 69",">=70","2017 or earlier", "2018 or later")  ,
         selected = "Overall",
         options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
         multiple = TRUE
       )
     ),
     tabsetPanel(
       type = "tabs",
       tabPanel(
         "Survival summary",
     DT::dataTableOutput("dt_surv") %>% withSpinner(),
     tags$h5("Note, median survival is not calculated for groups where survival was more than 50% in the group at the last time point.")
     
       ),
     
     tabPanel(
       "Survival plot",
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "surv_facet",
           label = "Facet by",
           choices = c("none", "cdm_name", "strata_level", "strata_name"),
           selected = c("cdm_name","strata_name"),
           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = TRUE
         )
       ),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "surv_colour",
           label = "Colour by",
           choices = c("none", "cdm_name", "strata_level", "strata_name"),
           selected = "strata_level",
           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = FALSE
         )
       ),
       checkboxInput("surv_cumulative", "Cumulatice failure rate", FALSE),
       plotOutput("surv_plot") %>% withSpinner(),h4("Download figure"),
       div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
       div(
         style = "display: inline-block;",
         textInput("surv_download_height", "", 10, width = "50px")
       ),
       div("cm", style = "display: inline-block; margin-right: 25px;"),
       div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
       div(
         style = "display: inline-block;",
         textInput("surv_download_width", "", 20, width = "50px")
       ),
       downloadButton("surv_download_plot", "Download plot")
     ))

    )
  )
)


