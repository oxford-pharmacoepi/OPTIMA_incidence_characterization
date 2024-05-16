#### PACKAGES -----
renv::restore()

library(shiny)
library(shinydashboard)
library(shinythemes)
library(readr)
library(here)
library(stringr)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(bslib)
library(PatientProfiles)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(CirceR)
library(rclipboard)
library(CodelistGenerator)
library(CohortSurvival)
library(CohortCharacteristics)
library(rjson)
library(omopgenerics)
library(dplyr)
library(readr)

# install.packages("devtools")
# devtools::install_github("darwin-eu-dev/omopgenerics", force = T)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#605ca8"
  ),
  adminlte_sidebar(
    dark_bg = "#78B7C5", #  "#D8DEE9",
    dark_hover_bg = "#3B9AB2", #"#81A1C1",
    dark_color ="white" ,
    dark_submenu_bg = "#605ca8"
  ),
  adminlte_global(
    content_bg = "#eaebea"
    #content_bg = "white"
  ),
  adminlte_vars(
    border_color = "black",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446",
    table_border_color = "black"

  )
)

# format markdown
formatMarkdown <- function(x) {
  lines <- strsplit(x, "\r\n\r\n") |> unlist()
  getFormat <- function(line) {
    if (grepl("###", line)) {return(h3(gsub("###", "", line)))} 
    else {h4(line)} 
  }
  purrr::map(lines, ~ getFormat(.))
}

# Data prep functions -----
# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# printing numbers with 4 decimal place and commas 
nice.num4<-function(x) {
  trimws(format(round(x,4),
                big.mark=",", nsmall = 4, digits=4, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}



# Load, prepare, and merge results -----
results <-list.files(here("data"), full.names = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = ".zip")

#unzip data
for (i in (1:length(results))) {
  utils::unzip(zipfile = results[[i]],
               exdir = here("data"))
}

#grab the results from the folders
results <- list.files(
  path = here("data"),
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)

# cohort concept code lists -----
# cohort_set <- CDMConnector::read_cohort_set(
# gsub("3_Reporting/", "", here("2_study", "1_InstantiateCohorts", "Cohorts", "incidence") ))

cohort_set <- CDMConnector::read_cohort_set(here::here(
  "www", "Cohorts" , "incidence" ))

cohort_set1 <- CDMConnector::read_cohort_set(here::here(
  "www", "Cohorts" , "prevalence" ))

cohort_set <- bind_rows(cohort_set,
                        cohort_set1)

cohort_set$markdown <- ""

for (n in  row_number(cohort_set) ) {
  
  cohort <- cohort_set$cohort_name[n]  
  json <- paste0(cohort_set$json[n]  )
  cohortExpresion <- CirceR::cohortExpressionFromJson(json)
  markdown <- CirceR::cohortPrintFriendly(cohortExpresion)
  cohort_set$markdown[n] <-  markdown
  
} 


# Get concept ids from a provided path to cohort json files
# in dataframe
# Get a list of JSON files in the directory
json_files <- list.files(path = here("www", "Cohorts", "incidence"), pattern = "\\.json$", full.names = TRUE)
  concept_lists_temp <- list()
  concept_lists <- list()
  concept_sets <- list()
  
if(length(json_files > 0)){
  
  for(i in seq_along(json_files)){
    concept_lists_temp[[i]] <- fromJSON(file = json_files[[i]]) 
    
  } 

  for(i in 1:length(concept_lists_temp)){
    
    for(k in 1:length(concept_lists_temp[[i]]$ConceptSets[[1]]$expression$items)){  
      
      concept_sets[[k]] <- bind_rows(concept_lists_temp[[i]]$ConceptSets[[1]]$expression$items[[k]]$concept)  
      
    }

    concept_lists[[i]] <- bind_rows(concept_sets) %>% 
      mutate(name = concept_lists_temp[[i]]$ConceptSets[[1]]$name)
      
    
  }
  
  
  concept_sets_final <- bind_rows(concept_lists)
  
}

  concept_sets_final <- concept_sets_final %>% 
  mutate(name = ifelse(name == "lung_cancer_broad_inc", "lung_cancer_incident_broad", name)) %>% 
  mutate(name = ifelse(name == "lung_cancer_narrow_inc", "lung_cancer_incident_narrow", name)) 
  
# incidence estimates not standardized -----
incidence_estimates_files <-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files <-results[stringr::str_detect(results, "incidence_estimates")]

if(length(incidence_estimates_files > 0)){
  
incidence_estimates_files <-incidence_estimates_files[!(stringr::str_detect(incidence_estimates_files, "age_std_"))]

incidence_estimates <- list()

for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
  
  
}


incidence_estimates <- dplyr::bind_rows(incidence_estimates) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))


# perform a filter to remove data with no values ie small cell lung cancer
remove_outcomes <- incidence_estimates %>% 
  filter(analysis_interval == "overall") %>% 
  filter(denominator_sex == "Both") %>% 
  filter(denominator_age_group == "18 to 150") %>% 
  group_by(outcome_cohort_name) %>%
  filter(sum(n_events) == 0) %>% 
  distinct(outcome_cohort_name) %>% 
  pull(outcome_cohort_name)

incidence_estimates <- dplyr::bind_rows(incidence_estimates) %>% 
  filter(!(outcome_cohort_name %in% remove_outcomes ))  %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

# age standardized incidence estimates -----
incidence_estimates_files_std<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files_std<-results[stringr::str_detect(results, "incidence_estimates")]
incidence_estimates_files_std<-incidence_estimates_files_std[(stringr::str_detect(incidence_estimates_files_std, "age_std_"))]

incidence_estimates_std <- list()

for(i in seq_along(incidence_estimates_files_std)){
  incidence_estimates_std[[i]]<-readr::read_csv(incidence_estimates_files_std[[i]], 
                                            show_col_types = FALSE)  
}

incidence_estimates_std <- dplyr::bind_rows(incidence_estimates_std) %>% 
  filter(!(outcome_cohort_name %in% remove_outcomes) ) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

# incidence attrition -----
incidence_attrition_files<-results[stringr::str_detect(results, ".csv")]
incidence_attrition_files<-results[stringr::str_detect(results, "incidence_attrition")]
incidence_attrition <- list()

for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}

incidence_attrition <- dplyr::bind_rows(incidence_attrition) %>% 
  filter(!(outcome_cohort_name %in% remove_outcomes ))  %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

# incidence settings ------
incidence_settings_files<-results[stringr::str_detect(results, ".csv")]
incidence_settings_files<-results[stringr::str_detect(results, "incidence_settings")]
incidence_settings <- list()
for(i in seq_along(incidence_settings_files)){
  incidence_settings[[i]]<-readr::read_csv(incidence_settings_files[[i]], 
                                            show_col_types = FALSE)  
}

incidence_settings <- dplyr::bind_rows(incidence_settings) %>% 
  filter(!(outcome_cohort_name %in% remove_outcomes ))  %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

}


# prevalence estimates -----
prevalence_estimates_files <-results[stringr::str_detect(results, ".csv")]
prevalence_estimates_files <-results[stringr::str_detect(results, "prevalence_estimates")]
if(length(prevalence_estimates_files > 0)){
  
  prevalence_estimates <- list()
  
  for(i in seq_along(prevalence_estimates_files)){
    prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                              show_col_types = FALSE)  
  }
  
  prevalence_estimates <- dplyr::bind_rows(prevalence_estimates) %>% 
    mutate(cdm_name = str_replace_all(cdm_name, "_", " "))
    
  
  # prevalence attrition -----
  prevalence_attrition_files<-results[stringr::str_detect(results, ".csv")]
  prevalence_attrition_files<-results[stringr::str_detect(results, "prevalence_attrition")]
  prevalence_attrition <- list()
  for(i in seq_along(prevalence_attrition_files)){
    prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                              show_col_types = FALSE)  
  }
  
  prevalence_attrition <- dplyr::bind_rows(prevalence_attrition) %>% 
    filter(!(outcome_cohort_name %in% remove_outcomes )) %>% 
    mutate(cdm_name = str_replace_all(cdm_name, "_", " "))
  
  # prevalence settings ------
  prevalence_settings_files<-results[stringr::str_detect(results, ".csv")]
  prevalence_settings_files<-results[stringr::str_detect(results, "prevalence_settings")]
  prevalence_settings <- list()
  for(i in seq_along(prevalence_settings_files)){
    prevalence_settings[[i]]<-readr::read_csv(prevalence_settings_files[[i]], 
                                             show_col_types = FALSE)  
  }
  
  prevalence_settings <- dplyr::bind_rows(prevalence_settings)  %>% 
    mutate(cdm_name = str_replace_all(cdm_name, "_", " "))
  
}

# survival estimates1 -------
survival_estimates_files <- results[stringr::str_detect(results, ".csv")]
survival_estimates_files <- results[stringr::str_detect(results, "survival_results_analysis1")]

if(length(survival_estimates_files > 0)){

survival_estimates <- list()
for(i in seq_along(survival_estimates_files)){
  survival_estimates[[i]]<-readr::read_csv(survival_estimates_files[[i]],
                                           show_col_types = FALSE)
}

survival_estimates <- dplyr::bind_rows(survival_estimates) %>% 
  omopgenerics::newSummarisedResult() %>% 
  visOmopResults::splitAll(
    keep = TRUE,
    fill = "overall") %>%
  mutate(time = as.numeric(time)) %>%
  pivot_wider(names_from = estimate_name, values_from = estimate_value) 


# # works
# plotSurvival(survival_estimates)
# 
# 
# 
#   #mutate(cdm_name = str_replace_all(cdm_name, "_", " "))
# 
# # doesnt work
# asd <- tableSurvival(survival_estimates, times = c(365))
  
}

# survival attrition ------
survival_attrition_files <- results[stringr::str_detect(results, ".csv")]
survival_attrition_files <- results[stringr::str_detect(results, "survival_attrition")]

if(length(survival_attrition_files > 0)){
survival_attrition <- list()
for(i in seq_along(survival_attrition_files)){
  survival_attrition[[i]]<-readr::read_csv(survival_attrition_files[[i]],
                                           show_col_types = FALSE)
}
survival_attrition <- dplyr::bind_rows(survival_attrition) %>% 
  filter(!(outcome_cohort_name %in% remove_outcomes ))  %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

}

# survival summaries ------
# survival_median_files <- results[stringr::str_detect(results, ".csv")]
# survival_median_files <- results[stringr::str_detect(results, "survival_summary")]
# 
# if(length(survival_median_files > 0)){
#   
# survival_median_table <- list()
# 
# for(i in seq_along(survival_median_files)){
#   survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
#                                               show_col_types = FALSE)  
# }
# survival_median_table <- dplyr::bind_rows(survival_median_table) %>% 
#   mutate(estimate_value = as.character(estimate_value)) %>% 
#   filter(estimate_name == "number_records" |
#            estimate_name == "events" |
#            estimate_name == "median_survival" |
#          estimate_name == "median_survival_95CI_lower" |
#          estimate_name == "median_survival_95CI_higher" )  %>% 
#   mutate(result_type = "summarised_characteristics") %>% 
#   mutate(group_name = "cohort_name") %>% 
#   mutate(strata_name = case_when(
#     strata_name == "Overall" ~ "overall",
#              TRUE ~ strata_name
#            )) %>% 
#   mutate(strata_level = case_when(
#     strata_level == "Overall" ~ "overall",
#     TRUE ~ strata_level
#   )) %>% 
#   
#   mutate(estimate_type = case_when(
#     estimate_name == "number_records" ~ "integer",
#     estimate_name == "events" ~ "integer",
#     estimate_name == "median_survival" ~ "integer",
#     estimate_name == "median_survival_95CI_lower" ~ "integer",
#     estimate_name == "median_survival_95CI_higher" ~ "integer",
#     
#     
#     TRUE ~ estimate_type
#   )) %>% 
#   pivot_wider(names_from = estimate_name, values_from = estimate_value) %>% 
#   select(c("cdm_name",
#            "group_level",
#            "strata_level",
#            "median_survival"       ,
#            "median_survival_95CI_lower" ,
#            "median_survival_95CI_higher"
#            )) %>% 
#   filter(!(group_level %in% remove_outcomes ))  %>% 
#   mutate(cdm_name = str_replace_all(cdm_name, "_", " "))
#   
# 
# }

# table one demographics------
tableone_demo_files <- results[stringr::str_detect(results, ".csv")]
tableone_demo_files <- results[stringr::str_detect(results, "demographics")]

if(length(tableone_demo_files > 0)){

tableone_demo <- list()
settings_demo <- list()

for(i in seq_along(tableone_demo_files)){
  #read in the files
  tableone_demo[[i]] <- readr::read_csv(tableone_demo_files[[i]],
                                                 show_col_types = FALSE)
  
  
  settings_demo[[i]] <- tableone_demo[[i]] %>%
    dplyr::filter(variable_name == "settings")
  
  # remove from the summarised results
  tableone_demo[[i]] <- tableone_demo[[i]] %>%
    dplyr::filter(variable_name != "settings")
  
  #turn back into a summarised result
  
  tableone_demo[[i]] <- tableone_demo[[i]] %>%
    omopgenerics::newSummarisedResult(
      settings = tibble(
        result_id = 1L,
        result_type = settings_demo[[i]]$estimate_value[3],
        package_name = settings_demo[[i]]$estimate_value[1],
        package_version = settings_demo[[i]]$estimate_value[2],
        value = 5)
    )


}

}

demo_characteristics <- Reduce(omopgenerics::bind, tableone_demo) %>%
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

rm(tableone_demo)

# table one medications ------
tableone_med_files <- results[stringr::str_detect(results, ".csv")]
tableone_med_files <- results[stringr::str_detect(results, "medications")]

if(length(tableone_med_files > 0)){
  
  tableone_med <- list()
  settings_med <- list()
  
  for(i in seq_along(tableone_med_files)){
    #read in the files
    tableone_med[[i]] <- readr::read_csv(tableone_med_files[[i]],
                                          show_col_types = FALSE)
    
    
    settings_med[[i]] <- tableone_med[[i]] %>%
      dplyr::filter(variable_name == "settings")
    
    # remove from the summarised results
    tableone_med[[i]] <- tableone_med[[i]] %>%
      dplyr::filter(variable_name != "settings")
    
    #turn back into a summarised result
    
    tableone_med[[i]] <- tableone_med[[i]] %>%
      omopgenerics::newSummarisedResult(
        settings = tibble(
          result_id = 1L,
          result_type = settings_demo[[i]]$estimate_value[3],
          package_name = settings_demo[[i]]$estimate_value[1],
          package_version = settings_demo[[i]]$estimate_value[2],
          value = 5)
      )

  }
  
}

med_characteristics <- Reduce(omopgenerics::bind, tableone_med) %>%
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

rm(tableone_med)

# table one comorbidities ------
tableone_comorb_files <- results[stringr::str_detect(results, ".csv")]
tableone_comorb_files <- results[stringr::str_detect(results, "comorbidity")]

if(length(tableone_comorb_files > 0)){
  
  tableone_comorb <- list()
  settings_comorb <- list()
  
  for(i in seq_along(tableone_comorb_files)){
    #read in the files
    tableone_comorb[[i]] <- readr::read_csv(tableone_comorb_files[[i]],
                                         show_col_types = FALSE)
    
    
    settings_comorb[[i]] <- tableone_comorb[[i]] %>%
      dplyr::filter(variable_name == "settings")
    
    # remove from the summarised results
    tableone_comorb[[i]] <- tableone_comorb[[i]] %>%
      dplyr::filter(variable_name != "settings")
    
    #turn back into a summarised result
    tableone_comorb[[i]] <- tableone_comorb[[i]] %>%
      omopgenerics::newSummarisedResult(
        settings = tibble(
          result_id = 1L,
          result_type = settings_comorb[[i]]$estimate_value[3],
          package_name = settings_comorb[[i]]$estimate_value[1],
          package_version = settings_comorb[[i]]$estimate_value[2],
          value = 5)
      )

    
  }
  
}


comorb_characteristics <- Reduce(omopgenerics::bind, tableone_comorb) %>%
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

rm(tableone_comorb)

# risk tables ----------
survival_risk_table_files<-results[stringr::str_detect(results, ".csv")]
survival_risk_table_files<-results[stringr::str_detect(results, "risk_table_results")]

if(length(survival_risk_table_files > 0)){

survival_risk_table <- list()
for(i in seq_along(survival_risk_table_files)){
  survival_risk_table[[i]]<-readr::read_csv(survival_risk_table_files[[i]],
                                            show_col_types = FALSE) %>%
    mutate_if(is.double, as.character)

}

survival_risk_table <- dplyr::bind_rows(survival_risk_table)  %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

}

# cdm snapshot ------
snapshot_files <- results[stringr::str_detect(results, ".csv")]
snapshot_files <- results[stringr::str_detect(results, "cdm_snapshot")]
snapshotcdm <- list()
for(i in seq_along(snapshot_files)){
  snapshotcdm[[i]] <- readr::read_csv(snapshot_files[[i]],
                                      show_col_types = FALSE) %>% 
    mutate_all(as.character)
  
}
snapshotcdm <- bind_rows(snapshotcdm) %>% 
  select("cdm_name", "person_count", "observation_period_count" , "start_date",
         "vocabulary_version", "cdm_version", "cdm_description",) %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
  rename("Database name" = "cdm_name",
         "Study Start Date" = "start_date",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description" )  


# attrition functions ----
attritionChart <- function(x) {
  formatNum <- function(col) {
    col <- round(as.numeric(col))
    if_else(
      !is.na(col),
      gsub(" ", "", format(as.integer(col), big.mark=",")),
      as.character(col)
    )
  }
  
  xn <- x %>%
    arrange(reason_id) %>%
    mutate(
      number_subjects = formatNum(number_subjects),
      number_records = formatNum(number_records),
      excluded_subjects = formatNum(excluded_subjects),
      excluded_records = formatNum(excluded_records),
      label = paste0(
        "N subjects = ", number_subjects, "\nN records = ", number_records
      )
    )
  if (nrow(xn) == 1) {
    xn <- xn %>%
      mutate(label = paste0("Qualifying events", "\n", label)) %>%
      select(label)
  } else {
    att <- xn %>%
      filter(reason_id > min(reason_id)) %>%
      mutate(
        label = paste0(
          "N subjects = ", excluded_subjects, "\nN records = ", excluded_records
        )
      ) %>%
      select(reason, label)
    xn <- xn %>%
      mutate(
        label = if_else(
          reason_id == min(reason_id),
          paste0("Initial events", "\n", label),
          if_else(
            reason_id == max(reason_id),
            paste0("Final events", "\n", label),
            label
          )
        )
      ) %>%
      select(label)
  }
  n <- nrow(x)
  xg <- create_graph()
  
  for (k in seq_len(n)) {
    xg <- xg %>%
      add_node(
        label = xn$label[k],
        node_aes = node_aes(
          shape = "box",
          x = 1,
          width = 1.4,
          y = n + 1 - k + ifelse(k == 1, 0.1, 0) + ifelse(k == n, -0.1, 0),
          height = ifelse(k == 1 | k == n, 0.6, 0.4),
          fontsize = 10, fontcolor = "black", penwidth = ifelse(k == 1 | k == n, 2, 1), color = "black"
        )
      )
    if (k > 1) {
      xg <- xg %>%
        add_edge(from = k - 1, to = k, edge_aes = edge_aes(color = "black"))
    }
  }
  salt <- function(x) {
    s <- 50
    x <- strsplit(x = x, split = " ") |> unlist()
    nn <- (nchar(x) + c(0, rep(1, length(x)-1))) |> cumsum()
    id <- which(nn > s)
    if (length(id) > 0) {
      id <- id[1] - 1
      x <- paste0(paste0(x[1:id], collapse = " "), "\n", paste0(x[-(1:id)], collapse = " "))
    } else {
      x <- paste0(x, collapse = " ")
    }
    return(x)
  }
  if (n > 1) {
    for (k in seq_len(nrow(att))) {
      res <- att$reason[k]
      res <- salt(res)
      xg <- xg %>%
        add_node(
          label = att$label[k],
          node_aes = node_aes(
            shape = "box", x = 3.5, width = 1.2, y = n + 0.5 - k, height = 0.4,
            fontsize = 8, fillcolor = "grey", fontcolor = "black", color = "black"
          )
        ) %>%
        add_node(
          label = res,
          node_aes = node_aes(
            shape = "box", x = 1, width = 3.2, y = n + 0.5 - k, height = 0.35, fillcolor = "white", color = "black", fontcolor = "back"
          )
        ) %>%
        add_edge(
          from = 2*k + n, to = 2*k + n -1, edge_aes = edge_aes(color = "black")
        )
    }
  }
  
  return(xg)
}