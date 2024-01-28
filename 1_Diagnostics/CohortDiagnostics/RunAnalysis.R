# Get cohort details -----
cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "Cohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
working.json<-here("1_InstantiateCohorts", "Cohorts",
                      cohortJsonFiles[i])
cohortJson <- readChar(working.json, file.info(working.json)$size)
cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
sql <- buildCohortQuery(cohortExpression, 
                   options = CirceR::createGenerateOptions(generateStats = TRUE))

cohortDefinitionSet[[i]]<-tibble(atlasId = i,
      cohortId = i,
      cohortName = str_replace(cohortJsonFiles[i],".json",""),
      json=cohortJson,
      sql=sql,
      logicDescription = NA,
      generateStats=TRUE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)

# Names of tables to be created during study run ----- 
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableStem)

# Create the tables in the database -----
CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_database_schema)

# Generate the cohort set -----
CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

# get stats  -----
CohortGenerator::exportCohortStatsTables(
    connectionDetails = connectionDetails,
    connection = NULL,
    cohortDatabaseSchema = results_database_schema,
    cohortTableNames = cohortTableNames,
    cohortStatisticsFolder = here("Results"),
    incremental = FALSE)

# cohort diagnostics
executeDiagnostics(cohortDefinitionSet,
                   connectionDetails = connectionDetails,
                   cohortTable = cohortTableStem,
                   cohortDatabaseSchema = results_database_schema,
                   cdmDatabaseSchema = cdm_database_schema,
                   exportFolder = here("Results"),
                   databaseId = db.name,
                   minCellCount = 5,
                   runInclusionStatistics = FALSE, 
                   runOrphanConcepts = TRUE,
                   runTimeDistributions = FALSE, 
                   runVisitContext = FALSE,
                   runBreakdownIndexEvents = TRUE, 
                   runIncidenceRate = FALSE, 
                   runTimeSeries = FALSE, 
                   runCohortOverlap = TRUE, 
                   runCohortCharacterization = TRUE,
                   runTemporalCohortCharacterization = TRUE)

# drop cohort stats table
CohortGenerator::dropCohortStatsTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = results_database_schema,
    cohortTableNames = cohortTableNames,
    connection = NULL)

