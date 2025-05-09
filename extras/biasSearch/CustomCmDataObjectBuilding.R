# This script contains a function that efficiently creates all CohortMethodData
# objects by loading data for all cohorts in one go, and then generating the
# objects for each comparison. This avoids having to load the same data over and
# over again for each comparison. The code is relatively clear because we don't
# need covariates.

createCmDataObjects <- function(connectionDetails,
                                cdmDatabaseSchema,
                                cohortDatabaseSchema,
                                cohortTable,
                                targetComparatorOutcomesList,
                                indicationFolder) {
  cohortsAndOutcomesFile <- file.path(indicationFolder, "allCohortsAndOutcomes.zip")
  
  if (!file.exists(cohortsAndOutcomesFile)) {
    message("Fetching all data from the server")
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
    
    # Create one big table for consistent rowIds and personSeqIds --------------
    exposureIds <- unique(do.call(c, lapply(targetComparatorOutcomesList, function(x) c(x$targetId, x$comparatorId))))
    
    sql <- "
DROP TABLE IF EXISTS #cohort_union;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT ROW_NUMBER() OVER (
		ORDER BY tmp1.subject_id,
			tmp1.cohort_start_date
		) AS row_id,
	tmp1.subject_id,
	tmp1.cohort_start_date,
	-1 AS cohort_end_date,
	-1 AS cohort_definition_id,
	tmp2.person_seq_id
INTO #cohort_union
FROM (
	SELECT DISTINCT subject_id,
		cohort_start_date
	FROM @cohort_database_schema.@cohort_table cohort
	WHERE cohort_definition_id IN (@exposure_ids)
	) tmp1
INNER JOIN (
  SELECT subject_id, 
    ROW_NUMBER() OVER (
      ORDER BY subject_id
    ) AS person_seq_id
  FROM (
    SELECT DISTINCT subject_id
	  FROM @cohort_database_schema.@cohort_table
	  WHERE cohort_definition_id IN (@exposure_ids)
    ) tmp3
  ) tmp2
  ON tmp1.subject_id = tmp2.subject_id;
"
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      exposure_ids = exposureIds
    )
    
    # Retrieve cohorts and outcomes ----------------------------------------------
    cohortsAndOutcomes <- Andromeda::andromeda()
    
    sql <- "
SELECT cohort_union.row_id,
  person_seq_id,
	CAST(cohort.subject_id AS VARCHAR(30)) AS person_id,
	cohort.cohort_definition_id AS exposure_id,
	cohort.cohort_start_date,
	DATEDIFF(DAY, observation_period_start_date, cohort.cohort_start_date) AS days_from_obs_start,
	DATEDIFF(DAY, cohort.cohort_start_date, cohort.cohort_end_date) AS days_to_cohort_end,
	DATEDIFF(DAY, cohort.cohort_start_date, observation_period_end_date) AS days_to_obs_end
FROM #cohort_union cohort_union
INNER JOIN @cohort_database_schema.@cohort_table cohort
  ON cohort_union.subject_id = cohort.subject_id
    AND cohort_union.cohort_start_date = cohort.cohort_start_date
INNER JOIN @cdm_database_schema.observation_period
	ON cohort.subject_id = person_id  
    AND cohort.cohort_start_date <= observation_period_end_date
	  AND cohort.cohort_start_date >= observation_period_start_date
WHERE cohort.cohort_definition_id IN (@exposure_ids);
	"
    DatabaseConnector::renderTranslateQuerySqlToAndromeda(
      connection = connection,
      sql = sql,
      andromeda = cohortsAndOutcomes, 
      andromedaTableName = "cohorts",
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      cdm_database_schema = cdmDatabaseSchema,
      exposure_ids = exposureIds,
      snakeCaseToCamelCase = TRUE
    )
    
    sql <- "
SELECT cohort_union.row_id,
	exposure.cohort_definition_id  AS exposure_id,
  outcome.cohort_definition_id AS outcome_id,
  DATEDIFF(DAY, exposure.cohort_start_date, outcome.cohort_start_date) AS days_to_event
FROM #cohort_union cohort_union
INNER JOIN @cohort_database_schema.@cohort_table exposure
  ON cohort_union.subject_id = exposure.subject_id
    AND cohort_union.cohort_start_date = exposure.cohort_start_date
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.subject_id = person_id  
    AND exposure.cohort_start_date <= observation_period_end_date
	  AND exposure.cohort_start_date >= observation_period_start_date
INNER JOIN @cohort_database_schema.@cohort_table outcome
	ON outcome.subject_id = person_id  
    AND outcome.cohort_start_date <= observation_period_end_date
	  AND outcome.cohort_start_date >= observation_period_start_date
WHERE exposure.cohort_definition_id IN (@exposure_ids)
  AND outcome.cohort_definition_id in (@outcome_ids);  
      "
    outcomeIds <- unique(do.call(c, lapply(targetComparatorOutcomesList, function(x) sapply(x$outcomes, function(y) y$outcomeId))))
    DatabaseConnector::renderTranslateQuerySqlToAndromeda(
      connection = connection,
      sql = sql,
      andromeda = cohortsAndOutcomes, 
      andromedaTableName = "outcomes",
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      cdm_database_schema = cdmDatabaseSchema,
      exposure_ids = exposureIds,
      outcome_ids = outcomeIds,
      snakeCaseToCamelCase = TRUE
    )
    
    Andromeda::saveAndromeda(cohortsAndOutcomes, cohortsAndOutcomesFile)
    
    sql <- "TRUNCATE TABLE #cohort_union; DROP TABLE #cohort_union;"
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      reportOverallTime = FALSE,
      progressBar = FALSE
    )
  }
  
  
  # Create CohortMethodData files ----------------------------------------------
  message("Creating CohortMethodData files")
  cohortsAndOutcomes <- Andromeda::loadAndromeda(cohortsAndOutcomesFile)
  
  pb <- txtProgressBar(style = 3)
  for (i in seq_along(targetComparatorOutcomesList)) {
    tco <- targetComparatorOutcomesList[[i]]
    cmdFileName <- CohortMethod:::.createCohortMethodDataFileName(
      loadId = 1,
      targetId = tco$targetId,
      comparatorId = tco$comparatorId
    )
    if (!file.exists(file.path(indicationFolder, cmdFileName))) {
      object <- FeatureExtraction::createEmptyCovariateData(
        aggregated = FALSE,
        temporal = FALSE,
        cohortIds = c(tco$targetId, tco$comparatorId))
      
      object$cohorts <- cohortsAndOutcomes$cohorts |>
        filter(exposureId %in% c(tco$targetId, tco$comparatorId)) |>
        mutate(treatment = if_else(exposureId == tco$targetId, 1, 0)) |>
        select(-"exposureId")
      
      object$outcomes <- cohortsAndOutcomes$outcomes |>
        filter(exposureId %in% c(tco$targetId, tco$comparatorId)) |>
        select(-"exposureId")
      
      class(object) <- "CohortMethodData"
      attr(class(object), "package") <- "CohortMethod"
      
      # # Test same as standard process:
      # cmData <- getDbCohortMethodData(
      #   connectionDetails = connectionDetails,
      #   cdmDatabaseSchema = cdmDatabaseSchema,
      #   exposureDatabaseSchema = cohortDatabaseSchema,
      #   exposureTable = cohortTable,
      #   outcomeDatabaseSchema = cohortDatabaseSchema,
      #   outcomeTable = cohortTable,
      #   targetId = tco$targetId,
      #   comparatorId = tco$comparatorId,
      #   outcomeIds = outcomeIds,
      #   covariateSettings = createCovariateSettings(useDemographicsEthnicity = TRUE)
      # )
      # studyPop <- createStudyPopulation(object, outcomeId = outcomeIds[1], removeDuplicateSubjects = "keep first")
      # model <- fitOutcomeModel(studyPop, modelType = "cox", profileBounds = NULL)
      # studydPop2 <- createStudyPopulation(cmData, outcomeId = outcomeIds[1], removeDuplicateSubjects = "keep first")
      # model2 <- fitOutcomeModel(studydPop2, modelType = "cox", profileBounds = NULL)
      # all_equal(model$outcomeModelTreatmentEstimate, model2$outcomeModelTreatmentEstimate)
      invisible(capture.output(suppressMessages(CohortMethod::saveCohortMethodData(object, file.path(indicationFolder, cmdFileName)))))
    } 
    setTxtProgressBar(pb, i / length(targetComparatorOutcomesList))
    
  }
  close(pb)
}
