# Run CohortMethod for all comparisons having sufficient sample size
# Assumes CreateCohorts.R has been executed.

source("extras/biasSearch/SetConnectionDetails.R")
library(readr)
library(dplyr)
library(CohortMethod)

minExposedSubjects <- 10000
minOutcomeSubjects <- 10000

exposures <- read_csv("extras/biasSearch/Exposures.csv", show_col_types = FALSE)
negativeControls <- read_csv("extras/biasSearch/NegativeControls.csv", show_col_types = FALSE)
cohortCounts <- read_csv(file.path(outputFolder, "cohortCounts.csv"), show_col_types = FALSE)

exposures <- exposures |>
  inner_join(
    cohortCounts |>
      select(conceptId = "cohortId", "cohortSubjects"),
    by = join_by("conceptId")
  ) |>
  filter(cohortSubjects > minExposedSubjects)

negativeControls <- negativeControls |>
  inner_join(
    cohortCounts |>
      select(conceptId = "cohortId", "cohortSubjects"),
    by = join_by("conceptId")
  ) |>
  filter(cohortSubjects > minOutcomeSubjects)


indications <- exposures |>
  distinct(indicationId)  |>
  pull()

for (i in seq_along(indications)) {
  indication <- indications[i]
  message("Running CohortMethod for indication: ", indication)
  
  indicationFolder <- file.path(outputFolder, indication)
  if (!dir.exists(indicationFolder)) {
    dir.create(indicationFolder)
  }
  
  # Create targetComparatorOutcomesList ----------------------------------------
  exposureSubset <- exposures |>
    filter(indicationId == indication)
  outcomeSubset <- negativeControls |>
    filter(indicationId == indication)
  
  comparisons <- cross_join(
    exposureSubset |>
      select(targetId = "conceptId", targetName = "name"),
    exposureSubset |>
      select(comparatorId = "conceptId", comparatorName = "name")
  ) |>
    filter(comparatorId > targetId)
  
  targetComparatorOutcomesList <- list()
  outcomes <- lapply(
    outcomeSubset$conceptId, 
    function(outcomeId) createOutcome(outcomeId, outcomeOfInterest = FALSE)
  )
  for (j in seq_len(nrow(comparisons))) {
    targetComparatorOutcomesList[[j]] <- createTargetComparatorOutcomes(
      targetId = comparisons$targetId[j],
      comparatorId = comparisons$comparatorId[j],
      outcomes = outcomes
    )
  }
  
  # Create cmAnalysisList ------------------------------------------------------
  # Not used because we use hack for efficiency:
  getDbCohortMethodDataArgs <- createGetDbCohortMethodDataArgs(
    covariateSettings = NULL
  )
  
  createStudyPopulationArgs <- createCreateStudyPopulationArgs(
    restrictToCommonPeriod = TRUE,
    removeDuplicateSubjects = "keep first",
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999,
    minDaysAtRisk = 1,
    maxDaysAtRisk = 99999,
    riskWindowStart = 0,
    startAnchor = "cohort start",
    riskWindowEnd = 0,
    endAnchor = "cohort end",
  )
  
  fitOutcomeModelArgs <- createFitOutcomeModelArgs(
    modelType = "cox",
    profileBounds = NULL
  )
  
  cmAnalysis <- createCmAnalysis(
    analysisId = 1,
    description = "Unadjusted",
    getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
    createStudyPopArgs = createStudyPopulationArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
  cmAnalysisList <- list(cmAnalysis)
  
  # Dirty hack: pull data for all comparisons at once for efficiency -----------
  source("extras/biasSearch/CustomCmDataObjectBuilding.R")
  createCmDataObjects(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = cohortDatabaseSchema,
                      cohortTable = cohortTable,
                      targetComparatorOutcomesList = targetComparatorOutcomesList,
                      indicationFolder = indicationFolder)
  
  
  # Execute analyses -----------------------------------------------------------
  runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = cohortDatabaseSchema,
    exposureTable = cohortTable,
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    cmAnalysisList = cmAnalysisList,
    outputFolder = indicationFolder,
    multiThreadingSettings = createDefaultMultiThreadingSettings(maxCores)
  )
}
