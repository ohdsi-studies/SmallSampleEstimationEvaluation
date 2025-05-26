# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of SmallSampleEstimationEvaluation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# library(dplyr)

#' @export
createCohorts <- function(connectionDetails,
                          cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortTable,
                          outputFolder,
                          maxCores) {
  connection <- connect(connectionDetails)
  on.exit(disconnect(connection))
  
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)
  CohortGenerator::createCohortTables(
    connection = connection, 
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames
  )
  # Generate exposure cohorts -------------------------------------------------
  rdsFileName <- system.file("CohortDefinitionSet.rds", package = "SmallSampleEstimationEvaluation")
  cohortDefinitionSet <- readRDS(rdsFileName)
  CohortGenerator::generateCohortSet(
    connection = connection, 
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDefinitionSet = cohortDefinitionSet
  )
  # Generate negative control outcome cohorts ---------------------------------
  csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
  negativeControlOutcomeCohortSet <- readr::read_csv(csvFileName, show_col_types = FALSE) |>
    mutate(cohortId = .data$outcomeConceptId) |>
    select("cohortId", cohortName = "outcomeName", "outcomeConceptId") |>
    filter(!duplicated(cohortId))
  CohortGenerator::generateNegativeControlOutcomeCohorts(
    connection = connection, 
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
  
  # Generate synthetic positive controls --------------------------------------  
  csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
  exposureOutcomePairs <- readr::read_csv(csvFileName, show_col_types = FALSE) |>
    select(exposureId = "targetId", outcomeId = "outcomeConceptId") |>
    distinct()
  prior <- Cyclops::createPrior("laplace", exclude = 0, useCrossValidation = TRUE)
  control <- Cyclops::createControl(
    cvType = "auto",
    startingVariance = 0.01,
    noiseLevel = "quiet",
    cvRepetitions = 1,
    threads = min(c(10, maxCores))
  )
  covariateSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAgeGroup = TRUE,
    useDemographicsGender = TRUE,
    useDemographicsIndexYear = TRUE,
    useDemographicsIndexMonth = TRUE,
    useConditionGroupEraLongTerm = TRUE,
    useDrugGroupEraLongTerm = TRUE,
    useProcedureOccurrenceLongTerm = TRUE,
    useMeasurementLongTerm = TRUE,
    useObservationLongTerm = TRUE,
    useCharlsonIndex = TRUE,
    useDcsi = TRUE,
    useChads2Vasc = TRUE,
    longTermStartDays = 365,
    endDays = 0
  )
  injectionFolder <- file.path(outputFolder, "SignalInjection")
  if (!file.exists(injectionFolder)) {
    dir.create(injectionFolder)
  }
  result <- MethodEvaluation::synthesizePositiveControls(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = cohortDatabaseSchema,
    exposureTable = cohortTable,
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable,
    outputDatabaseSchema = cohortDatabaseSchema,
    outputTable = cohortTable,
    createOutputTable = FALSE,
    outputIdOffset = 10000,
    exposureOutcomePairs = exposureOutcomePairs,
    firstExposureOnly = TRUE,
    firstOutcomeOnly = TRUE,
    removePeopleWithPriorOutcomes = TRUE,
    modelType = "survival",
    washoutPeriod = 365,
    riskWindowStart = 0,
    riskWindowEnd = 0,
    endAnchor = "cohort end",
    effectSizes = c(1.5, 2, 4),
    precision = 0.01,
    prior = prior,
    control = control,
    maxSubjectsForModel = 250000,
    minOutcomeCountForModel = 100,
    minOutcomeCountForInjection = 25,
    workFolder = injectionFolder,
    modelThreads = max(1, round(maxCores/8)),
    generationThreads = min(6, maxCores),
    covariateSettings = covariateSettings)
  injectionSummaryFile <- file.path(injectionFolder, "injectionSummary.rds")
  saveRDS(result, injectionSummaryFile)
  
  negativeControls <- readr::read_csv(csvFileName, show_col_types = FALSE) |>
    rename(outcomeId = "outcomeConceptId")
  injectedSignals <- readRDS(injectionSummaryFile) |>
    rename(targetId = "exposureId") |>
    inner_join(negativeControls, by = join_by("targetId", "outcomeId")) |>
    filter(.data$trueEffectSize != 0) |>
    mutate(outcomeName = sprintf("%s, RR=%0.1f", .data$outcomeName, .data$targetEffectSize)) |>
    rename(oldOutcomeId = "outcomeId") |>
    rename(outcomeId = "newOutcomeId")
  negativeControls$targetEffectSize <- 1
  negativeControls$trueEffectSize <- 1
  negativeControls$trueEffectSizeFirstExposure <- 1
  negativeControls$oldOutcomeId <- negativeControls$outcomeId
  allControls <- bind_rows(negativeControls, injectedSignals[, names(negativeControls)])
  allControlsFileName <- file.path(outputFolder, "allControls.csv")
  readr::write_csv(allControls, allControlsFileName)
}
