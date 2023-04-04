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

#' Run CohortMethod
#'
#' @param connectionDetails       An R object of type \code{ConnectionDetails} created using the
#'                                function \code{createConnectionDetails} in the
#'                                \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema       A database schema containing health care data in the OMOP Commond
#'                                Data Model. Note that for SQL Server, botth the database and schema
#'                                should be specified, e.g. 'cdm_schema.dbo'.
#' @param exposureDatabaseSchema  The name of the database schema where the exposure cohorts will be
#'                                created. Only needed if \code{referenceSet = 'ohdsiDevelopment'}. Note
#'                                that for SQL Server, both the database and schema should be specified,
#'                                e.g. 'cdm_schema.dbo'.
#' @param exposureTable           The name of the table that will be created to store the exposure
#'                                cohorts. Only needed if \code{referenceSet = 'ohdsiDevelopment'}.
#' @param outcomeDatabaseSchema   The database schema where the target outcome table is located. Note
#'                                that for SQL Server, both the database and schema should be
#'                                specified, e.g. 'cdm_schema.dbo'
#' @param outcomeTable            The name of the table where the outcomes will be stored.
#' @param maxCores                How many parallel cores should be used? If more cores are made available
#'                                this can speed up the analyses.
#' @param referenceSet            The name of the reference set for which outcomes need to be created.
#'                                Currently supported are "ohdsiMethodsBenchmark", and "ohdsiDevelopment".
#' @param outputFolder            Name of local folder to place intermediary results; make sure to use
#'                                forward slashes (/). Do not use a folder on a network drive since
#'                                this greatly impacts performance.
#' @param cmFolder                Name of local folder to place intermediary results; make sure to use
#'                                forward slashes (/). Do not use a folder on a network drive since
#'                                this greatly impacts performance.
#' @param externalPsFolder        Name of folder containing propensity score files of a superset of the population.
#'
#' @export
runCohortMethod <- function(connectionDetails = NULL,
                            cdmDatabaseSchema = "",
                            exposureDatabaseSchema = "",
                            exposureTable = "",
                            outcomeDatabaseSchema = "",
                            outcomeTable = "",
                            maxCores = 1,
                            outputFolder,
                            cmFolder,
                            externalPsFolder = NULL) {
  # Create list of target-comparator-outcomes ---------------------------------
  allControls <- read.csv(file.path(outputFolder, "allControls.csv")) 
  allControls <- split(allControls, paste(allControls$targetId, allControls$comparatorId))
  
  tcosList <- list()
  for (i in seq_along(allControls)) {
    controls <- allControls[[i]]
    excludedCovariateConceptIds <- c(
      as.numeric(strsplit(controls$targetConceptIds[1], ";")[[1]]),
      as.numeric(strsplit(controls$comparatorConceptIds[1], ";")[[1]])
    )
    outcomes <- list()
    for (j in seq_along(controls$outcomeId)) {
      outcomes[[j]] <- CohortMethod::createOutcome(
        outcomeId = controls$outcomeId[j],
        outcomeOfInterest = TRUE
      )
    }
    tcos <- CohortMethod::createTargetComparatorOutcomes(
      targetId = controls$targetId[1],
      comparatorId = controls$comparatorId[1],
      outcomes = outcomes,
      excludedCovariateConceptIds = excludedCovariateConceptIds
    )
    tcosList[[i]] <- tcos
  }
  
  # Create analysis settings list -------------------------------------------------
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(addDescendantsToExclude = TRUE)
  getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
    washoutPeriod = 365,
    firstExposureOnly = TRUE,
    removeDuplicateSubjects = "remove all",
    maxCohortSize = 1e6,
    covariateSettings = covariateSettings
  )
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    removeSubjectsWithPriorOutcome = TRUE,
    minDaysAtRisk = 1,
    riskWindowStart = 0,
    startAnchor = "cohort start",
    riskWindowEnd = 0,
    endAnchor = "cohort end"
  )
  createPsArgs <- CohortMethod::createCreatePsArgs(
    errorOnHighCorrelation = TRUE,
    stopOnError = FALSE,
    maxCohortSizeForFitting = 150000,
    control = Cyclops::createControl(
      cvType = "auto",
      startingVariance = 0.01,
      noiseLevel = "quiet",
      tolerance = 2e-07,
      cvRepetitions = 10,
      minCVData = 10
    )
  )
  matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 1)
  computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs()
  fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(modelType = "cox", stratified = FALSE)
  cmAnalysis1 <- CohortMethod::createCmAnalysis(
    analysisId = 1,
    description = "1-on-1 matching",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPsArgs = createPsArgs,
    matchOnPsArgs = matchOnPsArgs,
    computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
  cmAnalysis2 <- CohortMethod::createCmAnalysis(
    analysisId = 2,
    description = "Crude",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
  stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10)
  fitOutcomeModelArgs2 <- CohortMethod::createFitOutcomeModelArgs(modelType = "cox", stratified = TRUE)
  cmAnalysis3 <- CohortMethod::createCmAnalysis(
    analysisId = 3,
    description = "Stratification",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPsArgs = createPsArgs,
    stratifyByPsArgs = stratifyByPsArgs,
    computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs2
  )
  if (!is.null(externalPsFolder)) {
    externalPsFileNames <- list.files(externalPsFolder, "Ps_l1_s1_p1_t[0-9]+_c[0-9]+.rds")
    for (externalPsFileName in externalPsFileNames) {
      file.copy(
        file.path(externalPsFolder, externalPsFileName),
        file.path(cmFolder, gsub("_p1_", "_p3_", externalPsFileName))
      )
    }
    dummyCreatePsArgs <- CohortMethod::createCreatePsArgs(
      errorOnHighCorrelation = TRUE,
      stopOnError = FALSE,
      maxCohortSizeForFitting = 1,
      control = Cyclops::createControl(
        cvType = "auto",
        startingVariance = 0.01,
        noiseLevel = "quiet",
        tolerance = 2e-07,
        cvRepetitions = 1
      )
    )
    cmAnalysis4 <- CohortMethod::createCmAnalysis(
      analysisId = 4,
      description = "1-on-1 matching on external PS",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopArgs = createStudyPopArgs,
      createPsArgs = dummyCreatePsArgs,
      matchOnPsArgs = matchOnPsArgs,
      computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs
    )
    cmAnalysis5 <- CohortMethod::createCmAnalysis(
      analysisId = 5,
      description = "Stratification by external PS",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopArgs = createStudyPopArgs,
      createPsArgs = dummyCreatePsArgs,
      stratifyByPsArgs = stratifyByPsArgs,
      computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs2
    )
    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5)
  } else {
    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3)
  }
  CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(cmFolder, "cmAnalysisList.json"))
  
  # Run analyses ----------------------------------------------------------------------
  multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(maxCores = maxCores)
  if (is.null(connectionDetails)) {
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql")
  }
  cmResult <- CohortMethod::runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = cohortDatabaseSchema,
    exposureTable = cohortTable,
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable,
    outputFolder = cmFolder,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = tcosList,
    refitPsForEveryOutcome = FALSE,
    multiThreadingSettings = multiThreadingSettings
  )
}
