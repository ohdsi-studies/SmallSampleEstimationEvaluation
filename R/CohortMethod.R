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

#' Create cohorts used for the evaluation.
#'
#' @details
#' This function will create the outcomes of interest and nesting cohorts referenced in the various
#' reference sets. The outcomes of interest are derives using information like diagnoses, procedures,
#' and drug prescriptions. The outcomes are stored in a table on the database server.
#'
#' For the 'ohdsiMethodsBenchmark' reference set, the exposures are taken from the drug_era table, and
#' are therefore not generated as separate cohorts, and an exposure cohort table therefore needn't be supplied.
#' For the 'ohdsiDevelopment' reference set, exposure cohorts will be generated.
#'
#' @param connectionDetails       An R object of type \code{ConnectionDetails} created using the
#'                                function \code{createConnectionDetails} in the
#'                                \code{DatabaseConnector} package.
#' @param oracleTempSchema        Should be used in Oracle to specify a schema where the user has write
#'                                privileges for storing temporary tables.
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
                            oracleTempSchema = NULL,
                            cdmDatabaseSchema = "",
                            exposureDatabaseSchema = "",
                            exposureTable = "",
                            outcomeDatabaseSchema = "",
                            outcomeTable = "",
                            maxCores = 1,
                            referenceSet = "ohdsiMethodsBenchmark",
                            outputFolder,
                            cmFolder,
                            externalPsFolder = NULL) {
  # Create list of target-comparator-outcomes ---------------------------------
  allControls <- read.csv(file.path(outputFolder, "allControls.csv"))
  tcs <- unique(allControls[, c("targetId", "comparatorId", "targetConceptIds", "comparatorConceptIds")])
  tcosList <- list()
  for (i in 1:nrow(tcs)) {
    outcomeIds <- allControls$outcomeId[allControls$targetId == tcs$targetId[i] &
      allControls$comparatorId == tcs$comparatorId[i] &
      !is.na(allControls$mdrrComparator)]
    excludedCovariateConceptIds <- c(
      as.numeric(strsplit(tcs$targetConceptIds[i], ";")[[1]]),
      as.numeric(strsplit(tcs$comparatorConceptIds[i], ";")[[1]])
    )
    if (length(outcomeIds) != 0) {
      tcos <- CohortMethod::createTargetComparatorOutcomes(
        targetId = tcs$targetId[i],
        comparatorId = tcs$comparatorId[i],
        outcomeIds = outcomeIds,
        excludedCovariateConceptIds = excludedCovariateConceptIds
      )
      tcosList[[length(tcosList) + 1]] <- tcos
    }
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
      cvRepetitions = 1
    )
  )
  matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 1)
  fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(modelType = "cox", stratified = FALSE)
  cmAnalysis1 <- CohortMethod::createCmAnalysis(
    analysisId = 1,
    description = "1-on-1 matching",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPs = TRUE,
    createPsArgs = createPsArgs,
    matchOnPs = TRUE,
    matchOnPsArgs = matchOnPsArgs,
    fitOutcomeModel = TRUE,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
  cmAnalysis2 <- CohortMethod::createCmAnalysis(
    analysisId = 2,
    description = "Crude",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    fitOutcomeModel = TRUE,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
  stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10)
  fitOutcomeModelArgs2 <- CohortMethod::createFitOutcomeModelArgs(modelType = "cox", stratified = TRUE)
  cmAnalysis3 <- CohortMethod::createCmAnalysis(
    analysisId = 3,
    description = "Stratification",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPs = TRUE,
    createPsArgs = createPsArgs,
    stratifyByPs = TRUE,
    stratifyByPsArgs = stratifyByPsArgs,
    fitOutcomeModel = TRUE,
    fitOutcomeModelArgs = fitOutcomeModelArgs2
  )
  if (!is.null(externalPsFolder)) {
    externalPsFileNames <- list.files(externalPsFolder, "Ps_l1_p1_t[0-9]+_c[0-9]+.rds")
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
      createPs = TRUE,
      createPsArgs = dummyCreatePsArgs,
      matchOnPs = TRUE,
      matchOnPsArgs = matchOnPsArgs,
      fitOutcomeModel = TRUE,
      fitOutcomeModelArgs = fitOutcomeModelArgs
    )
    cmAnalysis5 <- CohortMethod::createCmAnalysis(
      analysisId = 5,
      description = "Stratification by external PS",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopArgs = createStudyPopArgs,
      createPs = TRUE,
      createPsArgs = dummyCreatePsArgs,
      stratifyByPs = TRUE,
      stratifyByPsArgs = stratifyByPsArgs,
      fitOutcomeModel = TRUE,
      fitOutcomeModelArgs = fitOutcomeModelArgs2
    )
    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5)
  } else {
    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3)
  }
  CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(cmFolder, "cmAnalysisList.json"))

  # Run analyses ----------------------------------------------------------------------
  cmResult <- CohortMethod::runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    oracleTempSchema = oracleTempSchema,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema,
    outcomeTable = outcomeTable,
    outputFolder = cmFolder,
    cdmVersion = cdmVersion,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = tcosList,
    refitPsForEveryOutcome = FALSE,
    refitPsForEveryStudyPopulation = FALSE,
    getDbCohortMethodDataThreads = min(3, maxCores),
    createStudyPopThreads = min(3, maxCores),
    createPsThreads = min(3, maxCores),
    psCvThreads = min(10, floor(maxCores / 3)),
    trimMatchStratifyThreads = min(10, maxCores),
    fitOutcomeModelThreads = min(max(1, floor(maxCores / 8)), 3),
    outcomeCvThreads = min(10, maxCores)
  )
  ParallelLogger::logInfo("Summarizing results")
  cmSummary <- CohortMethod::summarizeAnalyses(cmResult, cmFolder)
  saveRDS(cmSummary, file.path(cmFolder, "cmSummary.rds"))

  # Compute covariate balance -------------------------------------------------------------
  ParallelLogger::logInfo("Computing balance")
  cmResult <- readRDS(file.path(cmFolder, "outcomeModelReference.rds"))
  analysisIds <- cmResult %>%
    filter(.data$sharedPsFile != "") %>%
    distinct(.data$analysisId) %>%
    pull()

  getCmAnalysis <- function(analysisId) {
    for (cmAnalysis in cmAnalysisList) {
      if (cmAnalysis$analysisId == analysisId) {
        return(cmAnalysis)
      }
    }
    return(NULL)
  }
  for (analysisId in analysisIds) {
    tcs <- cmResult %>%
      filter(.data$analysisId == !!analysisId) %>%
      distinct(.data$targetId, .data$comparatorId) %>%
      collect()


    for (i in 1:nrow(tcs)) {
      tc <- tcs[i, ]
      balanceFile <- sprintf("bal_t%s_c%s_a%s.rds", tc$targetId, tc$comparatorId, analysisId)
      if (!file.exists(file.path(cmFolder, balanceFile))) {
        omrRow <- cmResult[cmResult$targetId == tc$targetId &
          cmResult$comparatorId == tc$comparatorId &
          cmResult$analysisId == analysisId, ][1, ]
        sharedPs <- readRDS(file.path(cmFolder, omrRow$sharedPsFile))
        cmDataFile <- omrRow$cohortMethodDataFile
        cmData <- CohortMethod::loadCohortMethodData(file.path(cmFolder, cmDataFile))
        cmAnalysis <- getCmAnalysis(analysisId)
        createStudyPopArgs <- cmAnalysis$createStudyPopArgs
        createStudyPopArgs$cohortMethodData <- cmData
        studyPop <- do.call(CohortMethod::createStudyPopulation, createStudyPopArgs)
        studyPop <- studyPop %>%
          inner_join(sharedPs %>% select(.data$rowId, .data$propensityScore),
            by = "rowId"
          )
        if (cmAnalysis$matchOnPs) {
          matchOnPsArgs <- cmAnalysis$matchOnPsArgs
          matchOnPsArgs$population <- studyPop
          strataPop <- do.call(CohortMethod::matchOnPs, matchOnPsArgs)
        } else if (cmAnalysis$stratifyByPs) {
          stratifyByPsArgs <- cmAnalysis$stratifyByPsArgs
          stratifyByPsArgs$population <- studyPop
          strataPop <- do.call(CohortMethod::stratifyByPs, stratifyByPsArgs)
        }
        balance <- CohortMethod::computeCovariateBalance(strataPop, cmData)
        saveRDS(balance, file.path(cmFolder, balanceFile))
        balancePlotFile <- sprintf("balPlot_t%s_c%s_a%s.png", tc$targetId, tc$comparatorId, analysisId)
        CohortMethod::plotCovariateBalanceScatterPlot(
          balance = balance,
          showCovariateCountLabel = TRUE,
          showMaxLabel = TRUE,
          fileName = file.path(cmFolder, balancePlotFile)
        )
      }
    }
  }
}
