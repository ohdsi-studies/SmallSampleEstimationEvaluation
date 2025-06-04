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

#' @export
runCohortMethod <- function(connectionDetails = NULL,
                            cdmDatabaseSchema = "",
                            cohortDatabaseSchema = "",
                            cohortTable = "",
                            maxCores = 1,
                            outputFolder,
                            cmFolder,
                            externalPsFolder = NULL,
                            small = FALSE) {
  # Create list of target-comparator-outcomes ---------------------------------
  allControls <- read.csv(file.path(outputFolder, "allControls.csv")) 
  allControls <- split(allControls, paste(allControls$targetId, allControls$comparatorId))
  
  tcosList <- list()
  for (i in seq_along(allControls)) {
    controls <- allControls[[i]]
    excludedCovariateConceptIds <- c(
      as.numeric(strsplit(as.character(controls$targetConceptIds[1]), ";")[[1]]),
      as.numeric(strsplit(as.character(controls$comparatorConceptIds[1]), ";")[[1]])
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
    # washoutPeriod = 365,
    # firstExposureOnly = TRUE,
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
      minCVData = 10,
      maxIterations = 3000
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
  # multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(maxCores = maxCores)
  multiThreadingSettings <- CohortMethod::createMultiThreadingSettings(
    getDbCohortMethodDataThreads = min(2, maxCores),
    createPsThreads = max(1, floor(maxCores / 8)),
    psCvThreads = min(8, maxCores),
    createStudyPopThreads = min(3, maxCores),
    trimMatchStratifyThreads = min(5, maxCores),
    computeSharedBalanceThreads = min(3, maxCores),
    computeBalanceThreads = min(5, maxCores),
    prefilterCovariatesThreads = min(3, maxCores),
    fitOutcomeModelThreads = if (small) {max(1, floor(maxCores / 2))} else {max(1, floor(maxCores / 4))},
    outcomeCvThreads = min(4, maxCores),
    calibrationThreads = min(4, maxCores)
    )
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
