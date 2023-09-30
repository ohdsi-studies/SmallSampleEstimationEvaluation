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
computeMdrr <- function(outputFolder, cmFolder, connectionDetails, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable, outputFileName) {
  fileRef <- CohortMethod::getFileReference(cmFolder)
  tcs <- fileRef %>%
    distinct(.data$targetId, .data$comparatorId, .data$cohortMethodDataFile) %>%
    mutate(tempTargetId = row_number()) %>%
    mutate(tempComparatorId = .data$tempTargetId * 100)
  # MDRR function assumes cohorts exist in database. We sampled cohorts, so must upload them
  # (temporarily) to compute MDRR:
  cohorts <- list()
  for (i in seq_len(nrow(tcs))) {
    cmData <- CohortMethod::loadCohortMethodData(file.path(cmFolder, tcs$cohortMethodDataFile[i]))
    cohort <- cmData$cohorts %>%
      select("personId", "treatment", "cohortStartDate", "daysToCohortEnd") %>%
      collect() %>%
      mutate(
        personId = bit64::as.integer64(.data$personId),
        cohortEndDate = .data$cohortStartDate + .data$daysToCohortEnd,
        cohortDefinitionId = if_else(.data$treatment == 1, tcs$tempTargetId[i], tcs$tempComparatorId[i])
      ) %>%
      select(subjectId = "personId", "cohortDefinitionId", "cohortStartDate", "cohortEndDate")
    cohorts[[i]] <- cohort
  }
  cohorts <- bind_rows(cohorts)
  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = "temp_table",
    data = cohorts,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    camelCaseToSnakeCase = TRUE
  )
  DatabaseConnector::disconnect(connection)
  csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
  negativeControls <- readr::read_csv(csvFileName, show_col_types = FALSE)  %>%
    select("targetId", "comparatorId", outcomeId = "outcomeConceptId")
  ncs <- negativeControls %>%
    inner_join(tcs, by = join_by("targetId", "comparatorId"))
  synthesisSummary <- readRDS(file.path(outputFolder, "SignalInjection", "injectionSummary.rds"))
  pcs <- synthesisSummary %>%
    select(targetId = "exposureId", outcomeId = "newOutcomeId") %>%
    inner_join(tcs, by = join_by("targetId"), relationship = "many-to-many")
  exposureOutcomePairs <- bind_rows(
    pcs %>%
      select(exposureId = "tempTargetId", "outcomeId"),
    pcs %>%
      select(exposureId = "tempComparatorId", "outcomeId"),
    ncs %>%
      select(exposureId = "tempTargetId", "outcomeId"),
    ncs %>%
      select(exposureId = "tempComparatorId", "outcomeId")
  )
  mdrr <- MethodEvaluation::computeMdrr(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureOutcomePairs = exposureOutcomePairs,
    exposureDatabaseSchema = cohortDatabaseSchema,
    exposureTable = "temp_table",
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable
  )
  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "TRUNCATE TABLE @database_schema.temp_table; DROP TABLE @database_schema.temp_table;",
    database_schema = cohortDatabaseSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  DatabaseConnector::disconnect(connection)
  mdrr <- inner_join(
    mdrr %>%
      as_tibble() %>%
      select(tempTargetId = "exposureId", "outcomeId", "mdrr") %>%
      inner_join(tcs, by = join_by("tempTargetId")) %>%
      select("targetId", "comparatorId", "outcomeId", mdrrTarget = "mdrr"),
    mdrr %>%
      as_tibble() %>%
      select(tempComparatorId = "exposureId", "outcomeId", "mdrr") %>%
      inner_join(tcs, by = join_by("tempComparatorId")) %>%
      select("targetId", "comparatorId", "outcomeId", mdrrComparator = "mdrr"),
    by = join_by("targetId", "comparatorId", "outcomeId")
  )
  readr::write_csv(mdrr, file.path(outputFolder, "Mdrr_LargeSample.csv"))
}

#' @export
computePerformance <- function(outputFolder,
                               cmFolder,
                               maxCores = 1,
                               outputFileName,
                               databaseId) {
  if (file.exists(outputFileName)) {
    return()
  }
  csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
  negativeControls <- readr::read_csv(csvFileName, show_col_types = FALSE)  %>%
    select("targetId", "comparatorId", outcomeId = "outcomeConceptId") %>%
    mutate(type = "Outcome control")
  synthesisSummary <- readRDS(file.path(outputFolder, "SignalInjection", "injectionSummary.rds"))
  estimates <- CohortMethod::getResultsSummary(cmFolder)
  mdrr <- readr::read_csv(file.path(outputFolder, "Mdrr_LargeSample.csv"), show_col_types = FALSE)
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(file.path(cmFolder, "cmAnalysisList.json"))
  analysisId <- unlist(ParallelLogger::selectFromList(cmAnalysisList, "analysisId"))
  description <- unlist(ParallelLogger::selectFromList(cmAnalysisList, "description"))
  details <- sapply(cmAnalysisList, ParallelLogger::convertSettingsToJson)
  analysisRef <- tibble(
    method = "CohortMethod",
    analysisId = analysisId,
    description = description,
    details = details,
    comparative = TRUE,
    nesting = FALSE,
    firstExposureOnly = TRUE
  )
  if (max(estimates$analysisId) > 100) {
    analysisRefRandomFx <- analysisRef %>%
      mutate(
        analysisId = .data$analysisId + 100,
        description = paste0(.data$description, ", RFX")
      )
    analysisRef <- bind_rows(analysisRef, analysisRefRandomFx)
  }
  if (max(estimates$analysisId) > 200) {
    analysisTraditional <- analysisRef %>%
      filter(.data$analysisId < 100) %>%
      mutate(
        analysisId = .data$analysisId + 200,
        description = paste0(.data$description, ", Traditional")
      )
    analysisRef <- bind_rows(analysisRef, analysisTraditional)
  }
  MethodEvaluation::packageCustomBenchmarkResults(
    estimates = estimates %>% 
      recodeTargetId(),
    negativeControls = negativeControls %>% 
      recodeTargetId(),
    synthesisSummary = synthesisSummary %>% 
      inner_join(negativeControls %>%
                   distinct(.data$targetId, .data$comparatorId) %>%
                   rename(exposureId = .data$targetId),
                 by = join_by("exposureId"),
                 relationship = "many-to-many")  %>% 
      recodeExposureId(),
    mdrr = mdrr  %>% 
      rename(exposureId = "targetId") %>%
      recodeExposureId(),
    analysisRef = analysisRef,
    databaseName = databaseId,
    exportFolder = cmFolder
  )
  metrics <- tibble()
  tcs <- negativeControls %>%
    distinct(.data$targetId, .data$comparatorId) %>%
    mutate(exposureId = .data$targetId) %>%
    recodeExposureId()
  for (i in seq_len(nrow(tcs))) {
    tc <- tcs[i, ]
    metricsUncalibrated <- MethodEvaluation::computeOhdsiBenchmarkMetrics(
      exportFolder = cmFolder,
      mdrr = 1.25,
      calibrated = FALSE,
      comparative = TRUE,
      stratum = tcs$exposureId[i]
    )
    metricsUncalibrated$calibrated <- FALSE
    metricsUncalibrated$targetId <- tc$targetId
    metricsUncalibrated$comparatorId <- tc$comparatorId
    metrics <- bind_rows(metrics, metricsUncalibrated)
    metricsCalibrated <- MethodEvaluation::computeOhdsiBenchmarkMetrics(
      exportFolder = cmFolder,
      mdrr = 1.25,
      calibrated = TRUE,
      comparative = TRUE,
      stratum = tcs$exposureId[i]
    )
    metricsCalibrated$calibrated <- TRUE
    metricsCalibrated$targetId <- tc$targetId
    metricsCalibrated$comparatorId <- tc$comparatorId
    metrics <- bind_rows(metrics, metricsCalibrated)
  }
  subsets <- estimates %>%
    inner_join(negativeControls, by = join_by("targetId", "comparatorId", "outcomeId")) %>%
    inner_join(analysisRef %>%
                 select("analysisId", "description"),
               by = "analysisId"
    )%>%
    group_by(.data$analysisId, .data$targetId, .data$comparatorId) %>%
    group_split()
  message("Computing EASE")
  cluster <- ParallelLogger::makeCluster(maxCores)
  on.exit(ParallelLogger::stopCluster(cluster))
  ParallelLogger::clusterRequire(cluster, "dplyr")
  ease <- ParallelLogger::clusterApply(cluster, subsets, generatePlotsAndComputeEase, folder = cmFolder) %>%
    bind_rows() %>%
    mutate(calibrated = FALSE)
  metrics <- metrics %>%
    left_join(ease, by = join_by("analysisId", "targetId", "comparatorId", "calibrated"))
  readr::write_csv(metrics, outputFileName)
}

# ncs = subsets[[1]]
generatePlotsAndComputeEase <- function(ncs, folder = NULL) {
  null <- EmpiricalCalibration::fitMcmcNull(
    logRr = ncs$logRr,
    seLogRr = ncs$seLogRr
  )
  if (!is.null(folder)) {
    ncPlotFileName <- file.path(folder, sprintf("ncs_t%d_c%d_a%d.png", ncs$targetId[1], ncs$comparatorId[1], ncs$analysisId[1]))
    EmpiricalCalibration::plotCalibrationEffect(
      logRrNegatives = ncs$logRr,
      seLogRrNegatives = ncs$seLogRr,
      null = null,
      showCis = TRUE,
      showExpectedAbsoluteSystematicError = TRUE,
      title = ncs$description[1],
      fileName = ncPlotFileName
    )
  }
  easeResult <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
  easeResult <- ncs %>%
    head(1) %>%
    select("targetId", "comparatorId", "analysisId") %>%
    mutate(
      ease = easeResult$ease,
      easeCi95Lb = easeResult$ciLb,
      easeCi95Ub = easeResult$ciUb,
      nullMean = null[1],
      nullSd = 1/sqrt(null[2])
    )
  return(easeResult)
}

computeSingleSampleMetrics <- function(sampleFolder, ref) {
  if (ref$sharedBalanceFile[1] == "") {
    maxSdm <- NA
  } else {
    balance <- readRDS(file.path(sampleFolder, ref$sharedBalanceFile[1]))
    maxSdm <- max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)
  }
  if (ref$sharedPsFile[1] == "") {
    covCount <- NA
    nonZeroCoefCount <- NA
    auc <- NA
  } else {
    ps <- readRDS(file.path(sampleFolder, ref$sharedPsFile[1]))
    metaData <- attr(ps, "metaData")
    covCount <- length(metaData$psModelCoef)
    nonZeroCoefCount <- sum(metaData$psModelCoef != 0)
    auc <- CohortMethod::computePsAuc(ps)
  }
  tibble(
    maxSdm = maxSdm,
    covCount = covCount,
    nonZeroCoefCount = nonZeroCoefCount,
    auc = auc
  ) %>% return()
}

# row = combis[[1]]
computePsMetricsForAnalysisId <- function(row, sampleFolders) {
  ref <- CohortMethod::getFileReference(sampleFolders[1]) %>%
    filter(.data$analysisId == row$analysisId, .data$targetId == row$targetId) %>%
    head(1)
  stats <- lapply(sampleFolders, computeSingleSampleMetrics, ref = ref)
  stats <- bind_rows(stats)
  result <- c()
  for (column in colnames(stats)) {
    temp <- quantile(stats[[column]], c(0, 0.25, 0.5, 0.75, 1))  
    names(temp) <- paste0(column, c("Min", "P25", "Median", "P75", "Max"))
    result <- c(result, temp)
  }
  result <- as_tibble(t(result)) %>% 
    mutate(analysisId = row$analysisId,
           targetId = row$targetId,
           comparatorId = row$comparatorId)
  return(result)
}

#' @export
computePsMetrics <- function(sampleFolders, outputFileName) {
  if (file.exists(outputFileName)) {
    return()
  }
  csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
  combis <- negativeControls <- readr::read_csv(csvFileName, show_col_types = FALSE)  %>%
    distinct(.data$targetId, .data$comparatorId) %>%
    cross_join(tibble(analysisId = c(1, 3)))
  combis <- split(combis, seq_len(nrow(combis)))
  results <- lapply(combis, computePsMetricsForAnalysisId, sampleFolders = sampleFolders)
  results <- bind_rows(results)
  readr::write_csv(results, outputFileName)
}

recodeTargetId <- function(data) {
   data %>%
    mutate(targetId = .data$targetId + 100 * .data$comparatorId) %>%
    return()
}

recodeExposureId <- function(data) {
  data %>%
    mutate(exposureId = .data$exposureId + 100 * .data$comparatorId) %>%
    return()
}
