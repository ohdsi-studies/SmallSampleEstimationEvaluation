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

#' Compute performance metrics of a run
#'
#' @param referenceSet            The name of the reference set for which outcomes need to be created.
#'                                Currently supported are "ohdsiMethodsBenchmark", and "ohdsiDevelopment".
#' @param outputFolder            Name of local folder to place intermediary results; make sure to use
#'                                forward slashes (/). Do not use a folder on a network drive since
#' @param cmFolders               Name of local folders where the CohortMethod runs can be found.
#' @param maxCores                How many parallel cores should be used? If more cores are made available
#'                                this can speed up the analyses.
#'
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
    select("targetId", outcomeId = "outcomeConceptId") %>%
    mutate(type = "Outcome control")
  synthesisSummary <- readRDS(file.path(outputFolder, "SignalInjection", "injectionSummary.rds"))
  estimates <- CohortMethod::getResultsSummary(cmFolder)
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
    estimates = estimates,
    negativeControls = negativeControls,
    synthesisSummary = synthesisSummary,
    analysisRef = analysisRef,
    databaseName = databaseId,
    exportFolder = cmFolder
  )
  metrics <- tibble()
  targetIds <- unique(synthesisSummary$exposureId)
  for (targetId in targetIds) {
    metricsUncalibrated <- MethodEvaluation::computeOhdsiBenchmarkMetrics(
      exportFolder = cmFolder,
      mdrr = "All",
      calibrated = FALSE,
      comparative = TRUE,
      stratum = targetId
    )
    metricsUncalibrated$calibrated <- FALSE
    metricsUncalibrated$targetId <- targetId
    metrics <- bind_rows(metrics, metricsUncalibrated)
    metricsCalibrated <- MethodEvaluation::computeOhdsiBenchmarkMetrics(
      exportFolder = cmFolder,
      mdrr = "All",
      calibrated = TRUE,
      comparative = TRUE,
      stratum = targetId
    )
    metricsCalibrated$calibrated <- TRUE
    metricsCalibrated$targetId <- targetId
    metrics <- bind_rows(metrics, metricsCalibrated)
  }
  subsets <- estimates %>%
    inner_join(negativeControls, by = join_by("targetId", "outcomeId")) %>%
    inner_join(analysisRef %>%
                 select("analysisId", "description"),
               by = "analysisId"
    )%>%
    group_by(.data$analysisId, .data$targetId) %>%
    group_split()
  ease <- lapply(subsets, generatePlotsAndComputeEase, folder = cmFolder) %>%
    bind_rows() %>%
    select(-"comparatorId") %>%
    mutate(calibrated = FALSE)
  metrics <- metrics %>%
    left_join(ease, by = join_by("analysisId", "targetId", "calibrated"))
  readr::write_csv(metrics, outputFileName)
}

# ncs = subsets[[1]]
generatePlotsAndComputeEase <- function(ncs, folder = NULL) {
  null <- EmpiricalCalibration::fitMcmcNull(
    logRr = ncs$logRr,
    seLogRr = ncs$seLogRr
  )
  if (!is.null(folder)) {
    ncPlotFileName <- file.path(folder, sprintf("ncs_t%d_a%d.png", ncs$targetId[1], ncs$analysisId[1]))
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
    filter(analysisId == row$analysisId, targetId == row$targetId) %>%
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
           targetId = row$targetId)
  return(result)
}

computePsMetrics <- function(sampleFolders, outputFileName) {
  if (file.exists(outputFileName)) {
    return()
  }
  combis <- expand.grid(
    analysisIds = c(1, 3),
    targetIds = c(1, 3)
  )
  combis <- split(combis, seq_len(nrow(combis)))
  results <- lapply(combis, computePsMetricsForAnalysisId, sampleFolders = sampleFolders)
  results <- bind_rows(results)
  readr::write_csv(results, outputFileName)
}
