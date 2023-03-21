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
computePerformance <- function(referenceSet = "ohdsiMethodsBenchmark",
                               outputFolder,
                               cmFolder,
                               maxCores = 1,
                               outputFileName) {
  controlSummary <- read.csv(file.path(outputFolder, "allControls.csv"))
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

  MethodEvaluation::packageOhdsiBenchmarkResults(
    estimates = estimates,
    controlSummary = controlSummary,
    analysisRef = analysisRef,
    databaseName = databaseId,
    exportFolder = cmFolder,
    referenceSet = referenceSet
  )

  metricsUncalibrated <- MethodEvaluation::computeOhdsiBenchmarkMetrics(
    exportFolder = cmFolder,
    mdrr = "All",
    calibrated = FALSE,
    comparative = TRUE
  )
  metricsUncalibrated$calibrated <- FALSE
  metricsCalibrated <- MethodEvaluation::computeOhdsiBenchmarkMetrics(
    exportFolder = cmFolder,
    mdrr = "All",
    calibrated = TRUE,
    comparative = TRUE
  )
  metricsCalibrated$calibrated <- TRUE
  metrics <- bind_rows(metricsUncalibrated, metricsCalibrated)
  
  estimates <- controlSummary %>%
    select("targetId", "outcomeId", "targetEffectSize", "trueEffectSize", "trueEffectSizeFirstExposure") %>%
    left_join(estimates,
      by = c("targetId", "outcomeId"),
      multiple = "all",
    ) %>%
    inner_join(analysisRef %>%
      select("analysisId", "description"),
    by = "analysisId"
    )


  ease <- lapply(split(estimates, estimates$analysisId), generatePlotsAndComputeEase, folder = cmFolder)
  ease <- bind_rows(ease) %>%
    select(-"targetId", -"comparatorId") %>%
    mutate(calibrated = FALSE)
  # TODO: handle case when we have multiple targets and comparators
  metrics <- metrics %>%
    left_join(ease, by = join_by("analysisId", "calibrated"))
  
  readr::write_csv(metrics, outputFileName)
}

# subset = split(estimates, estimates$analysisId)[[1]]
generatePlotsAndComputeEase <- function(subset, folder = NULL) {
  ncs <- subset[subset$targetEffectSize == 1, ]
  null <- EmpiricalCalibration::fitMcmcNull(
    logRr = ncs$logRr,
    seLogRr = ncs$seLogRr
  )
  if (!is.null(folder)) {
    ncPlotFileName <- file.path(folder, sprintf("ncs_a%d.png", subset$analysisId[1]))
    EmpiricalCalibration::plotCalibrationEffect(
      logRrNegatives = ncs$logRr,
      seLogRrNegatives = ncs$seLogRr,
      null = null,
      showCis = TRUE,
      showExpectedAbsoluteSystematicError = TRUE,
      title = subset$description[1],
      fileName = ncPlotFileName
    )
  }
  easeResult <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
  easeResult <- subset %>%
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
