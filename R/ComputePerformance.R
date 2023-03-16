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

  estimates <- readRDS(file.path(cmFolder, "cmSummary.rds"))
  colnames(estimates)[colnames(estimates) == "ci95lb"] <- "ci95Lb"
  colnames(estimates)[colnames(estimates) == "ci95ub"] <- "ci95Ub"

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
    databaseName = "MDCD",
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
  readr::write_csv(metrics, outputFileName)

  estimates <- controlSummary %>%
    select(.data$targetId, .data$outcomeId, .data$targetEffectSize, .data$trueEffectSize, .data$trueEffectSizeFirstExposure) %>%
    left_join(estimates,
      by = c("targetId", "outcomeId")
    ) %>%
    inner_join(analysisRef %>%
      select(.data$analysisId, .data$description),
    by = "analysisId"
    )


  invisible(lapply(split(estimates, estimates$analysisId), generatePlots, folder = cmFolder))
}

generatePlots <- function(subset, folder = NULL) {
  ncs <- subset[subset$targetEffectSize == 1, ]
  null <- EmpiricalCalibration::fitMcmcNull(
    logRr = ncs$logRr,
    seLogRr = ncs$seLogRr
  )
  # ease <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
  # metrics$ease <- ease$ease
  # metrics$easeLb <- ease$ciLb
  # metrics$easeUb <- ease$ciUb

  # nc <- ncs[ncs$outcomeId == 77965, ]
  # ncs <- ncs[ncs$outcomeId != 77965, ]
  # nc <- ncs[ncs$outcomeId == 373478, ]
  # ncs <- ncs[ncs$outcomeId != 77965, ]
  #
  # ncs1 <- estimates[estimates$analysisId == 3 & estimates$targetEffectSize == 1, ]
  # ncs2 <- estimates[estimates$analysisId == 5 & estimates$targetEffectSize == 1, ]
  #
  # combi <- inner_join(tibble(outcomeId = ncs1$outcomeId,
  #                            logRr1 = ncs1$logRr,
  #                            seLogRr1 = ncs1$seLogRr),
  #                     tibble(outcomeId = ncs2$outcomeId,
  #                            logRr2 = ncs2$logRr,
  #                            seLogRr2 = ncs2$seLogRr))
  # delta <- combi$logRr1 - combi$logRr2
  #
  # exp(nc$logRr)
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
  return(NULL)
}
