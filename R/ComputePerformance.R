# Copyright 2020 Observational Health Data Sciences and Informatics
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
                               parentFolder = NULL, 
                               cmFolders,
                               maxCores = 1,
                               outputFileName) {
  controlSummary <- read.csv(file.path(outputFolder, "allControls.csv"))
  
  computeMetrics <- function(cmFolder) {
    estimates <- readRDS(file.path(cmFolder, "cmSummary.rds"))
    estimates <- controlSummary %>%
      select(.data$targetId, .data$outcomeId, .data$targetEffectSize, .data$trueEffectSize, .data$trueEffectSizeFirstExposure) %>%
      left_join(estimates,
                by = c("targetId", "outcomeId"))
    results <- lapply(split(estimates, estimates$analysisId), computeMetricsForAnalysis, folder = cmFolder)
    results <- do.call("rbind", results)
    results <- results %>%
      mutate(cmFolder = cmFolder)
    return(results)
  }
  results <- lapply(cmFolders, computeMetrics)
  results <- do.call("rbind", results)
  
  if (length(cmFolders) > 1) {
    # Also compute overall statistics by pooling strata files
    omr <- readRDS(file.path(cmFolders[1], "outcomeModelReference.rds"))
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(file.path(cmFolders[1], "cmAnalysisList.json"))
    
    getCmAnalysis <- function(analysisId) {
      for (cmAnalysis in cmAnalysisList) {
        if (cmAnalysis$analysisId == analysisId) {
          return(cmAnalysis)
        }
      }
      return(NULL)
    }
    
    computeOverallEstimates <- function(omrSubset) {
      ParallelLogger::logInfo("Computing overall estimates for analysis ID: ", omrSubset$analysisId[1])
      cmAnalysis <- getCmAnalysis(omrSubset$analysisId[1])
      cluster <- ParallelLogger::makeCluster(min(maxCores, 5))
      subset <- ParallelLogger::clusterApply(cluster, 
                                             1:nrow(omrSubset), 
                                             fitOverallOutcomeModel, 
                                             omrSubset = omrSubset, 
                                             cmAnalysis = cmAnalysis, 
                                             cmFolders = cmFolders)
      ParallelLogger::stopCluster(cluster)
      subset <- bind_rows(subset)
      return(subset)
    }
    overallEstimates <- lapply(split(omr, omr$analysisId), computeOverallEstimates)
    overallEstimates <- bind_rows(overallEstimates)
    saveRDS(overallEstimates, file.path(parentFolder, "cmSummary.rds"))
    
    estimates <- controlSummary %>%
      select(.data$targetId, .data$outcomeId, .data$targetEffectSize, .data$trueEffectSize, .data$trueEffectSizeFirstExposure) %>%
      left_join(overallEstimates,
                by = c("targetId", "outcomeId"))
    overallResults <- lapply(split(estimates, estimates$analysisId), computeMetricsForAnalysis, folder = parentFolder)
    overallResults <- bind_rows(overallResults)
    overallResults$cmFolder <- parentFolder
    results <- bind_rows(results, overallResults)
  }
  readr::write_csv(results, outputFileName)
}

fitOverallOutcomeModel <- function(rowIdx, omrSubset, cmAnalysis, cmFolders) {
  if (omrSubset$strataFile[rowIdx] != "") {
    strataPop <- lapply(cmFolders, function(x) readRDS(file.path(x, omrSubset$strataFile[rowIdx])))
  } else {
    strataPop <- lapply(cmFolders, function(x) readRDS(file.path(x, omrSubset$studyPopFile[rowIdx])))
  }
  if (cmAnalysis$fitOutcomeModelArgs$stratified) {
    highestId <- 0
    for (i in 1:length(strataPop)) {
      strataPop[[i]]$stratumId <- strataPop[[i]]$stratumId + highestId
      highestId <- max(strataPop[[i]]$stratumId) + 1
    } 
  } else {
    for (i in 1:length(strataPop)) {
      strataPop[[i]]$stratumId <- rep(i, nrow(strataPop[[i]]))
    }
  }
  strataPop <- dplyr::bind_rows(strataPop)
  fitOutcomeModelArgs <- cmAnalysis$fitOutcomeModelArgs
  fitOutcomeModelArgs$population <- strataPop
  # Always stratify by 'database':
  fitOutcomeModelArgs$stratified <- TRUE
  om <- do.call(CohortMethod::fitOutcomeModel, fitOutcomeModelArgs)
  if (is.null(coef(om))) {
    estimate <- dplyr::tibble(analysisId = omrSubset$analysisId[rowIdx],
                              targetId = omrSubset$targetId[rowIdx],
                              comparatorId = omrSubset$comparatorId[rowIdx],
                              outcomeId = omrSubset$outcomeId[rowIdx],
                              logRr = NA,
                              seLogRr = NA,
                              ci95Lb = NA,
                              ci95Ub = NA,
                              p = NA)
  } else { 
    estimate <- dplyr::tibble(analysisId = omrSubset$analysisId[rowIdx],
                              targetId = omrSubset$targetId[rowIdx],
                              comparatorId = omrSubset$comparatorId[rowIdx],
                              outcomeId = omrSubset$outcomeId[rowIdx],
                              logRr = om$outcomeModelTreatmentEstimate$logRr,
                              seLogRr = om$outcomeModelTreatmentEstimate$seLogRr,
                              ci95Lb = exp(om$outcomeModelTreatmentEstimate$logLb95),
                              ci95Ub = exp(om$outcomeModelTreatmentEstimate$logUb95),
                              p = EmpiricalCalibration::computeTraditionalP(om$outcomeModelTreatmentEstimate$logRr, 
                                                                            om$outcomeModelTreatmentEstimate$seLogRr))
  }
  return(estimate)
}

computeMetricsForAnalysis <- function(subset, folder = NULL) {
  metrics <- MethodEvaluation::computeMetrics(logRr = subset$logRr,
                                              seLogRr = subset$seLogRr,
                                              ci95Lb = subset$ci95Lb,
                                              ci95Ub = subset$ci95Ub,
                                              p = subset$p,
                                              trueLogRr = log(subset$trueEffectSizeFirstExposure))
  metrics <- as_tibble(t(metrics)) %>%
    mutate(analysisId = subset$analysisId[1])
  ncs <- subset[subset$targetEffectSize == 1, ]
  null <- EmpiricalCalibration::fitMcmcNull(logRr = ncs$logRr,
                                            seLogRr = ncs$seLogRr)
  ese <- EmpiricalCalibration::computeExpectedSystematicError(null)
  metrics$ese <- ese$expectedSystematicError
  metrics$eseLb <- ese$lb95ci
  metrics$eseUb <- ese$ub95ci
  
  if (!is.null(folder)) {
    ncPlotFileName <- file.path(folder, sprintf("ncs_a%d.png", subset$analysisId[1]))
    EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = ncs$logRr,
                                                seLogRrNegatives = ncs$seLogRr,
                                                null = null,
                                                showCis = TRUE,
                                                fileName = ncPlotFileName)
  }
  return(metrics)
}
