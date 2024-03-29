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
combineEstimates <- function(parentFolder,
                             cmFolders,
                             maxCores = 1) {
  fileName <- file.path(parentFolder, "resultsSummary.rds")
  if (file.exists(fileName)) {
    return()
  }
  omr <- CohortMethod::getFileReference(cmFolders[1])
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(file.path(cmFolders[1], "cmAnalysisList.json"))
  CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(parentFolder, "cmAnalysisList.json"))
  getCmAnalysis <- function(analysisId) {
    for (cmAnalysis in cmAnalysisList) {
      if (cmAnalysis$analysisId == analysisId) {
        return(cmAnalysis)
      }
    }
    return(NULL)
  }
  
  # omrSubset = split(omr, omr$analysisId)[[2]]
  computeOverallEstimates <- function(omrSubset) {
    message("Computing overall estimates for analysis ID: ", omrSubset$analysisId[1])
    cmAnalysis <- getCmAnalysis(omrSubset$analysisId[1])
    cluster <- ParallelLogger::makeCluster(min(maxCores, 10))
    ParallelLogger::clusterRequire(cluster, "dplyr")
    subset <- ParallelLogger::clusterApply(cluster,
                                           1:nrow(omrSubset),
                                           fitOverallOutcomeModel,
                                           omrSubset = omrSubset,
                                           cmAnalysis = cmAnalysis,
                                           cmFolders = cmFolders
    )
    ParallelLogger::stopCluster(cluster)
    subset <- bind_rows(subset)
    return(subset)
  }
  overallEstimates <- lapply(split(omr, omr$analysisId), computeOverallEstimates)
  overallEstimates <- bind_rows(overallEstimates)
  saveRDS(overallEstimates, fileName)
}

fitOverallOutcomeModel <- function(rowIdx, omrSubset, cmAnalysis, cmFolders) {
  
  # Pooling:
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
  strataPop <- bind_rows(strataPop)
  fitOutcomeModelArgs <- cmAnalysis$fitOutcomeModelArgs
  fitOutcomeModelArgs$population <- strataPop
  # Always stratify by 'database':
  fitOutcomeModelArgs$stratified <- TRUE
  om <- do.call(CohortMethod::fitOutcomeModel, fitOutcomeModelArgs)
  if (is.null(coef(om))) {
    estimate <- tibble(
      analysisId = omrSubset$analysisId[rowIdx],
      targetId = omrSubset$targetId[rowIdx],
      comparatorId = omrSubset$comparatorId[rowIdx],
      outcomeId = omrSubset$outcomeId[rowIdx],
      logRr = NA,
      seLogRr = NA,
      ci95Lb = NA,
      ci95Ub = NA,
      p = NA
    )
  } else {
    estimate <- tibble(
      analysisId = omrSubset$analysisId[rowIdx],
      targetId = omrSubset$targetId[rowIdx],
      comparatorId = omrSubset$comparatorId[rowIdx],
      outcomeId = omrSubset$outcomeId[rowIdx],
      logRr = om$outcomeModelTreatmentEstimate$logRr,
      seLogRr = om$outcomeModelTreatmentEstimate$seLogRr,
      ci95Lb = exp(om$outcomeModelTreatmentEstimate$logLb95),
      ci95Ub = exp(om$outcomeModelTreatmentEstimate$logUb95),
      p = EmpiricalCalibration::computeTraditionalP(
        om$outcomeModelTreatmentEstimate$logRr,
        om$outcomeModelTreatmentEstimate$seLogRr
      )
    )
  }
  
  # Random-effects meta-analysis ------------------------------------------
  outcomeModels <- lapply(cmFolders, function(x) readRDS(file.path(x, omrSubset$outcomeModelFile[rowIdx])))
  # i <- 6
  # outcomeModels[[i]]
  # EvidenceSynthesis::computeConfidenceInterval(outcomeModels[[i]]$logLikelihoodProfile)
  # plot(x = as.numeric(names(outcomeModels[[i]]$logLikelihoodProfile)), y = outcomeModels[[i]]$logLikelihoodProfile)
  profiles <- lapply(outcomeModels, function(x) x$logLikelihoodProfile)
  profiles[sapply(profiles, is.null)] <- NULL
  # profiles <- do.call("rbind", profiles)
  if (length(profiles) == 0) {
    randomFxEstimate <- tibble(
      analysisId = omrSubset$analysisId[rowIdx] + 100,
      targetId = omrSubset$targetId[rowIdx],
      comparatorId = omrSubset$comparatorId[rowIdx],
      outcomeId = omrSubset$outcomeId[rowIdx],
      logRr = NA,
      seLogRr = NA,
      ci95Lb = NA,
      ci95Ub = NA,
      p = NA
    )
  } else {
    randomFxEstimate <- EvidenceSynthesis::computeBayesianMetaAnalysis(profiles)
    randomFxEstimate <- tibble(
      analysisId = omrSubset$analysisId[rowIdx] + 100,
      targetId = omrSubset$targetId[rowIdx],
      comparatorId = omrSubset$comparatorId[rowIdx],
      outcomeId = omrSubset$outcomeId[rowIdx],
      logRr = randomFxEstimate$logRr,
      seLogRr = randomFxEstimate$seLogRr,
      ci95Lb = exp(randomFxEstimate$mu95Lb),
      ci95Ub = exp(randomFxEstimate$mu95Ub),
      p = EmpiricalCalibration::computeTraditionalP(
        randomFxEstimate$logRr,
        randomFxEstimate$seLogRr
      )
    )
    
    # fixedFxEstimate <- EvidenceSynthesis::computeFixedEffectMetaAnalysis(profiles)
  }
  estimate <- bind_rows(estimate, randomFxEstimate)
  
  # Traditional meta-analysis ---------------------------------------------------
  outcomeModels <- lapply(cmFolders, function(x) readRDS(file.path(x, omrSubset$outcomeModelFile[rowIdx])))
  getEstimate <- function(outcomeModel) {
    if (is.null(coef(outcomeModel)) || is.na(outcomeModel$outcomeModelTreatmentEstimate$seLogRr)) {
      return(NULL)
    } else {
      return(tibble(
        logRr = outcomeModel$outcomeModelTreatmentEstimate$logRr,
        seLogRr = outcomeModel$outcomeModelTreatmentEstimate$seLogRr
      ))
    }
  }
  
  ests <- lapply(outcomeModels, getEstimate)
  ests <- bind_rows(ests)
  if (nrow(ests) == 0) {
    traditionalEstimate <- tibble(
      analysisId = omrSubset$analysisId[rowIdx] + 200,
      targetId = omrSubset$targetId[rowIdx],
      comparatorId = omrSubset$comparatorId[rowIdx],
      outcomeId = omrSubset$outcomeId[rowIdx],
      logRr = NA,
      seLogRr = NA,
      ci95Lb = NA,
      ci95Ub = NA,
      p = NA
    )
  } else {
    meta <- meta::metagen(
      TE = ests$logRr,
      seTE = ests$seLogRr,
      studlab = rep("", nrow(ests)),
      byvar = NULL,
      sm = "RR",
      control = list(stepadj=0.5, maxiter=10000)
    )
    rfx <- summary(meta)$random
    traditionalEstimate <- tibble(
      analysisId = omrSubset$analysisId[rowIdx] + 200,
      targetId = omrSubset$targetId[rowIdx],
      comparatorId = omrSubset$comparatorId[rowIdx],
      outcomeId = omrSubset$outcomeId[rowIdx],
      logRr = rfx$TE,
      seLogRr = rfx$seTE,
      ci95Lb = exp(rfx$lower),
      ci95Ub = exp(rfx$upper),
      p = rfx$p
    )
  }
  estimate <- bind_rows(estimate, traditionalEstimate)
  return(estimate)
}
