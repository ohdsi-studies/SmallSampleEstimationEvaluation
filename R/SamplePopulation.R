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

#' @export
samplePopulation <- function(sourceCmFolder,
                             sampleFolder,
                             numberOfSamples = NULL,
                             sampleSize = NULL) {
  set.seed(123)
  
  subsetCmData <- function(cmData, sampleRowIds, outputFileName) {
    sampleCmData <- Andromeda::andromeda()
    sampleCmData$cohorts <- cmData$cohorts %>%
      filter(.data$rowId %in% sampleRowIds)
    sampleCmData$covariates <- cmData$covariates %>%
      filter(.data$rowId %in% sampleRowIds)
    sampleCmData$outcomes <- cmData$outcomes %>%
      filter(.data$rowId %in% sampleRowIds)
    sampleCmData$covariateRef <- cmData$covariateRef
    sampleCmData$analysisRef <- cmData$analysisRef
    metaData <- attr(cmData, "metaData")
    metaData$populationSize <- length(sampleRowIds)
    targetSize <- sum(pull(sampleCmData$cohorts, .data$treatment))
    metaData$attrition <- rbind(metaData$attrition,
                                tibble(description = "Random sample",
                                       targetPersons = targetSize,
                                       comparatorPersons = length(sampleRowIds) - targetSize,
                                       targetExposures  = targetSize,
                                       comparatorExposures = length(sampleRowIds) - targetSize))
    attr(sampleCmData, "metaData") <- metaData
    class(sampleCmData) <- class(cmData)
    CohortMethod::saveCohortMethodData(sampleCmData, outputFileName)
  }
  
  if (is.null(numberOfSamples)) {
    cmDataFiles <- list.files(sourceCmFolder, "CmData")
    for (cmDataFile in cmDataFiles) {
      cmData <- CohortMethod::loadCohortMethodData(file.path(sourceCmFolder, cmDataFile))
      rowIds <- cmData$cohorts %>%
        pull(.data$rowId)
      #TODO: need to handle case when original is smaller than requested sample size
      sampleRowIds <- sample(rowIds, sampleSize, replace = FALSE)
      subsetCmData(cmData = cmData, 
                   sampleRowIds = sampleRowIds, 
                   outputFileName = file.path(sampleFolder, cmDataFile))
    }
  } else {
    cmDataFiles <- list.files(sourceCmFolder, "CmData")
    for (cmDataFile in cmDataFiles) {
      cmData <- CohortMethod::loadCohortMethodData(file.path(sourceCmFolder, cmDataFile))
      rowIds <- cmData$cohorts %>%
        pull(.data$rowId)
      #TODO: need to handle case when original is smaller than requested sample size
      # Create equally-sized non-overlapping random samples without replacement:
      rnd <- runif(length(rowIds))
      breaks <- quantile(rnd, (1:(numberOfSamples - 1))/numberOfSamples)
      breaks <- unique(c(0, breaks, 1))
      sampleId <- as.integer(as.character(cut(rnd,
                                               breaks = breaks,
                                               labels = 1:(length(breaks) - 1))))
      pb <- txtProgressBar(style = 3)
      for (i in 1:numberOfSamples) {
        sampleRowIds <- rowIds[sampleId == i]
        sampleSubFolder <- file.path(sampleFolder, sprintf("Sample_%d", i))
        if (!file.exists(sampleSubFolder))
          dir.create(sampleSubFolder)
        subsetCmData(cmData = cmData, 
                     sampleRowIds = sampleRowIds, 
                     outputFileName = file.path(sampleSubFolder, cmDataFile))
        setTxtProgressBar(pb, i/numberOfSamples)
      }
      close(pb)
    }
  }
}

