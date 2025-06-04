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

#' Run the main analysis
#'
#' @param connectionDetails       An R object of type \code{ConnectionDetails} created using the
#'                                function \code{createConnectionDetails} in the
#'                                \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema       A database schema containing health care data in the OMOP Commond
#'                                Data Model. Note that for SQL Server, botth the database and schema
#'                                should be specified, e.g. 'cdm_schema.dbo'.
#' @param cohortDatabaseSchema    The name of the database schema where the exposure and outcome cohorts will be
#'                                created. 
#' @param cohortTable             The name of the table that will be created to store the exposure
#'                                and outcome cohorts. 
#' @param maxCores                How many parallel cores should be used? If more cores are made available
#'                                this can speed up the analyses.
#' @param outputFolder            Name of local folder to place intermediary results; make sure to use
#'                                forward slashes (/). Do not use a folder on a network drive since
#'                                this greatly impacts performance.
#' @param databaseId              A string used to identify the database in the results.
#' @param createCohorts           Should the cohorts be created? If `FALSE`, the cohorts are assumed to already
#'                                exist.
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable,
                    maxCores = 1,
                    outputFolder,
                    databaseId,
                    createCohorts = TRUE) {
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  if (createCohorts) {
    message("Creating cohorts")
    createCohorts(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outputFolder = outputFolder,
      maxCores = maxCores
    )
  }
  
  # Run on full data -----------------------------------------------
  fullDataFolder <- file.path(outputFolder, "fullData")
  if (!file.exists(fullDataFolder)) {
    dir.create(fullDataFolder)
  }
  runCohortMethod(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    outputFolder = outputFolder,
    maxCores = maxCores,
    cmFolder = fullDataFolder
  )

  
  # Create large sample ------------------------------------------------------
  largeSampleSize <- 20000
  largeSampleFolder <- file.path(outputFolder, "largeSample")
  samplePopulation(
    sourceCmFolder = fullDataFolder,
    sampleFolder = largeSampleFolder,
    sampleSize = largeSampleSize,
    seed = 123
  )
  runCohortMethod(
    outputFolder = outputFolder,
    cmFolder = largeSampleFolder,
    maxCores = maxCores,
    externalPsFolder = fullDataFolder
  )
  computeMdrr(
    outputFolder = outputFolder,
    cmFolder = largeSampleFolder,
    connectionDetails = connectionDetails, 
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable
  )
  computePerformance(
    outputFolder = outputFolder,
    cmFolder = fullDataFolder,
    maxCores = maxCores,
    databaseId = databaseId,
    outputFileName = file.path(outputFolder, "Metrics_FullData.csv")
  )
  computePerformance(
    outputFolder = outputFolder,
    cmFolder = largeSampleFolder,
    maxCores = maxCores,
    databaseId = databaseId,
    outputFileName = file.path(outputFolder, "Metrics_LargeSample.csv")
  )

  # Take smaller sample sizes ------------------------------------------------------
  sampleSizes <- c(4000, 2000, 1000, 500, 250)
  # sampleSizes <- c(4000)
  for (sampleSize in sampleSizes) {
    numberOfSamples <- largeSampleSize / sampleSize
    smallSamplesFolder <- file.path(outputFolder, sprintf("smallSample%d", sampleSize))
    samplePopulation(
      sourceCmFolder = largeSampleFolder,
      sampleFolder = smallSamplesFolder,
      numberOfSamples = numberOfSamples,
      seed = 123
    )
    smallSampleSubFolders <- file.path(smallSamplesFolder, sprintf("Sample_%d", seq_len(numberOfSamples)))
    for (smallSampleSubFolder in smallSampleSubFolders) {
      message("Performing CohortMethod analyses in ", smallSampleSubFolder)
      runCohortMethod(
        outputFolder = outputFolder,
        cmFolder = smallSampleSubFolder,
        maxCores = maxCores,
        externalPsFolder = fullDataFolder,
        small = TRUE
      )
    }
    combineEstimates(
      parentFolder = smallSamplesFolder,
      cmFolders = smallSampleSubFolders,
      maxCores = maxCores
    )
    computePerformance(
      outputFolder = outputFolder,
      cmFolder = smallSamplesFolder,
      maxCores = maxCores,
      databaseId = databaseId,
      outputFileName = file.path(outputFolder, sprintf("Metrics_sample_%d.csv", sampleSize))
    )
    computePsMetrics(
      sampleFolders = smallSampleSubFolders,
      outputFileName = file.path(outputFolder, sprintf("PsMetrics_sample_%d.csv", sampleSize))
    )
  }
}
