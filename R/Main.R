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
#' @param nestingDatabaseSchema   (For the OHDSI Methods Benchmark and OHDSI Development Set only) The
#'                                database schema where the nesting outcome table is located. Note that
#'                                for SQL Server, both the database and schema should be specified, e.g.
#'                                 'cdm_schema.dbo'.
#' @param nestingTable            (For the OHDSI Methods Benchmark and OHDSI Development Set only) The
#'                                name of the table where the nesting cohorts will be stored.
#' @param maxCores                How many parallel cores should be used? If more cores are made available
#'                                this can speed up the analyses.
#' @param referenceSet            The name of the reference set for which outcomes need to be created.
#'                                Currently supported are "ohdsiMethodsBenchmark", and "ohdsiDevelopment".
#' @param outputFolder            Name of local folder to place intermediary results; make sure to use
#'                                forward slashes (/). Do not use a folder on a network drive since
#'                                this greatly impacts performance.
#'
#' @export
execute <- function(connectionDetails,
                    oracleTempSchema = NULL,
                    cdmDatabaseSchema,
                    exposureDatabaseSchema = cdmDatabaseSchema,
                    exposureTable = "exposures",
                    outcomeDatabaseSchema = cdmDatabaseSchema,
                    outcomeTable = "outcomes",
                    nestingDatabaseSchema = cdmDatabaseSchema,
                    nestingTable = "nesting",
                    maxCores = 1,
                    referenceSet = "ohdsiMethodsBenchmark",
                    outputFolder,
                    createCohorts = TRUE) {
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  if (createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    createCohorts(
      connectionDetails = connectionDetails,
      oracleTempSchema = oracleTempSchema,
      cdmDatabaseSchema = cdmDatabaseSchema,
      exposureDatabaseSchema = exposureDatabaseSchema,
      exposureTable = exposureTable,
      outcomeDatabaseSchema = outcomeDatabaseSchema,
      outcomeTable = outcomeTable,
      nestingDatabaseSchema = nestingDatabaseSchema,
      nestingTable = nestingTable,
      referenceSet = referenceSet,
      outputFolder = outputFolder
    )
  }

  # Run on full data:
  fullDataFolder <- file.path(outputFolder, "fullData")
  if (!file.exists(fullDataFolder)) {
    dir.create(fullDataFolder)
  }

  runCohortMethod(
    connectionDetails = connectionDetails,
    oracleTempSchema = oracleTempSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema,
    outcomeTable = outcomeTable,
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    maxCores = maxCores,
    cmFolder = fullDataFolder
  )

  computePerformance(
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    cmFolder = fullDataFolder,
    maxCores = maxCores,
    outputFileName = file.path(outputFolder, "Metrics_FullData.csv")
  )

  # Create large sample:
  largeSampleFolder <- file.path(outputFolder, "largeSample")
  if (!file.exists(largeSampleFolder)) {
    dir.create(largeSampleFolder)
  }
  samplePopulation(
    sourceCmFolder = fullDataFolder,
    sampleFolder = largeSampleFolder,
    sampleSize = 20000,
    seed = 123
  )

  runCohortMethod(
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    cmFolder = largeSampleFolder,
    maxCores = maxCores,
    externalPsFolder = fullDataFolder
  )

  computePerformance(
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    cmFolder = largeSampleFolder,
    maxCores = maxCores,
    outputFileName = file.path(outputFolder, "Metrics_LargeSample.csv")
  )

  # Split large sample in 1k samples:
  oneKSamplesFolder <- file.path(outputFolder, "oneKSamples")
  if (!file.exists(oneKSamplesFolder)) {
    dir.create(oneKSamplesFolder)
  }
  samplePopulation(
    sourceCmFolder = largeSampleFolder,
    sampleFolder = oneKSamplesFolder,
    numberOfSamples = 20,
    seed = 123
  )
  oneKSampleSubFolders <- file.path(oneKSamplesFolder, sprintf("Sample_%d", 1:20))
  for (oneKSampleSubFolder in oneKSampleSubFolders) {
    ParallelLogger::logInfo("Performing CohortMethod analyses in ", oneKSampleSubFolder)
    runCohortMethod(
      referenceSet = referenceSet,
      outputFolder = outputFolder,
      cmFolder = oneKSampleSubFolder,
      maxCores = maxCores,
      externalPsFolder = fullDataFolder
    )
  }
  computePerformance(
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    cmFolder = oneKSampleSubFolders[1],
    maxCores = maxCores,
    outputFileName = file.path(outputFolder, "Metrics_OneKSample_1.csv")
  )

  combineEstimates(
    parentFolder = oneKSamplesFolder,
    cmFolders = oneKSampleSubFolders,
    maxCores = maxCores
  )

  computePerformance(
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    cmFolder = oneKSamplesFolder,
    maxCores = maxCores,
    outputFileName = file.path(outputFolder, "Metrics_OneKSample.csv")
  )

  # Split large sample in 0.5k samples:
  halfKSamplesFolder <- file.path(outputFolder, "halfKSamples")
  if (!file.exists(halfKSamplesFolder)) {
    dir.create(halfKSamplesFolder)
  }
  samplePopulation(
    sourceCmFolder = largeSampleFolder,
    sampleFolder = halfKSamplesFolder,
    numberOfSamples = 40,
    seed = 123
  )
  halfKSampleSubFolders <- file.path(halfKSamplesFolder, sprintf("Sample_%d", 1:40))
  for (halfKSampleSubFolder in halfKSampleSubFolders) {
    ParallelLogger::logInfo("Performing CohortMethod analyses in ", halfKSampleSubFolder)
    runCohortMethod(
      referenceSet = referenceSet,
      outputFolder = outputFolder,
      cmFolder = halfKSampleSubFolder,
      maxCores = maxCores,
      externalPsFolder = fullDataFolder
    )
  }
  computePerformance(
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    cmFolder = halfKSampleSubFolders[1],
    maxCores = maxCores,
    outputFileName = file.path(outputFolder, "Metrics_halfKSample_1.csv")
  )

  combineEstimates(
    parentFolder = halfKSamplesFolder,
    cmFolders = halfKSampleSubFolders,
    maxCores = maxCores
  )

  computePerformance(
    referenceSet = referenceSet,
    outputFolder = outputFolder,
    cmFolder = halfKSamplesFolder,
    maxCores = maxCores,
    outputFileName = file.path(outputFolder, "Metrics_halfKSample.csv")
  )
}
