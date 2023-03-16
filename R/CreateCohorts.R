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

#' Create cohorts used for the evaluation.
#'
#' @details
#' This function will create the outcomes of interest and nesting cohorts referenced in the various
#' reference sets. The outcomes of interest are derives using information like diagnoses, procedures,
#' and drug prescriptions. The outcomes are stored in a table on the database server.
#'
#' For the 'ohdsiMethodsBenchmark' reference set, the exposures are taken from the drug_era table, and
#' are therefore not generated as separate cohorts, and an exposure cohort table therefore needn't be supplied.
#' For the 'ohdsiDevelopment' reference set, exposure cohorts will be generated.
#'
#' @param connectionDetails       An R object of type \code{ConnectionDetails} created using the
#'                                function \code{createConnectionDetails} in the
#'                                \code{DatabaseConnector} package.
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
#' @param referenceSet            The name of the reference set for which outcomes need to be created.
#'                                Currently supported are "ohdsiMethodsBenchmark", and "ohdsiDevelopment".
#' @param outputFolder            Name of local folder to place intermediary results; make sure to use
#'                                forward slashes (/). Do not use a folder on a network drive since
#'                                this greatly impacts performance.

#'
#' @export
createCohorts <- function(connectionDetails,
                          oracleTempSchema = NULL,
                          cdmDatabaseSchema,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "exposures",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "outcomes",
                          nestingDatabaseSchema = cdmDatabaseSchema,
                          nestingTable = "nesting",
                          referenceSet = "ohdsiMethodsBenchmark",
                          outputFolder) {
  MethodEvaluation::createReferenceSetCohorts(connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema,
    outcomeTable = outcomeTable,
    nestingDatabaseSchema = nestingDatabaseSchema,
    nestingTable = nestingTable,
    referenceSet = referenceSet,
    workFolder = outputFolder
  )
  MethodEvaluation::synthesizeReferenceSetPositiveControls(connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema,
    outcomeTable = outcomeTable,
    referenceSet = referenceSet,
    maxCores = maxCores,
    workFolder = outputFolder
  )
}
