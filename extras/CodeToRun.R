library(SmallSampleEstimationEvaluation)

referenceSet <- "ohdsiDevelopment"

maxCores <- parallel::detectCores()

# RedShift settings
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))
oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_truven_mdcd_v2359"
exposureDatabaseSchema <- "scratch_mschuemi"
exposureTable <- "cohort_small_sample_eval"
outcomeDatabaseSchema <- exposureDatabaseSchema
outcomeTable <- exposureTable
nestingDatabaseSchema <- exposureDatabaseSchema
nestingTable <- exposureTable
cdmVersion <- "5"
outputFolder <- "d:/SmallSampleEstimationEvaluation"



