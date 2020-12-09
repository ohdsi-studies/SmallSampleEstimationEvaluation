library(SmallSampleEstimationEvaluation)

referenceSet <- "ohdsiDevelopment"

maxCores <- parallel::detectCores()

# RedShift settings
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))

Sys.setenv("AWS_OBJECT_KEY" = "bulk")
Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("bulkUploadS3Key"))
Sys.setenv("AWS_SECRET_ACCESS_KEY" = Sys.getenv("bulkUploadS3Secret"))
Sys.setenv("AWS_BUCKET_NAME" = Sys.getenv("bulkUploadS3Bucket"))
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-1")
Sys.setenv("AWS_SSE_TYPE" = "AES256")
Sys.setenv("USE_MPP_BULK_LOAD" = TRUE)

oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm"
exposureDatabaseSchema <- "scratch_mschuemi2"
exposureTable <- "mschuemie_cohorts_temp"
outcomeDatabaseSchema <- exposureDatabaseSchema
outcomeTable <- exposureTable
nestingDatabaseSchema <- exposureDatabaseSchema
nestingTable <- exposureTable
cdmVersion <- "5"
outputFolder <- "s:/SmallSampleEstimationEvaluation"



