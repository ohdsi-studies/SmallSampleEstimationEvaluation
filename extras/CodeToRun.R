library(SmallSampleEstimationEvaluation)

options(andromedaTempFolder = "d:/andromedaTemp")

maxCores <- parallel::detectCores()

# RedShift settings
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))
oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_truven_mdcd_v2359"
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable <- "cohort_small_sample_eval"
outputFolder <- "d:/SmallSampleEstimationEvaluation"
databaseId <- "MDCD"

execute(connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureDatabaseSchema = exposureDatabaseSchema,
        exposureTable = exposureTable,
        outcomeDatabaseSchema = outcomeDatabaseSchema,
        outcomeTable = outcomeTable,
        nestingDatabaseSchema = nestingDatabaseSchema,
        nestingTable = nestingTable,
        maxCores = maxCores,
        referenceSet = referenceSet,
        outputFolder = outputFolder,
        databaseId = databaseId,
        createCohorts = FALSE) 
