library(SmallSampleEstimationEvaluation)

options(andromedaTempFolder = "d:/andromedaTemp")

maxCores <- parallel::detectCores()

# MDCD
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))
oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_truven_mdcd_v2359"
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable <- "cohort_small_sample_eval"
outputFolder <- "d:/SmallSampleEstimationEvaluation_mdcd"
databaseId <- "MDCD"

# MDCR
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcr"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))
oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_truven_mdcr_v2322"
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable <- "cohort_small_sample_eval_mdcr"
outputFolder <- "d:/SmallSampleEstimationEvaluation_mdcr"
databaseId <- "MDCR"

# Optum EHR
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaOptumEhr"),
                                                                user = keyring::key_get("temp_user"),
                                                                password = keyring::key_get("temp_password"))
oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_optum_ehr_v2137"
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable <- "cohort_small_sample_eval_optum_ehr"
outputFolder <- "d:/SmallSampleEstimationEvaluation_optum_ehr"
databaseId <- "Optum EHR"

# JMDC
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaJmdc"),
                                                                user = keyring::key_get("temp_user"),
                                                                password = keyring::key_get("temp_password"))
oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_jmdc_v2432"
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable <- "cohort_small_sample_eval_jmdc"
outputFolder <- "d:/SmallSampleEstimationEvaluation_jmdc"
databaseId <- "JMDC"


# Run analyses ---------------------------------------------------
execute(connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        maxCores = maxCores,
        outputFolder = outputFolder,
        databaseId = databaseId,
        createCohorts = F) 
