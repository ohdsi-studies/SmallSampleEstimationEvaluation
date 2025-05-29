library(SmallSampleEstimationEvaluation)

options(andromedaTempFolder = "e:/andromedaTemp")

maxCores <- parallel::detectCores()

# CCAE
connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3467"

oracleTempSchema <- NULL
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable <- "cohort_small_sample_eval_ccae"
outputFolder <- "e:/SmallSampleEstimationEvaluation_ccae"
databaseId <- "CCAE"

# Optum EHR
connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
cdmDatabaseSchema <- "optum_ehr.cdm_optum_ehr_v3471"

oracleTempSchema <- NULL
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable <- "cohort_small_sample_eval_optum_ehr"
outputFolder <- "e:/SmallSampleEstimationEvaluation_optum_ehr"
databaseId <- "Optum EHR"


# Run analyses ---------------------------------------------------
execute(connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        maxCores = maxCores,
        outputFolder = outputFolder,
        databaseId = databaseId,
        createCohorts = FALSE) 
