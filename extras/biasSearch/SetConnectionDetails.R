library(DatabaseConnector)
options(andromedaTempFolder = "e:/andromedaTemp")

connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3467"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "bias_search_3467"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
outputFolder <- "e:/BiasSearch_3467"

maxCores <- parallel::detectCores()
