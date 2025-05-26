library(DatabaseConnector)
options(andromedaTempFolder = "e:/andromedaTemp")

connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3326"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "bias_search"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
outputFolder <- "e:/BiasSearch"

maxCores <- parallel::detectCores()
