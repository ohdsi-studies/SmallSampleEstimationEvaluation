library(Capr)
library(dplyr)
library(DatabaseConnector)
library(CirceR)
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
  user = keyring::key_get("redShiftUserName"),
  password = keyring::key_get("redShiftPassword")
)
cdmDatabaseSchema <- "cdm_truven_mdcd_v2359"
connection <- connect(connectionDetails)

# ACE inhibitors vs thiazides and thiazide-like diuretics -----------------------
hypertensiveDisorder <- cs(
  descendants(316866),
  name = "Hypertensive disorder"
)
hypertensiveDisorder <- getConceptSetDetails(hypertensiveDisorder, connection, cdmDatabaseSchema)
aceInhibitors <- cs(
  descendants(1308216,1310756,1331235,1334456,1335471,1340128,1341927,1342439,1363749,1373225),
  name = "ACE inhibitors"
)
aceInhibitors <- getConceptSetDetails(aceInhibitors, connection, cdmDatabaseSchema)
thiazides <- cs(
  descendants(907013,974166,978555,1395058),
  name = "Thiazide or thiazide-like diuretic"
)
thiazides <- getConceptSetDetails(thiazides, connection, cdmDatabaseSchema)
aceInhibitorsNewUsers <- cohort(
  entry = entry(
    drug(aceInhibitors, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(aceInhibitors, persistenceWindow = 30, surveillanceWindow = 0))
)
thiazidesNewUsers <- cohort(
  entry = entry(
    drug(thiazides, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(thiazides, persistenceWindow = 30, surveillanceWindow = 0))
)

# Sitagliptin vs liraglutide --------------------------------------------------------------
t2dm <- cs(
  descendants(443238, 201820, 442793), 
  descendants(exclude(195771, 201254, 435216, 761051, 4058243, 40484648)),
  name = "Type 2 diabetes mellitus (diabetes mellitus excluding T1DM and secondary)"
)
t2dm <- getConceptSetDetails(t2dm, connection, cdmDatabaseSchema)
sitagliptin <- cs(
  descendants(1580747),
  name = "Sitagliptin"
)
sitagliptin <- getConceptSetDetails(sitagliptin, connection, cdmDatabaseSchema)
liraglutide <- cs(
  descendants(40170911),
  name = "Liraglutide"
)
liraglutide <- getConceptSetDetails(liraglutide, connection, cdmDatabaseSchema)
sitagliptinNewUsers <- cohort(
  entry = entry(
    drug(sitagliptin, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior T2DM" = withAll(
      atLeast(1, condition(t2dm), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(sitagliptin, persistenceWindow = 30, surveillanceWindow = 0))
)
liraglutideNewUsers <- cohort(
  entry = entry(
    drug(liraglutide, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior T2DM" = withAll(
      atLeast(1, condition(t2dm), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(liraglutide, persistenceWindow = 30, surveillanceWindow = 0))
)

# Write to file ----------------------------------------------------------
cohortDefinitionSet <- tibble(
  cohortId = c(
    1, 
    2, 
    3, 
    4),
  cohortName = c(
    "ACE inhibitors",
    "Thiazide or thiazide-like diuretic",
    "Sitagplitin",
    "Liraglutide"
  ),
  json = c(
    as.json(aceInhibitorsNewUsers),
    as.json(thiazidesNewUsers),
    as.json(sitagliptinNewUsers),
    as.json(liraglutideNewUsers)
  ),
  sql = c(
    buildCohortQuery(as.json(aceInhibitorsNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(thiazidesNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(sitagliptinNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(liraglutideNewUsers), createGenerateOptions(generateStats = FALSE))
  )
)
disconnect(connection)
saveRDS(cohortDefinitionSet, "inst/CohortDefinitionSet.rds")

# Review --------------------------------------------------------------
writeLines(cohortPrintFriendly(as.json(sitagliptinNewUsers)))
writeLines(cohortPrintFriendly(as.json(liraglutideNewUsers)))
writeLines(cohortPrintFriendly(as.json(aceInhibitorsNewUsers)))
writeLines(cohortPrintFriendly(as.json(thiazidesNewUsers)))
sitagliptin
liraglutide
t2dm
hypertensiveDisorder

# ncs <- readRDS(system.file("ohdsiDevelopmentNegativeControls.rds", package = "MethodEvaluation"))
# readr::write_csv(ncs, "inst/NegativeControls.csv")
