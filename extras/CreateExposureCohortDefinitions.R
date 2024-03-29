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

# Lisinopril, hydrochlorothiazide, metoprolol -----------------------

# Concept sets
hypertensiveDisorder <- cs(
  descendants(316866),
  name = "Hypertensive disorder"
)
hypertensiveDisorder <- getConceptSetDetails(hypertensiveDisorder, connection, cdmDatabaseSchema)
lisinopril <- cs(
  descendants(1308216),
  name = "Lisinopril"
)
lisinopril <- getConceptSetDetails(lisinopril, connection, cdmDatabaseSchema)
hydrochlorothiazide <- cs(
  descendants(974166),
  name = "Hydrochlorothiazide"
)
hydrochlorothiazide <- getConceptSetDetails(hydrochlorothiazide, connection, cdmDatabaseSchema)
metoprolol <- cs(
  descendants(1307046),
  name = "Metoprolol"
)
metoprolol <- getConceptSetDetails(metoprolol, connection, cdmDatabaseSchema)

# Cohorts
lisinoprilNewUsers <- cohort(
  entry = entry(
    drug(lisinopril, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(lisinopril, persistenceWindow = 30, surveillanceWindow = 0))
)
hydrochlorothiazideNewUsers <- cohort(
  entry = entry(
    drug(hydrochlorothiazide, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(hydrochlorothiazide, persistenceWindow = 30, surveillanceWindow = 0))
)
metoprololNewUsers <- cohort(
  entry = entry(
    drug(metoprolol, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(metoprolol, persistenceWindow = 30, surveillanceWindow = 0))
)

# Sitagliptin, liraglutide, glimepiride --------------------------------------------------------------

# Concept sets
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
glimepiride <- cs(
  descendants(1597756),
  name = "Glimepiride"
)
glimepiride <- getConceptSetDetails(glimepiride, connection, cdmDatabaseSchema)

# Cohorts
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
glimepirideNewUsers <- cohort(
  entry = entry(
    drug(glimepiride, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior T2DM" = withAll(
      atLeast(1, condition(t2dm), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(glimepiride, persistenceWindow = 30, surveillanceWindow = 0))
)

# Write to file ----------------------------------------------------------
cohortDefinitionSet <- tibble(
  cohortId = c(
    1, 
    2,
    3,
    4, 
    5,
    6
  ),
  cohortName = c(
    "Lisinopril",
    "Hydrochlorothiazide",
    "Metoprolol",
    "Sitagplitin",
    "Liraglutide",
    "Glimepiride"
  ),
  json = c(
    as.json(lisinoprilNewUsers),
    as.json(hydrochlorothiazideNewUsers),
    as.json(metoprololNewUsers),
    as.json(sitagliptinNewUsers),
    as.json(liraglutideNewUsers),
    as.json(glimepirideNewUsers)
  ),
  sql = c(
    buildCohortQuery(as.json(lisinoprilNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(hydrochlorothiazideNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(metoprololNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(sitagliptinNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(liraglutideNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(glimepirideNewUsers), createGenerateOptions(generateStats = FALSE))
  )
)
disconnect(connection)
saveRDS(cohortDefinitionSet, "inst/CohortDefinitionSet.rds")

# Review --------------------------------------------------------------
writeLines(cohortPrintFriendly(as.json(lisinoprilNewUsers)))
writeLines(cohortPrintFriendly(as.json(hydrochlorothiazideNewUsers)))
writeLines(cohortPrintFriendly(as.json(metoprololNewUsers)))
writeLines(cohortPrintFriendly(as.json(sitagliptinNewUsers)))
writeLines(cohortPrintFriendly(as.json(liraglutideNewUsers)))
writeLines(cohortPrintFriendly(as.json(glimepirideNewUsers)))
lisinopril
hydrochlorothiazide
metoprolol
hypertensiveDisorder
sitagliptin
liraglutide
glimepiride
t2dm


# ncs <- readRDS(system.file("ohdsiDevelopmentNegativeControls.rds", package = "MethodEvaluation"))
# readr::write_csv(ncs, "inst/NegativeControls.csv")
