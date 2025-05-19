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

#hydrochlorothiazide, losartan, quinapril, propranolol -----------------------

# Concept sets
hypertensiveDisorder <- cs(
  descendants(316866),
  name = "Hypertensive disorder"
)
hypertensiveDisorder <- getConceptSetDetails(hypertensiveDisorder, connection, cdmDatabaseSchema)
hydrochlorothiazide <- cs(
  descendants(974166),
  name = "Hydrochlorothiazide"
)
hydrochlorothiazide <- getConceptSetDetails(hydrochlorothiazide, connection, cdmDatabaseSchema)
losartan <- cs(
  descendants(1367500),
  name = "Losartan"
)
losartan <- getConceptSetDetails(losartan, connection, cdmDatabaseSchema)
quinapril <- cs(
  descendants(1331235),
  name = "Quinapril"
)
quinapril <- getConceptSetDetails(quinapril, connection, cdmDatabaseSchema)
propranolol <- cs(
  descendants(1353766),
  name = "Propranolol"
)
propranolol <- getConceptSetDetails(propranolol, connection, cdmDatabaseSchema)

# Cohorts
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
losartanNewUsers <- cohort(
  entry = entry(
    drug(losartan, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(losartan, persistenceWindow = 30, surveillanceWindow = 0))
)
quinaprilNewUsers <- cohort(
  entry = entry(
    drug(quinapril, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
    exit = exit(endStrategy = drugExit(quinapril, persistenceWindow = 30, surveillanceWindow = 0))
)
propranololNewUsers <- cohort(
  entry = entry(
    drug(propranolol, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior hypertensive disorder" = withAll(
      atLeast(1, condition(hypertensiveDisorder), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(propranolol, persistenceWindow = 30, surveillanceWindow = 0))
)

# Sitagliptin, dapagliflozin, glimepiride, saxagliptin ----------------------

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
dapagliflozin <- cs(
  descendants(44785829),
  name = "Dapagliflozin"
)
dapagliflozin <- getConceptSetDetails(dapagliflozin, connection, cdmDatabaseSchema)
glimepiride <- cs(
  descendants(1597756),
  name = "Glimepiride"
)
glimepiride <- getConceptSetDetails(glimepiride, connection, cdmDatabaseSchema)
saxagliptin <- cs(
  descendants(40166035),
  name = "Saxagliptin"
)
saxagliptin <- getConceptSetDetails(saxagliptin, connection, cdmDatabaseSchema)

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
dapagliflozinNewUsers <- cohort(
  entry = entry(
    drug(dapagliflozin, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior T2DM" = withAll(
      atLeast(1, condition(t2dm), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(dapagliflozin, persistenceWindow = 30, surveillanceWindow = 0))
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
saxagliptinNewUsers <- cohort(
  entry = entry(
    drug(saxagliptin, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior T2DM" = withAll(
      atLeast(1, condition(t2dm), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(saxagliptin, persistenceWindow = 30, surveillanceWindow = 0))
)

# Nortriptyline, fluoxetine, amitriptyline, venlafaxine ----------------------

# Concept sets

#Depression, including: severe major depression, severe depression, recurrent major depression
#recurrent depression, moderate major depression, moderate depression, mild major depression, mild depression
#major depression single episode
depression <- cs(
  descendants(42872722, 4149321, 4282316, 4098302, 4307111, 4151170, 4336957, 4149320, 4282096), 
  name = "Depression"
)

depression <- getConceptSetDetails(depression, connection, cdmDatabaseSchema)
nortriptyline <- cs(
  descendants(721724),
  name = "Nortriptyline"
)
nortriptyline <- getConceptSetDetails(nortriptyline, connection, cdmDatabaseSchema)
fluoxetine <- cs(
  descendants(755695),
  name = "Fluoxetine"
)
fluoxetine <- getConceptSetDetails(fluoxetine, connection, cdmDatabaseSchema)
amitriptyline <- cs(
  descendants(710062),
  name = "Amitriptyline"
)
amitriptyline <- getConceptSetDetails(amitriptyline, connection, cdmDatabaseSchema)
venlafaxine <- cs(
  descendants(743670),
  name = "Venlafaxine"
)
venlafaxine <- getConceptSetDetails(venlafaxine, connection, cdmDatabaseSchema)

# Cohorts
nortriptylineNewUsers <- cohort(
  entry = entry(
    drug(nortriptyline, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior depression" = withAll(
      atLeast(1, condition(depression), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(nortriptyline, persistenceWindow = 30, surveillanceWindow = 0))
)
fluoxetineNewUsers <- cohort(
  entry = entry(
    drug(fluoxetine, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior depression" = withAll(
      atLeast(1, condition(depression), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(fluoxetine, persistenceWindow = 30, surveillanceWindow = 0))
)
amitriptylineNewUsers <- cohort(
  entry = entry(
    drug(amitriptyline, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior depression" = withAll(
      atLeast(1, condition(depression), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(amitriptyline, persistenceWindow = 30, surveillanceWindow = 0))
)
venlafaxineNewUsers <- cohort(
  entry = entry(
    drug(venlafaxine, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition(
    "prior depression" = withAll(
      atLeast(1, condition(depression), duringInterval(eventStarts(-Inf, 0)))
    )
  ),
  exit = exit(endStrategy = drugExit(venlafaxine, persistenceWindow = 30, surveillanceWindow = 0))
)

# Write to file ----------------------------------------------------------
cohortDefinitionSet <- tibble(
  cohortId = c(
    1, 
    2,
    3,
    4, 
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12
  ),
  cohortName = c(
    "Hydrochlorothiazide",
    "Losartan",
    "Quinapril",
    "Propranolol",
    "Sitagplitin",
    "Dapagliflozin",
    "Glimepiride",
    "Saxagliptin",
    "Nortriptyline",
    "Fluoxetine",
    "Amitriptyline",
    "Venlafaxine"
  ),
  json = c(
    as.json(hydrochlorothiazideNewUsers),
    as.json(losartanNewUsers),
    as.json(quinaprilNewUsers),
    as.json(propranololNewUsers),
    as.json(sitagliptinNewUsers),
    as.json(dapagliflozinNewUsers),
    as.json(glimepirideNewUsers),
    as.json(saxagliptinNewUsers),
    as.json(nortriptylineNewUsers),
    as.json(fluoxetineNewUsers),
    as.json(amitriptylineNewUsers),
    as.json(venlafaxineNewUsers)
  ),
  sql = c(
    buildCohortQuery(as.json(hydrochlorothiazideNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(losartanNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(quinaprilNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(propranololNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(sitagliptinNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(dapagliflozinNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(glimepirideNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(saxagliptinNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(nortriptylineNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(fluoxetineNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(amitriptylineNewUsers), createGenerateOptions(generateStats = FALSE)),
    buildCohortQuery(as.json(venlafaxineNewUsers), createGenerateOptions(generateStats = FALSE))
  )
)
disconnect(connection)
saveRDS(cohortDefinitionSet, "inst/CohortDefinitionSet.rds")

# Review --------------------------------------------------------------
writeLines(cohortPrintFriendly(as.json(hydrochlorothiazideNewUsers)))
writeLines(cohortPrintFriendly(as.json(losartanNewUsers)))
writeLines(cohortPrintFriendly(as.json(quinaprilNewUsers)))
writeLines(cohortPrintFriendly(as.json(propranololNewUsers)))
writeLines(cohortPrintFriendly(as.json(sitagliptinNewUsers)))
writeLines(cohortPrintFriendly(as.json(dapagliflozinNewUsers)))
writeLines(cohortPrintFriendly(as.json(glimepirideNewUsers)))
writeLines(cohortPrintFriendly(as.json(saxagliptinNewUsers)))
writeLines(cohortPrintFriendly(as.json(nortriptylineNewUsers)))
writeLines(cohortPrintFriendly(as.json(fluoxetineNewUsers)))
writeLines(cohortPrintFriendly(as.json(amitriptylineNewUsers)))
writeLines(cohortPrintFriendly(as.json(venlafaxineNewUsers)))
hydrochlorothiazide
losartan
quinapril
propranolol
hypertensiveDisorder
sitagliptin
dapagliflozin
glimepiride
saxagliptin
t2dm
nortriptyline
fluoxetine
amitriptyline
venlafaxine
depression


# ncs <- readRDS(system.file("ohdsiDevelopmentNegativeControls.rds", package = "MethodEvaluation"))
# readr::write_csv(ncs, "inst/NegativeControls.csv")
