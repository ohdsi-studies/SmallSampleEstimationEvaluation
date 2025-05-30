# Create exposure and negative control outcome cohorts

source("extras/biasSearch/SetConnectionDetails.R")
library(Capr)
library(readr)
library(dplyr)
library(CirceR)

exposures <- read_csv("extras/biasSearch/Exposures.csv", show_col_types = FALSE)
negativeControls <- read_csv("extras/biasSearch/NegativeControls.csv", show_col_types = FALSE)

connection <- DatabaseConnector::connect(connectionDetails)

# Create exposure cohort definitions--------------------------------------------

indicationConceptSets <- list()

hypertensiveDisorder <- cs(
  descendants(316866),
  name = "Hypertensive disorder"
)
hypertensiveDisorder <- getConceptSetDetails(hypertensiveDisorder, connection, cdmDatabaseSchema)
indicationConceptSets[["Hypertension"]] <- hypertensiveDisorder

t2dm <- cs(
  descendants(443238, 201820, 442793), 
  descendants(exclude(195771, 201254, 435216, 761051, 4058243, 40484648)),
  name = "Type 2 diabetes mellitus (diabetes mellitus excluding T1DM and secondary)"
)
t2dm <- getConceptSetDetails(t2dm, connection, cdmDatabaseSchema)
indicationConceptSets[["Type2Diabetes"]] <- t2dm

mdd <- cs(
  descendants(4191716, 4212469, 4175329, 440383, 40546087), 
  descendants(exclude(377527, 379784, 433440, 435520, 436665, 438727, 442306, 443864, 4224940, 4239471, 36684319, 40481798)),
  name = "Major depressive disorder"
)
mdd <- getConceptSetDetails(mdd, connection, cdmDatabaseSchema)
indicationConceptSets[["Depression"]] <- mdd

cohortDefinitionSet  <- list()
for (i in seq_len(nrow(exposures))) {
  drugConceptSet <- cs(
    descendants(exposures$conceptId[i]),
    name = exposures$name[i]
  )
  drugConceptSet <- getConceptSetDetails(drugConceptSet, connection, cdmDatabaseSchema)
  
  indicationConceptSet <- indicationConceptSets[[exposures$indicationId[i]]]
  
  newUserCohort <- cohort(
    entry = entry(
      drugExposure(drugConceptSet, firstOccurrence()),
      observationWindow = continuousObservation(priorDays = 365)
    ),
    attrition = attrition(
      "Has indication" = withAll(
        atLeast(1, conditionOccurrence(indicationConceptSet), duringInterval(eventStarts(-365, 0)))
      )
    ),
    exit = exit(endStrategy = drugExit(drugConceptSet, persistenceWindow = 30, surveillanceWindow = 0))
  )
  json <- as.json(newUserCohort)
  cohortDefinitionSet [[i]] <- tibble(
    cohortId = exposures$conceptId[i],
    cohortName = exposures$name[i],
    json = json,
    sql = buildCohortQuery(json, createGenerateOptions(generateStats = FALSE))
  )
}
cohortDefinitionSet <- bind_rows(cohortDefinitionSet)

# Generate cohorts -------------------------------------------------------------
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)
CohortGenerator::createCohortTables(
  connection = connection, 
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTableNames
)

CohortGenerator::generateCohortSet(
  connection = connection, 
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTableNames,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlOutcomeCohortSet <- negativeControls |>
  mutate(cohortId = conceptId) |>
  select("cohortId", cohortName = "outcomeName", outcomeConceptId = "conceptId") |>
  filter(!duplicated(cohortId))
CohortGenerator::generateNegativeControlOutcomeCohorts(
  connection = connection, 
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cdmDatabaseSchema = cdmDatabaseSchema,
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Count cohorts ----------------------------------------------------------------
cohortCounts <- CohortGenerator::getCohortCounts(
  connection = connection, 
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable
)
cohortCounts <- cohortCounts |>
  inner_join(
    bind_rows(
      exposures |>
        select(cohortId = "conceptId", cohortName = "name") |>
        mutate(type = "exposure"),
      negativeControlOutcomeCohortSet |>
        select("cohortId", "cohortName") |>
        mutate(type = "outcome")
    ),
    by = join_by("cohortId")
  )
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}
write_csv(cohortCounts, file.path(outputFolder, "cohortCounts.csv"))

DatabaseConnector::disconnect(connection)

