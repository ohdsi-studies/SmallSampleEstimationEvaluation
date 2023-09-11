# Compare the propensity scores from the global and the local propensity model
library(dplyr)
library(ggplot2)

options(andromedaTempFolder = "d:/andromedaTemp")

rootFolderName <- "d:/SmallSampleEstimationEvaluation_"
databases <- c("MDCD", "MDCR", "Optum_EHR")
sampleSizes <- c(4000, 2000, 1000, 500, 250)
# sampleSizes <- c(4000, 1000,  250)

tcs <- read.csv(file.path(paste0(rootFolderName, databases[1]), "allControls.csv")) %>%
  distinct(targetId, targetName, comparatorId, comparatorName) %>%
  mutate(comparison = sprintf("%s vs %s", targetName, comparatorName))

computeAuc <- function(ps, cohortMethodData) {
  population <- CohortMethod::matchOnPs(ps, maxRatio = 1) %>%
    select("rowId", "treatment")
  if (sum(population$treatment) < 10) {
    return(tibble(auc = NA, aucLb95ci = NA, aucUb95ci = NA))
  } 
  filteredCovariateData <- Andromeda::andromeda(
    covariates = cohortMethodData$covariates %>%
      filter(.data$rowId %in% local(population$rowId)),
    covariateRef = cohortMethodData$covariateRef,
    analysisRef = cohortMethodData$analysisRef
  )
  metaData <- attr(cohortMethodData, "metaData")
  metaData$populationSize <- nrow(population)
  attr(filteredCovariateData, "metaData") <- metaData
  class(filteredCovariateData) <- "CovariateData"
  capture.output(covariateData <- FeatureExtraction::tidyCovariateData(filteredCovariateData))
  population$y <- population$treatment
  covariateData$outcomes <- population
  cyclopsData <- Cyclops::convertToCyclopsData(covariateData$outcomes, covariateData$covariates, modelType = "lr")
  # Note: Cyclops will throw an error if the optimal betas are identical to the starting betas, so 
  # setting intercept to start at 1:
  nCovariates <- Cyclops::getNumberOfCovariates(cyclopsData)
  fit <- Cyclops::fitCyclopsModel(
    cyclopsData = cyclopsData, 
    prior = Cyclops::createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
    control = Cyclops::createControl(
      noiseLevel = "silent",
      cvType = "auto",
      seed = 1,
      resetCoefficients = TRUE,
      fold = 10,
      cvRepetitions = 10,
      startingVariance = 0.01,
      minCVData = 10,
      threads = 25
    ),
    startingCoefficients = c(1, rep(0, nCovariates - 1))
  )
  population$propensityScore <- predict(fit)
  auc <- CohortMethod::computePsAuc(population, confidenceIntervals = TRUE)
  return(auc)
}
# database <- "MDCD"
for (database in databases) {
  message("Database: ", database)
  
  aucs <- list()
  
  dbFolder <- paste0(rootFolderName, database)
  cmFolder <- file.path(dbFolder, "largeSample")
  ref <- CohortMethod::getFileReference(cmFolder) %>%
    filter(analysisId == 4) %>%
    distinct(targetId, comparatorId, cohortMethodDataFile, sharedPsFile) %>%
    inner_join(tcs, by = join_by(targetId, comparatorId))
  # i <- 1
  for (i in seq_len(nrow(ref))) {
    refRow <- ref[i, ]
    message(sprintf("Exposure: %d, comparator: %d", refRow$targetId, refRow$comparatorId))
    # sampleSize = 2000
    for (sampleSize in sampleSizes) {
      message(sprintf("- Sample size: %d", sampleSize))
      
      sampleRootFolder <- file.path(dbFolder, sprintf("smallSample%d", sampleSize))
      # sampleFolder = list.dirs(sampleRootFolder, full.names = TRUE, recursive = FALSE)[1]
      for (sampleFolder in list.dirs(sampleRootFolder, full.names = TRUE, recursive = FALSE)) {
        localRef <- CohortMethod::getFileReference(sampleFolder) %>%
          filter(targetId == refRow$targetId & comparatorId == refRow$comparatorId) %>%
          distinct(analysisId, cohortMethodDataFile, sharedPsFile) 
        cohortMethodDataFile <- localRef %>%
          filter(analysisId == 1) %>%
          pull(cohortMethodDataFile)
        cohortMethodData <- CohortMethod::loadCohortMethodData(file.path(sampleFolder, cohortMethodDataFile))
        
        # Local PS
        sharedPsFile <- localRef %>%
          filter(analysisId == 1) %>%
          pull(sharedPsFile)
        ps <- readRDS(file.path(sampleFolder, sharedPsFile)) 
        aucLocal <- computeAuc(ps, cohortMethodData)
        
        # Global PS
        sharedPsFile <- localRef %>%
          filter(analysisId == 4) %>%
          pull(sharedPsFile)
        ps <- readRDS(file.path(sampleFolder, sharedPsFile)) 
        ps <- ps %>%
          filter(rowId %in% pull(cohortMethodData$cohorts, rowId))
        aucGlobal <- computeAuc(ps, cohortMethodData)
        
        row <- refRow %>%
          select("targetId", "comparatorId", "comparison") %>%
          cross_join(bind_rows(aucLocal, aucGlobal)) %>%
          mutate(analysisId = c(1, 3),
                 database = !!database,
                 sampleSize = !!sampleSize)
        
        aucs[[length(aucs) + 1]] <- row
      }
    }
  }
  aucs <- bind_rows(aucs)
  saveRDS(aucs, file.path(paste0(rootFolderName, databases[1]), "plotsAndTables", sprintf("PostAdjAucs_%s.rds", database)))
  # aucs$sampleSize <- as.factor(aucs$sampleSize)
  # ggplot(aucs, aes(x = sampleSize, y = auc, group = sampleSize)) +
  #   # geom_boxplot() +
  #   geom_violin() +
  #   facet_grid(~comparison)
}

