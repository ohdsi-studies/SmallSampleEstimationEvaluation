# Code to extract balance data to share
library(dplyr)

databaseIds <- c("MDCR", "MDCD", "Optum EHR")
outputFolders <- paste0("d:/SmallSampleEstimationEvaluation_",
                        tolower(gsub(" ", "_", databaseIds)))
sampleSizes <- c(4000, 2000, 1000, 500, 250)
targetFolder <- "d:/SmallSampleBalance"
covariates <- tibble(covariateId = 1) %>%
  filter(covariateId != 1)
covariateColumns <- c("covariateName","analysisId", "conceptId", "domainId", "isBinary")

for (i in seq_along(databaseIds)) {
  message("Extracting for database ", databaseIds[i])
  for (sampleSize in sampleSizes) {
    message("- Sample size ", sampleSize)
    samples <- 20000 / sampleSize
    for (sample in seq_len(samples)) {
      sourceSampleFolder <- file.path(outputFolders[i],
                                      sprintf("smallSample%d", sampleSize),
                                      sprintf("Sample_%d", sample))
      targetSampleFolder <- file.path(targetFolder, 
                                      gsub(" ", "_", databaseIds[i]),
                                      sprintf("SampleSize_%d", sampleSize),
                                      sprintf("Sample_%d", sample))
      if (!dir.exists(targetSampleFolder)) {
        dir.create(targetSampleFolder, recursive = TRUE)
      }
      fileRef <- CohortMethod::getFileReference(sourceSampleFolder)
      fileRef <- fileRef[!duplicated(fileRef$sharedBalanceFile), ]
      fileRef <- fileRef[fileRef$sharedBalanceFile != "", ]
      for (j in seq_len(nrow(fileRef))) {
        sourceBalanceFile <- file.path(sourceSampleFolder, fileRef$sharedBalanceFile[j])
        targetBalanceFile <- file.path(targetSampleFolder, sprintf("Balance_t%d_c%d_a%d.rds",
                                                                   fileRef$targetId[j],
                                                                   fileRef$comparatorId[j],
                                                                   fileRef$analysisId[j]))
        balance <- readRDS(sourceBalanceFile)
        newCovariateIdx <- which(!balance$covariateId %in% covariates$covariateId)
        if (length(newCovariateIdx) > 0) {
          newCovariates <-balance[newCovariateIdx, c("covariateId", covariateColumns)]
          covariates <- bind_rows(covariates, newCovariates)
        }
        balance[, covariateColumns] <- NULL
        saveRDS(balance, targetBalanceFile)
      }
    }
  }
}
readr::write_csv(covariates, file.path(targetFolder, "covariateRef.csv"))
csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
negativeControls <- readr::read_csv(csvFileName, show_col_types = FALSE) 
targets <- negativeControls %>%
  distinct(targetId, targetName)
readr::write_csv(targets,  file.path(targetFolder, "targetRef.csv"))
comparators <- negativeControls %>%
  distinct(comparatorId, comparatorName)
readr::write_csv(comparators,  file.path(targetFolder, "comparatorRef.csv"))
analyses <- tibble(analysisId = c(1, 2, 3, 4, 5),
                   adjustment = c("Matching", "Crude", "Stratification", "Matching", "Stratification"),
                   propensityModel = c("Local", NA, "Local", "Global", "Global"))
readr::write_csv(analyses,  file.path(targetFolder, "analysisRef.csv"))
