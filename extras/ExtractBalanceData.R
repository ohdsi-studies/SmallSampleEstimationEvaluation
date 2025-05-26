# Code to extract balance data to share
library(dplyr)

databaseIds <- c("MDCR", "MDCD", "Optum EHR")
outputFolders <- paste0("d:/SmallSampleEstimationEvaluation_",
                        tolower(gsub(" ", "_", databaseIds)))
sampleSizes <- c(4000, 2000, 1000, 500, 250)
targetFolder <- "d:/SmallSampleBalance"
covariates <- tibble(covariateId = 1) |>
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
      
      # Also store effect size estimates:
      estimates <- CohortMethod::getResultsSummary(sourceSampleFolder) |>
        select("targetId", "comparatorId", "outcomeId", "analysisId", "logRr", "seLogRr", "p")
      targetEstimatesFile <- file.path(targetSampleFolder, sprintf("Estimates.rds"))
      saveRDS(estimates, targetEstimatesFile)
      
      # And likelihood profiles:
      fileRef <- CohortMethod::getFileReference(sourceSampleFolder)
      for (j in seq_len(nrow(fileRef))) {
        sourceOmFile <- file.path(sourceSampleFolder, fileRef$outcomeModelFile[j])
        targetProfileFile <- file.path(targetSampleFolder, sprintf("LlProfile_t%d_c%d_o%d_a%d.rds",
                                                                   fileRef$targetId[j],
                                                                   fileRef$comparatorId[j],
                                                                   fileRef$outcomeId[j],
                                                                   fileRef$analysisId[j]))
        om <- readRDS(sourceOmFile)
        saveRDS(om$logLikelihoodProfile, targetProfileFile)
      }
    }
  }
  positiveControls <- readr::read_csv(file.path(outputFolders[i], "allControls.csv"), show_col_types = FALSE) |>
    filter(targetEffectSize > 1) |>
    select("targetId", "comparatorId", "outcomeId", "outcomeName", "targetEffectSize", "trueEffectSizeFirstExposure")
  readr::write_csv(positiveControls, file.path(targetFolder, 
                                               gsub(" ", "_", databaseIds[i]),
                                               "positiveControlOutcomeRef.csv"))
}
readr::write_csv(covariates, file.path(targetFolder, "covariateRef.csv"))
csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
negativeControls <- readr::read_csv(csvFileName, show_col_types = FALSE) 
targets <- negativeControls |>
  distinct(targetId, targetName)
readr::write_csv(targets,  file.path(targetFolder, "targetRef.csv"))
comparators <- negativeControls |>
  distinct(comparatorId, comparatorName)
readr::write_csv(comparators,  file.path(targetFolder, "comparatorRef.csv"))
outcomes <- negativeControls |>
  distinct(outcomeConceptId, outcomeName) |>
  rename(outcomeId = "outcomeConceptId")
readr::write_csv(outcomes,  file.path(targetFolder, "negativeControlOutcomeRef.csv"))
analyses <- tibble(analysisId = c(1, 2, 3, 4, 5),
                   adjustment = c("Matching", "Crude", "Stratification", "Matching", "Stratification"),
                   propensityModel = c("Local", NA, "Local", "Global", "Global"))
readr::write_csv(analyses,  file.path(targetFolder, "analysisRef.csv"))


# Example code for meta-analysis using likelihood profiles -----------------------
library(dplyr)
computeMetaAnalysis <- function(targetId, 
                                comparatorId,
                                outcomeId,
                                analysisId,
                                sampleSize,
                                sampleIds,
                                databaseFolder,
                                type = "fixed") {
  # Load likelihood profiles:
  fileNames <- file.path(databaseFolder,
                         sprintf("SampleSize_%d", sampleSize),
                         sprintf("Sample_%d", sampleIds),
                         sprintf("LlProfile_t%d_c%d_o%d_a%d.rds",
                                 targetId,
                                 comparatorId,
                                 outcomeId,
                                 analysisId))
  profiles <- lapply(fileNames, readRDS)
  if (type == "fixed") {
    # Faster, and in this case correct because the effect is fixed:
    maEstimate <- EvidenceSynthesis::computeFixedEffectMetaAnalysis(profiles) |>
      transmute(rr = rr,
                ci95Lb = lb,
                ci95Ub = ub)
    
  } else {
    # Slower, but what we would do in the real world. May have less power because
    # it doesn't assume fixed effect:
    maEstimate <- EvidenceSynthesis::computeBayesianMetaAnalysis(profiles) |>
      transmute(rr = exp(mu),
                ci95Lb = exp(mu95Lb),
                ci95Ub = exp(mu95Ub))
  }
  return(maEstimate)
}

# Example of using some but not all sites:
computeMetaAnalysis(
  targetId = 1,
  comparatorId = 2 ,
  outcomeId = 36713918,
  analysisId = 1,
  sampleSize = 1000,
  sampleIds = c(1:10, 12, 14, 16:20),
  databaseFolder = "d:/SmallSampleBalance/MDCR",
  type = "fixed"
)

