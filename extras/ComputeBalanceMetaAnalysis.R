# Code to extract balance data to share
library(dplyr)
library(meta)
library(ParallelLogger)

baseFolder <- "E:/SmallSampleEstimationEvaluation_ccae"
outputFolder <- file.path(baseFolder, "BalanceMetaAnalysis")

dir.create(outputFolder, showWarnings = FALSE)

sampleSizes <- c(4000, 2000, 1000, 500, 250)

# group = groups[[1]]
doMetaAnalysis <- function(group) {
  group <- group |>
    filter(!is.na(afterMatchingSdmVariance))
  if (nrow(group) == 0) {
    return(NULL)
  }
  metaBefore <- meta::metagen(group$beforeMatchingStdDiff,
                              sqrt(group$beforeMatchingSdmVariance),
                              control=list(iter.max=1000))
  metaAfter <- meta::metagen(group$afterMatchingStdDiff,
                              sqrt(group$afterMatchingSdmVariance),
                              control=list(iter.max=1000))
  row <- tibble(
    covariateId = group$covariateId[1],
    covariateName = group$covariateName[1],
    beforeMatchingStdDiff = metaBefore$TE.random,
    beforeMatchingSdmVariance = metaBefore$seTE.random,
    afterMatchingStdDiff = metaAfter$TE.random,
    afterMatchingSdmVariance = metaAfter$seTE.random
  )
  return(row)
}

computeBalanceP <- function(sdm, sdmVariance, threshold) {
  zUpper <- (abs(sdm) - threshold) / sqrt(sdmVariance)
  pUpper <- pnorm(zUpper, lower.tail = FALSE)
  zLower <- (-abs(sdm) - threshold) / sqrt(sdmVariance)
  pLower <- pnorm(zLower, lower.tail = TRUE)
  p <- pUpper + pLower
  return(p)
}

cluster <- makeCluster(4)
clusterRequire(cluster, "dplyr")
clusterRequire(cluster, "metafor")
for (sampleSize in sampleSizes) {
  message("- Sample size ", sampleSize)
  samples <- 20000 / sampleSize
  
  sampleFolders <- file.path(baseFolder,
                             sprintf("smallSample%d", sampleSize),
                             sprintf("Sample_%d", seq_len(samples)))
  fileRef <- CohortMethod::getFileReference(sampleFolders[1])
  fileRef <- fileRef[!duplicated(fileRef$sharedBalanceFile), ]
  fileRef <- fileRef[fileRef$sharedBalanceFile != "", ]
  
  for (i in seq_len(nrow(fileRef))) {
    fileName <- file.path(outputFolder, sprintf("Balance_s%d_a%d_t%d_c%d.rds",
                                                sampleSize,
                                                fileRef$analysisId[i],
                                                fileRef$targetId[i],
                                                fileRef$comparatorId[i]))
    if (!file.exists(fileName)) {
      message("  Balance file: ",  fileRef$sharedBalanceFile[i])
      balanceAllSamples <- lapply(file.path(sampleFolders, fileRef$sharedBalanceFile[i]), readRDS)
      balanceAllSamples <- bind_rows(balanceAllSamples)
      groups <- balanceAllSamples |>
        group_by(covariateId) |>
        group_split()
      balance <- clusterApply(cluster, groups, doMetaAnalysis)
      balance <- bind_rows(balance)
      balance$afterMatchingSdmP <- computeBalanceP(balance$afterMatchingStdDiff, balance$afterMatchingSdmVariance, 0.1)
      balance$afterMatchingBalanced <-  balance$afterMatchingSdmP > 0.05 / nrow(balance)
      # CohortMethod::plotCovariateBalanceScatterPlot(balance, showUnbalanced = TRUE)
      
      saveRDS(balance, fileName)
    }
  }
}

stopCluster(cluster)

