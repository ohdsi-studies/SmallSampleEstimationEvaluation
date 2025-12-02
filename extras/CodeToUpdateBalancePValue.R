# Code to update balance p-values to the new implementation that also takes into
# account the lower tail. This should increase p-values especially when variance
# is high.


# Update CohortMethod ----------------------------------------------------------
remove.packages("CohortMethod")
renv::purge("CohortMethod", version = "5.5.0", prompt = FALSE)
renv::restore(packages = "CohortMethod", prompt = FALSE)


# Update all balance files -------------------------------------------------------
outputFolder <- "e:/SmallSampleEstimationEvaluation_ccae_new_settings"

balanceFiles <- list.files(outputFolder, "Balance.*.rds", recursive = TRUE, full.names = TRUE)

meanDeltas <- rep(0, length(balanceFiles))
pb <- txtProgressBar(style = 3)
for (i in seq_along(balanceFiles)) {
  balanceFile <- balanceFiles[i]
  balance <- readRDS(balanceFile)
  oldP <- balance$afterMatchingSdmP
  balance$afterMatchingSdmP <- CohortMethod:::computeBalanceP(
    sdm = balance$afterMatchingStdDiff,
    sdmVariance = balance$afterMatchingSdmVariance,
    threshold = 0.1
  )
  saveRDS(balance, balanceFile)
  meanDeltas[i] <- mean(balance$afterMatchingSdmP - oldP, na.rm = TRUE)
  if (i %% 100 == 0) {
    setTxtProgressBar(pb, i / length(balanceFiles))  
  }
}
close(pb)
quantile(meanDeltas, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
# 0%         5%        25%        50%        75%        95%       100% 
# 0.00000000 0.00000000 0.00000000 0.00000000 0.05380624 0.11979042 0.18170097 

# Update summary statistics ----------------------------------------------------
largeSampleSize <- 20000
sampleSizes <- c(4000, 2000, 1000, 500, 250)
for (sampleSize in sampleSizes) {
  message("Recomputing summary statistics at sample size ", sampleSize)
  numberOfSamples <- largeSampleSize / sampleSize
  smallSamplesFolder <- file.path(outputFolder, sprintf("smallSample%d", sampleSize))
  smallSampleSubFolders <- file.path(smallSamplesFolder, sprintf("Sample_%d", seq_len(numberOfSamples)))
  outputFile <- file.path(outputFolder, sprintf("PsMetrics_sample_%d.csv", sampleSize))
  unlink(outputFile)
  computePsMetrics(
    sampleFolders = smallSampleSubFolders,
    outputFileName = outputFile
  )
}
