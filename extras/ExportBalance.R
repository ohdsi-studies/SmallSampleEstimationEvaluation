library(dplyr)

outputFolder <- "d:/SmallSampleEstimationEvaluation"

exportFolder <- file.path(outputFolder, "balanceExport")

# dir.create(exportFolder)

sampleSizes <- c(4000, 2000, 1000, 500, 250)

sampleSize <- 500

i <- 1
cmFolder <- file.path(outputFolder, sprintf("smallSample%d", sampleSize), sprintf("Sample_%d", i))
ref <- CohortMethod::getFileReference(cmFolder)
refRow <- ref |>
  filter(
    targetId == 1,
    comparatorId == 2,
    analysisId == 1
  ) |>
  head(1) 
balance <- readRDS(file.path(cmFolder, refRow$sharedBalanceFile))
model <- readRDS(file.path(cmFolder, refRow$outcomeModelFile))
model$populationCounts
CohortMethod::getAttritionTable(model)
fileName <- file.path(exportFolder, sprintf(
  "Balance_t%d_c%d_a%d_s%d_i%d.csv",
  refRow$targetId,
  refRow$comparatorId,
  refRow$analysisId,
  sampleSize,
  i
))
readr::write_csv(balance, fileName)


0.05 * sum(balance$afterMatchingSumTarget != 0 & balance$afterMatchingSumComparator != 0, na.rm = TRUE)
