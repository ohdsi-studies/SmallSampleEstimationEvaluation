folder <- file.path(outputFolder, "smallSample1000", "Sample_2")
ref <- CohortMethod::getFileReference(folder)
ps <- readRDS(file.path(folder, ref$sharedPsFile[1]))
CohortMethod::plotPs(ps, showAucLabel = TRUE, showCountsLabel = TRUE)
str(ps)
metaData <- attr(ps, "metaData")
model <- metaData$psModelCoef[metaData$psModelCoef != 0]
model[order(-model)]
length(model)

balance <- readRDS(file.path(folder, ref$sharedBalanceFile[ref$analysisId == 1][1]))
max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)

balance <- readRDS(file.path(folder, ref$sharedBalanceFile[ref$analysisId == 3][1]))
max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)


