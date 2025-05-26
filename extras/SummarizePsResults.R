library(ggplot2)
library(dplyr)

outputFolder <- "d:/SmallSampleEstimationEvaluation_mdcd"
databaseId <- "MDCD"

outputFolder <- "d:/SmallSampleEstimationEvaluation_mdcr"
databaseId <- "MDCR"

outputFolder <- "d:/SmallSampleEstimationEvaluation_optum_ehr"
databaseId <- "Optum EHR"

plotsAndTablesFolder <- file.path(outputFolder, "plotsAndTables")

# dir.create(plotsAndTablesFolder)

sampleSizes <- c(4000, 2000, 1000, 500, 250, 125)

tcs <- read.csv(file.path(outputFolder, "allControls.csv")) |>
  distinct(targetId, targetName, comparatorId, comparatorName) |>
  mutate(comparison = sprintf("%s vs %s", targetName, comparatorName))
metrics <- tibble()
psMetrics <- tibble()
for (sampleSize in sampleSizes) {
  metrics <- metrics |>
    bind_rows(
      readr::read_csv(
        file = file.path(outputFolder, sprintf("Metrics_sample_%d.csv", sampleSize)),
        show_col_types = FALSE
      ) |>
        mutate(sampleSize = !!sampleSize)
    )
  psMetrics <- psMetrics |> 
    bind_rows(
      readr::read_csv(
        file = file.path(outputFolder, sprintf("PsMetrics_sample_%d.csv", sampleSize)),
        show_col_types = FALSE
      ) |>
        mutate(sampleSize = !!sampleSize)
    )
}
metricsLargeSample <- readr::read_csv(
  file = file.path(outputFolder, "Metrics_LargeSample.csv"),
  show_col_types = FALSE
) |>
  mutate(sampleSize = 20000)
x <- tibble(
  sampleSize = c(20000, sampleSizes),
  x = seq_len(length(sampleSizes) + 1)
)
largeSampleFolder <- file.path(outputFolder, "largeSample")
ref <- CohortMethod::getFileReference(largeSampleFolder) |>
  distinct(analysisId, targetId, comparatorId, sharedBalanceFile) |>
  filter(sharedBalanceFile != "")
getMaxSdm <- function(row) {
  balance <- readRDS(file.path(largeSampleFolder, row$sharedBalanceFile))
  maxSdm <- max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)
  row <- tibble(analysisId = row$analysisId,
         targetId = row$targetId,
         comparatorId = row$comparatorId,
         maxSdm = maxSdm) 
  return(row)
}
balLargeSample <- lapply(split(ref, seq_len(nrow(ref))), getMaxSdm) |>
  bind_rows() |>
  mutate(sampleSize = 20000)
easeP <- readr::read_csv(file.path(outputFolder, "easeP.csv"), show_col_types = FALSE)

# Combine tables into one overall metrics table and save
combined <- metrics |>
  filter(analysisId %in% c(1, 2, 3, 4, 5)) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", ifelse(analysisId %in% c(3, 5), "PS stratification", "No PS adjustment"))) |>
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", ifelse(analysisId %in% c(4, 5), "Global", "None"))) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  mutate(x = ifelse(psModel == "Local", x + 0.1, x - 0.1)) |>
  left_join(easeP, by = join_by(analysisId, targetId, comparatorId, sampleSize)) |>
  mutate(significant = ifelse(!is.na(p) & p < 0.05, "Significant", "Non-sign."),
         database = databaseId)
saveRDS(combined, file.path(outputFolder, "CombinedMetrics.rds"))

combinedPs <- psMetrics |>
  bind_rows(
    balLargeSample |>
      filter(analysisId %in% c(1, 3, 4, 5)) |>
      mutate(maxSdmMedian = maxSdm)
  ) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", ifelse(analysisId %in% c(3, 5), "PS stratification", "No PS adjustment"))) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  mutate(database = databaseId)
saveRDS(combinedPs, file.path(outputFolder, "CombinedPSMetrics.rds"))

# Plot EASE using local or global propensity model or crude, data pooling ----------------------------------------
vizData <- metrics |>
  filter(analysisId %in% c(1, 2, 3, 4, 5), calibrated == FALSE) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", ifelse(analysisId %in% c(3, 5), "PS stratification", "No PS adjustment"))) |>
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", ifelse(analysisId %in% c(4, 5), "Global", "None"))) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  mutate(x = ifelse(psModel == "Local", x + 0.1, x - 0.1)) |>
  left_join(easeP, by = join_by(analysisId, targetId, comparatorId, sampleSize)) |>
  mutate(significant = ifelse(!is.na(p) & p < 0.05, "Significant", "Non-sign.")) |>
  select(comparison, x, ease, easeCi95Lb, easeCi95Ub, significant, psModel, psMethod)
ggplot(vizData, aes(x = x, y = ease, ymin = easeCi95Lb, ymax = easeCi95Ub, group = psModel, color = psModel)) +
  geom_errorbar() +
  geom_point(aes(shape = significant, size = significant), fill = rgb(1, 1, 1)) +
  scale_shape_manual(values = c(19, 24)) +
  scale_size_manual(values = c(2,3)) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("EASE (95% CI)") +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(color = "PS model") +
  facet_grid(comparison~psMethod)
ggsave(file.path(plotsAndTablesFolder, "EASE_pooled_crude.png"), width = 8, height = 8.5, dpi = 300)

# Plot precision using local or global propensity model, data pooling -----------------------------------------------------
vizData <- metrics |>
  filter(analysisId %in% c(1, 3, 4, 5), calibrated == FALSE) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification")) |>
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  select(comparison, x, meanP, psModel, psMethod, calibrated)
ggplot(vizData, aes(x = x, y = meanP, group = psModel, color = psModel)) +
  geom_point() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Mean precision", limits = c(0, max(vizData$meanP))) +
  labs(color = "PS model") +
  facet_grid(comparison~psMethod)
ggsave(file.path(plotsAndTablesFolder, "Precision_pooled.png"), width = 6, height = 8.5, dpi = 300)

# Plot coverage using local or global propensity model, data pooling -----------------------------------------------------
vizData <- metrics |>
  filter(analysisId %in% c(1, 3, 4, 5), calibrated == FALSE) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification")) |>
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  select(comparison, x, coverage, psModel, psMethod, calibrated)
ggplot(vizData, aes(x = x, y = coverage, group = psModel, color = psModel)) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_point() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Coverage", limits = c(0.5, 1)) +
  labs(color = "PS model") +
  facet_grid(comparison~psMethod)
ggsave(file.path(plotsAndTablesFolder, "Coverage_pooled.png"), width = 6, height = 8.5, dpi = 300)

# Plot precision after calibration using local or global propensity model, data pooling -----------------------------------------------------
vizData <- metrics |>
  filter(analysisId %in% c(1, 3, 4, 5), calibrated == TRUE) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification")) |>
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  select(comparison, x, meanP, psModel, psMethod, calibrated)
ggplot(vizData, aes(x = x, y = meanP, group = psModel, color = psModel)) +
  geom_point() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Mean precision after calibration", limits = c(0, max(vizData$meanP))) +
  labs(color = "PS model") +
  facet_grid(comparison~psMethod)
ggsave(file.path(plotsAndTablesFolder, "Precision_after_calibration_pooled.png"), width = 6, height = 8.5, dpi = 300)

# Plot coverage after calibration using local or global propensity model, data pooling -----------------------------------------------------
vizData <- metrics |>
  filter(analysisId %in% c(1, 3, 4, 5), calibrated == TRUE) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification")) |>
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  select(comparison, x, coverage, psModel, psMethod, calibrated)
ggplot(vizData, aes(x = x, y = coverage, group = psModel, color = psModel)) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_point() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Coverage after calibration", limits = c(0.5, 1)) +
  labs(color = "PS model") +
  facet_grid(comparison~psMethod)
ggsave(file.path(plotsAndTablesFolder, "Coverage_after_calibration_pooled.png"), width = 6, height = 8.5, dpi = 300)

# Plot balance -------------------------------------------------------------------------------------------------------
vizData <- psMetrics |>
  bind_rows(
    balLargeSample |>
      filter(analysisId %in% c(1, 3, 4, 5)) |>
      mutate(maxSdmMedian = maxSdm)
  ) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification"),
         psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  select(comparison, x, sampleSize, psMethod, maxSdmMin,  maxSdmP25,  maxSdmMedian,  maxSdmP75,  maxSdmMax)
ggplot(vizData, aes(x = x, y = maxSdmMedian, group = sampleSize)) +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  geom_boxplot(
    aes(ymin = maxSdmMin, lower = maxSdmP25, middle = maxSdmMedian, upper = maxSdmP75, ymax = maxSdmMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Max standardized difference of means (SDM)") +
  coord_cartesian(ylim = c(0, 1)) +
  facet_grid(comparison~psMethod)
ggsave(file.path(plotsAndTablesFolder, "Balance_pooled.png"), width = 6, height = 8.5, dpi = 300)

# Plot non-zero coefs -----------------------------------------------------------------------------------------------------
vizData <- psMetrics |>
  filter(analysisId == 1) |>
  inner_join(x, by = join_by(sampleSize))  |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) 
ggplot(vizData, aes(x = x, y = nonZeroCoefCountMedian, group = sampleSize)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(
    aes(ymin = nonZeroCoefCountMin, lower = nonZeroCoefCountP25, middle = nonZeroCoefCountMedian, upper = nonZeroCoefCountP75, ymax = nonZeroCoefCountMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Numer of non-zero coefficients in propensity model") +
  facet_grid(comparison ~ .)
ggsave(file.path(plotsAndTablesFolder, "NonZeroCoefes.png"), width = 6, height = 8.5, dpi = 300)

# Plot covariate count -----------------------------------------------------------------------------------------------------
vizData <- psMetrics |>
  filter(analysisId == 1) |>
  inner_join(x, by = join_by(sampleSize)) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId"))
ggplot(vizData, aes(x = x, y = covCountMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = covCountMin, lower = covCountP25, middle = covCountMedian, upper = covCountP75, ymax = covCountMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Numer of covariates") +
  facet_grid(comparison ~ .)
ggsave(file.path(plotsAndTablesFolder, "Covariates.png"), width = 6, height = 8.5, dpi = 300)

# Plot PS AUC count -----------------------------------------------------------------------------------------------------
vizData <- psMetrics |>
  filter(analysisId == 1) |>
  inner_join(x, by = join_by(sampleSize)) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) 
ggplot(vizData, aes(x = x, y = aucMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = aucMin, lower = aucP25, middle = aucMedian, upper = aucP75, ymax = aucMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Propensity model AUC") +
  facet_grid(comparison ~ .)
ggsave(file.path(plotsAndTablesFolder, "AuC.png"), width = 6, height = 8.5, dpi = 300)

# Plot EASE using pooling or meta-analysis -----------------------------------------------------
vizData <- metrics |>
  bind_rows(metricsLargeSample) |>
  filter(analysisId %in% c(1, 3, 101, 103, 201, 203), calibrated == FALSE) |>
  mutate(psMethod = ifelse(analysisId %in% c(1, 101, 201), "PS 1-on-1 matching", "PS stratification")) |>
  mutate(combiMethod = ifelse(analysisId %in% c(1, 3), "Pooling", ifelse(analysisId %in% c(101, 103), "Non-normal", "Normal"))) |>
  mutate(combiMethod = ifelse(sampleSize == 20000, "Single site", combiMethod)) |>
  inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
  inner_join(x, by = join_by(sampleSize)) |>
  mutate(x = ifelse(combiMethod == "Normal", x + 0.2, ifelse(combiMethod == "Non-normal", x - 0.2, x))) |>
  select(comparison, x, ease, easeCi95Lb, easeCi95Ub, combiMethod, psMethod)
ggplot(vizData, aes(x = x, y = ease, ymin = easeCi95Lb, ymax = easeCi95Ub, group = combiMethod, color = combiMethod)) +
  geom_point() +
  geom_errorbar() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("EASE (95% CI)") +
  labs(color = "Synthesis") +
  facet_grid(comparison~psMethod)
ggsave(file.path(plotsAndTablesFolder, "EASE_synthesis_methods.png"), width = 8, height = 8.5, dpi = 300)

# Get exposure counts -----------------------------------------------------------------------------
folder <- file.path(outputFolder, "fullData")
ref <- CohortMethod::getFileReference(folder)
csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
tcs <- readr::read_csv(csvFileName, show_col_types = FALSE)  |>
  distinct(targetId, targetName, comparatorId, comparatorName)

# row <- tcs[1, ]
getExposureCounts <- function(row) {
  outcomeModelFile <- ref |>
    inner_join(row, by = join_by(targetId, comparatorId)) |>
    head(1) |>
    pull(outcomeModelFile)
  model <- readRDS(file.path(folder, outcomeModelFile))
  attritionTable <- CohortMethod::getAttritionTable(model)
  row <- bind_cols(row, CohortMethod::getAttritionTable(model)[2, c("targetPersons", "comparatorPersons")])
  return(row)
}
exposureCounts <- lapply(split(tcs, seq_len(nrow(tcs))), getExposureCounts)
exposureCounts <- bind_rows(exposureCounts)
exposureCounts <- exposureCounts |>
  select(targetName, comparatorName, targetPersons, comparatorPersons)
colnames(exposureCounts) <- c("Target", "Comparator", "Target persons", "Comparator persons")
readr::write_csv(exposureCounts, file.path(outputFolder, "ExposureCounts.csv"))

