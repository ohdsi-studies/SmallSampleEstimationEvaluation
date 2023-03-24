library(ggplot2)
library(dplyr)

outputFolder <- "d:/SmallSampleEstimationEvaluation"

plotsAndTablesFolder <- file.path(outputFolder, "plotsAndTables")

# dir.create(plotsAndTablesFolder)

sampleSizes <- c(4000, 2000, 1000, 500, 250, 100)

metrics <- tibble()
psMetrics <- tibble()
for (sampleSize in sampleSizes) {
  metrics <- metrics %>%
    bind_rows(
      readr::read_csv(
        file = file.path(outputFolder, sprintf("Metrics_sample_%d.csv", sampleSize)),
        show_col_types = FALSE
      ) %>%
        mutate(sampleSize = !!sampleSize)
    )
  psMetrics <- psMetrics %>% 
    bind_rows(
      readr::read_csv(
        file = file.path(outputFolder, sprintf("PsMetrics_sample_%d.csv", sampleSize)),
        show_col_types = FALSE
      ) %>%
        mutate(sampleSize = !!sampleSize)
    )
}
metricsLargeSample <- readr::read_csv(
  file = file.path(outputFolder, "Metrics_LargeSample.csv"),
  show_col_types = FALSE
) %>%
  mutate(sampleSize = 20000)
x <- tibble(
  sampleSize = c(20000, sampleSizes),
  x = seq_len(length(sampleSizes) + 1)
)

# Plot EASE using local or global propensity model, data pooling -----------------------------------------------------
vizData <- metrics %>%
  filter(analysisId %in% c(1, 3, 4, 5), calibrated == FALSE) %>%
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification")) %>%
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) %>%
  inner_join(x, by = join_by(sampleSize)) %>%
  mutate(x = ifelse(psModel == "Local", x + 0.1, x - 0.1)) %>%
  select(x, ease, easeCi95Lb, easeCi95Ub, psModel, psMethod)
ggplot(vizData, aes(x = x, y = ease, ymin = easeCi95Lb, ymax = easeCi95Ub, group = psModel, color = psModel)) +
  geom_point() +
  geom_errorbar() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("EASE (95% CI)") +
  labs(color = "PS model") +
  facet_grid(~psMethod)
ggsave(file.path(plotsAndTablesFolder, "EASE_pooled.png"), width = 6, height = 4, dpi = 300)

# Plot precision using local or global propensity model, data pooling -----------------------------------------------------
vizData <- metrics %>%
  filter(analysisId %in% c(1, 3, 4, 5), calibrated == FALSE) %>%
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification")) %>%
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) %>%
  inner_join(x, by = join_by(sampleSize)) %>%
  mutate(x = ifelse(psModel == "Local", x + 0.1, x - 0.1)) %>%
  select(x, meanP, psModel, psMethod, calibrated)
ggplot(vizData, aes(x = x, y = meanP, group = psModel, color = psModel)) +
  geom_point() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Mean precision") +
  labs(color = "PS model") +
  facet_grid(~psMethod)
ggsave(file.path(plotsAndTablesFolder, "Precision_pooled.png"), width = 6, height = 4, dpi = 300)

# Plot balance -------------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  mutate(psMethod = ifelse(analysisId == 1, "PS 1-on-1 matching", "PS stratification")) %>%
  inner_join(x, by = join_by(sampleSize)) 
ggplot(vizData, aes(x = x, y = maxSdmMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = maxSdmMin, lower = maxSdmP25, middle = maxSdmMedian, upper = maxSdmP75, ymax = maxSdmMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Max standardized difference of means (SDM)") +
  facet_grid(~psMethod)
ggsave(file.path(plotsAndTablesFolder, "Balance_pooled.png"), width = 6, height = 4, dpi = 300)

# Plot non-zero coefs -----------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  filter(analysisId == 1) %>%
  inner_join(x, by = join_by(sampleSize)) 
ggplot(vizData, aes(x = x, y = nonZeroCoefCountMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = nonZeroCoefCountMin, lower = nonZeroCoefCountP25, middle = nonZeroCoefCountMedian, upper = nonZeroCoefCountP75, ymax = nonZeroCoefCountMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Numer of non-zero coefficients in propensity model") 
ggsave(file.path(plotsAndTablesFolder, "NonZeroCoefes.png"), width = 6, height = 4, dpi = 300)

# Plot covariate count -----------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  filter(analysisId == 1) %>%
  inner_join(x, by = join_by(sampleSize)) 
ggplot(vizData, aes(x = x, y = covCountMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = covCountMin, lower = covCountP25, middle = covCountMedian, upper = covCountP75, ymax = covCountMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Numer of covariates")
ggsave(file.path(plotsAndTablesFolder, "Covariates.png"), width = 6, height = 4, dpi = 300)

# Plot PS AUC count -----------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  filter(analysisId == 1) %>%
  inner_join(x, by = join_by(sampleSize)) 
ggplot(vizData, aes(x = x, y = aucMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = aucMin, lower = aucP25, middle = aucMedian, upper = aucP75, ymax = aucMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Propensity model AUC") 
ggsave(file.path(plotsAndTablesFolder, "AuC.png"), width = 6, height = 4, dpi = 300)

# Plot EASE using pooling or meta-analysis -----------------------------------------------------
vizData <- metrics %>%
  bind_rows(metricsLargeSample) %>%
  filter(analysisId %in% c(1, 3, 101, 103, 201, 203), calibrated == FALSE) %>%
  mutate(psMethod = ifelse(analysisId %in% c(1, 101, 201), "PS 1-on-1 matching", "PS stratification")) %>%
  mutate(combiMethod = ifelse(analysisId %in% c(1, 3), "Pooling", ifelse(analysisId %in% c(101, 103), "Non-normal", "Normal"))) %>%
  mutate(combiMethod = ifelse(sampleSize == 20000, "Single site", combiMethod)) %>%
  inner_join(x, by = join_by(sampleSize)) %>%
  mutate(x = ifelse(combiMethod == "Normal", x + 0.2, ifelse(combiMethod == "Non-normal", x - 0.2, x))) %>%
  select(x, ease, easeCi95Lb, easeCi95Ub, combiMethod, psMethod)
ggplot(vizData, aes(x = x, y = ease, ymin = easeCi95Lb, ymax = easeCi95Ub, group = combiMethod, color = combiMethod)) +
  geom_point() +
  geom_errorbar() +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("EASE (95% CI)") +
  labs(color = "Synthesis") +
  facet_grid(~psMethod)
ggsave(file.path(plotsAndTablesFolder, "EASE_synthesis_methods.png"), width = 6, height = 4, dpi = 300)


# Show per-estimate shift for:
# Different types of synthesis vs overall. 
# Dimensions: 
# PS matching vs stratifcation
# Sample size
# Synthesis method

