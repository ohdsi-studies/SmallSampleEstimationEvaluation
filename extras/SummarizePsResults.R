library(ggplot2)
library(dplyr)
sampleSizes <- c(2000, 1000, 500, 250, 100)

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
x <- tibble(
  sampleSize = sampleSizes,
  x = seq_along(sampleSizes)
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
  scale_x_continuous("Sample size", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("EASE") +
  facet_grid(~psMethod)

# Plot precision using local or global propensity model, data pooling -----------------------------------------------------
vizData <- metrics %>%
  filter(analysisId %in% c(1, 3, 4, 5)) %>%
  mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", "PS stratification")) %>%
  mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", "Global")) %>%
  inner_join(x, by = join_by(sampleSize)) %>%
  mutate(x = ifelse(psModel == "Local", x + 0.1, x - 0.1)) %>%
  select(x, meanP, psModel, psMethod, calibrated)

ggplot(vizData, aes(x = x, y = meanP, group = psModel, color = psModel, shape = calibrated)) +
  geom_point() +
  scale_x_continuous("Sample size", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Mean precision") +
  facet_grid(~psMethod)

# Plot balance -------------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  mutate(psMethod = ifelse(analysisId == 1, "PS 1-on-1 matching", "PS stratification")) %>%
  inner_join(x, by = join_by(sampleSize)) 

ggplot(vizData, aes(x = x, y = maxSdmMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = maxSdmMin, lower = maxSdmP25, middle = maxSdmMedian, upper = maxSdmP75, ymax = maxSdmMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Max SDM") +
  facet_grid(~psMethod)

# Plot non-zero coefs -----------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  filter(analysisId == 1) %>%
  inner_join(x, by = join_by(sampleSize)) 

ggplot(vizData, aes(x = x, y = nonZeroCoefCountMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = nonZeroCoefCountMin, lower = nonZeroCoefCountP25, middle = nonZeroCoefCountMedian, upper = nonZeroCoefCountP75, ymax = nonZeroCoefCountMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Numer of non-zero coefficients in propensity model") 

# Plot covariate count -----------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  filter(analysisId == 1) %>%
  inner_join(x, by = join_by(sampleSize)) 

ggplot(vizData, aes(x = x, y = covCountMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = covCountMin, lower = covCountP25, middle = covCountMedian, upper = covCountP75, ymax = covCountMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Numer of covariates")

# Plot PS AUC count -----------------------------------------------------------------------------------------------------
vizData <- psMetrics %>%
  filter(analysisId == 1) %>%
  inner_join(x, by = join_by(sampleSize)) 

ggplot(vizData, aes(x = x, y = aucMedian, group = sampleSize)) +
  geom_boxplot(
    aes(ymin = aucMin, lower = aucP25, middle = aucMedian, upper = aucP75, ymax = aucMax),
    stat = "identity"
  ) +
  scale_x_continuous("Sample size", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("PS AUC") 
