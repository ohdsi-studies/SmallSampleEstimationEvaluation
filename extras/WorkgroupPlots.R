# Plots for the Methods Workgroup meeting
library(dplyr)
library(ggplot2)

metrics <- bind_rows(
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcd", "CombinedMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcr", "CombinedMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_optum_ehr", "CombinedMetrics.rds"))
)
metrics <- metrics |>
  mutate(comparison = gsub("Lisinporil ", "Lisinopril ", comparison))
psMetrics <- bind_rows(
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcd", "CombinedPSMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcr", "CombinedPsMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_optum_ehr", "CombinedPsMetrics.rds"))
)
psMetrics <- psMetrics |>
  mutate(comparison = gsub("Lisinporil ", "Lisinopril ", comparison))
plotsAndTablesFolder <- file.path("d:/SmallSampleEstimationEvaluation_mdcd", "plotsAndTables")

sampleSizes <- c(4000, 2000, 1000, 500, 250, 125)
x <- tibble(
  sampleSize = c(20000, sampleSizes),
  x = seq_len(length(sampleSizes) + 1)
)
saveRDS(metrics, file.path(plotsAndTablesFolder, "metrics.rds"))
saveRDS(psMetrics, file.path(plotsAndTablesFolder, "psMetrics.rds"))

# EASE -----------------------------
vizData <- metrics |>
  filter(calibrated == FALSE) |>
  mutate(x = case_when(psMethod == "PS stratification" ~ x + 0.3, 
                       psMethod == "No PS adjustment" ~ x - 0.3,
                       .default = x - 0.1)) |>
  mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel)),
         comparison = gsub("vs ", "vs\n", comparison)) |>
  select(comparison, x, ease, easeCi95Lb, easeCi95Ub, adjustment, psMethod, psModel, adjustment, database)

ggplot(vizData, aes(x = x, y = ease, ymin = easeCi95Lb, ymax = easeCi95Ub, group = adjustment, color = psModel)) +
  geom_vline(xintercept = 0.5 + 2:5, color = "white") +
  geom_hline(yintercept = c(0.1, 0.25), linetype = "dashed") +
  geom_errorbar() +
  geom_point(aes(shape = psMethod), size = 2) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Expected Absolute Systematic Error (EASE) with 95% credible interval") +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(color = "Propensity model", shape = "Adjustment strategy") +
  facet_grid(comparison~database) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        panel.grid.major.x = element_blank())
ggsave(file.path(plotsAndTablesFolder, "EASE_crude_all.png"), width = 14, height = 7, dpi = 300)

# Precision after calibration --------------------------
vizData <- metrics |>
  filter(calibrated == TRUE) |>
  mutate(x = case_when(psMethod == "PS stratification" ~ x + 0.3, 
                       psMethod == "No PS adjustment" ~ x - 0.3,
                       .default = x - 0.1)) |>
  mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel)),
         comparison = gsub("vs ", "vs\n", comparison)) |>
  select(comparison, x, meanP, adjustment, psMethod, psModel, adjustment, database)

ggplot(vizData, aes(x = x, y = meanP, group = adjustment, color = psModel)) +
  geom_vline(xintercept = 0.5 + 2:5, color = "white") +
  geom_point(aes(shape = psMethod), size = 2) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Geometric mean precision after empirical calibration") +
  # coord_cartesian(ylim = c(0, 0.5)) +
  labs(color = "Propensity model", shape = "Adjustment strategy") +
  facet_grid(comparison~database) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        panel.grid.major.x = element_blank())
ggsave(file.path(plotsAndTablesFolder, "Precision_afer_calibration_crude_all.png"), width = 14, height = 7, dpi = 300)

# EASE before PS adjustment -----------------------------------------
metricsLargeSample <- bind_rows(
  readr::read_csv(file.path("d:/SmallSampleEstimationEvaluation_mdcd", "Metrics_LargeSample.csv")),
  readr::read_csv(file.path("d:/SmallSampleEstimationEvaluation_mdcr", "Metrics_LargeSample.csv")),
  readr::read_csv(file.path("d:/SmallSampleEstimationEvaluation_optum_ehr", "Metrics_LargeSample.csv"))
)
vizData <- metricsLargeSample |>
  filter(calibrated == FALSE & analysisId == 2) |>
  mutate(group = case_when(ease < 0.1 ~ "L", ease < 0.25 ~ "M", .default = "H")) |>
  mutate(label = sprintf("%0.2f (%0.2f-%0.2f) %s", ease, easeCi95Lb, easeCi95Ub, group)) |>
  inner_join(metrics |>
               distinct(targetId, comparatorId, comparison),
             by = join_by(targetId, comparatorId)) |>
  select(comparison, database, label) |>
  arrange(database, comparison)
vizData <- tidyr::pivot_wider(vizData, names_from = "database", values_from = "label")
readr::write_excel_csv(vizData, file.path("d:/SmallSampleEstimationEvaluation_mdcd", "crude_ease.csv"))

# Balance -----------------------------
vizData <- psMetrics |>
  # filter(analysisId == 1) |>
  mutate(comparison = gsub("vs ", "vs\n", comparison)) |>
  mutate(x = case_when(psMethod == "PS stratification" ~ x + 0.2, 
                       .default = x - 0.2)) |>
  select(comparison, x, maxSdmMin, maxSdmP25, maxSdmMedian, maxSdmP75, maxSdmMax, psMethod, database)

ggplot(vizData, aes(x = x, y = maxSdmMedian, group = x, color = psMethod)) +
  geom_vline(xintercept = 0.5 + 1:5, color = "white") +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  geom_boxplot(
    aes(ymin = maxSdmMin, lower = maxSdmP25, middle = maxSdmMedian, upper = maxSdmP75, ymax = maxSdmMax),
    stat = "identity",
    alpha = 0.5
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Maximum absolute standardized difference of mean (SDM)") +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(color = "Adjustment strategy") +
  facet_grid(comparison~database) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        panel.grid.major.x = element_blank())
ggsave(file.path(plotsAndTablesFolder, "Balance_all.png"), width = 14, height = 7, dpi = 300)
