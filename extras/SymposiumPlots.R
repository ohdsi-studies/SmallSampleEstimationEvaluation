# Plots for the OHDSI Global Symposium collaborator showcase submission
library(dplyr)

metrics <- bind_rows(
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcd", "CombinedMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcr", "CombinedMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_optum_ehr", "CombinedMetrics.rds"))
)
metrics <- metrics %>%
  mutate(comparison = gsub("Lisinporil ", "Lisinopril ", comparison))
plotsAndTablesFolder <- file.path("d:/SmallSampleEstimationEvaluation_mdcd", "plotsAndTables")

# EASE -----------------------------
vizData <- metrics %>%
  filter(analysisId %in% c(1, 3, 4, 5) & calibrated == FALSE) %>%
  mutate(x = if_else(psMethod == "PS stratification", x + 0.2, x - 0.2)) %>%
  mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel))) %>%
  select(comparison, x, ease, easeCi95Lb, easeCi95Ub, adjustment, psMethod, psModel, adjustment, database)

ggplot(vizData, aes(x = x, y = ease, ymin = easeCi95Lb, ymax = easeCi95Ub, group = adjustment, color = psModel)) +
  geom_vline(xintercept = 0.5 + 2:5, color = "white") +
  geom_errorbar() +
  geom_point(aes(shape = psMethod), size = 2) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Expected Absolute Systematic Error (EASE) with 95% credible interval") +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(color = "PS model", shape = "Adjustment strategy") +
  facet_grid(comparison~database) +
  theme(legend.position = "top",
        legend.direction = "vertical",
        panel.grid.major.x = element_blank())
ggsave(file.path(plotsAndTablesFolder, "EASE_all.png"), width = 8, height = 9.5, dpi = 300)

# Precision after calibration --------------------------
vizData <- metrics %>%
  filter(analysisId %in% c(1, 3, 4, 5) & calibrated == TRUE) %>%
  mutate(x = if_else(psMethod == "PS stratification", x + 0.2, x - 0.2)) %>%
  mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel))) %>%
  select(comparison, x, meanP, adjustment, psMethod, psModel, adjustment, database)

ggplot(vizData, aes(x = x, y = meanP, group = adjustment, color = psModel)) +
  geom_vline(xintercept = 0.5 + 2:5, color = "white") +
  geom_point(aes(shape = psMethod), size = 2) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Geometric mean precision after empirical calibration") +
  # coord_cartesian(ylim = c(0, 0.5)) +
  labs(color = "PS model", shape = "Adjustment strategy") +
  facet_grid(comparison~database) +
  theme(legend.position = "top",
        legend.direction = "vertical",
        panel.grid.major.x = element_blank())
ggsave(file.path(plotsAndTablesFolder, "Precision_afer_calibration_all.png"), width = 8, height = 9.5, dpi = 300)

# EASE before PS adjustment -----------------------------------------
metricsLargeSample <- bind_rows(
  readr::read_csv(file.path("d:/SmallSampleEstimationEvaluation_mdcd", "Metrics_LargeSample.csv")),
  readr::read_csv(file.path("d:/SmallSampleEstimationEvaluation_mdcr", "Metrics_LargeSample.csv")),
  readr::read_csv(file.path("d:/SmallSampleEstimationEvaluation_optum_ehr", "Metrics_LargeSample.csv"))
)
vizData <- metricsLargeSample %>%
  filter(calibrated == FALSE & analysisId == 2) %>%
  mutate(database = if_else(database == "Optum EHR", "Clinformatics", database)) %>%
  mutate(group = case_when(ease < 0.1 ~ "L", ease < 0.25 ~ "M", .default = "H")) %>%
  mutate(label = sprintf("%0.2f (%0.2f-%0.2f) %s", ease, easeCi95Lb, easeCi95Ub, group)) %>%
  inner_join(metrics %>%
               distinct(targetId, comparatorId, comparison),
             by = join_by(targetId, comparatorId)) %>%
  select(comparison, database, label) %>%
  arrange(database, comparison)
vizData <- tidyr::pivot_wider(vizData, names_from = "database", values_from = "label")
readr::write_excel_csv(vizData, file.path("d:/SmallSampleEstimationEvaluation_mdcd", "crude_ease.csv"))
