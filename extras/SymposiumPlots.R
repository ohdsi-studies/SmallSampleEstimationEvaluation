# Plots for the Methods Workgroup meeting
library(dplyr)
library(ggplot2)

# plotsAndTablesFolder <- "C:/Users/mschuemi/OneDrive - JNJ/home/Research/SmallCountConfounderAdjustment"
plotsAndTablesFolder <- file.path("d:/SmallSampleEstimationEvaluation_mdcd", "plotsAndTables")
metrics <- bind_rows(
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcd", "CombinedMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcr", "CombinedMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_optum_ehr", "CombinedMetrics.rds"))
)
metrics <- metrics %>%
  mutate(comparison = gsub("Lisinporil ", "Lisinopril ", comparison)) %>%
  mutate(comparison = gsub("Hydrochlorothiazide", "HCTZ", comparison))
psMetrics <- bind_rows(
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcd", "CombinedPSMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_mdcr", "CombinedPsMetrics.rds")),
  readRDS(file.path("d:/SmallSampleEstimationEvaluation_optum_ehr", "CombinedPsMetrics.rds"))
)
psMetrics <- psMetrics %>%
  mutate(comparison = gsub("Lisinporil ", "Lisinopril ", comparison)) %>%
  mutate(comparison = gsub("Hydrochlorothiazide", "HCTZ", comparison))
saveRDS(metrics, file.path(plotsAndTablesFolder, "metrics.rds"))
saveRDS(psMetrics, file.path(plotsAndTablesFolder, "psMetrics.rds"))

# metrics <- readRDS(file.path(plotsAndTablesFolder, "metrics.rds"))
# psMetrics <- readRDS(file.path(plotsAndTablesFolder, "psMetrics.rds"))

sampleSizes <- c(4000, 2000, 1000, 500, 250, 125)
x <- tibble(
  sampleSize = c(20000, sampleSizes),
  x = seq_len(length(sampleSizes) + 1)
)

# EASE -----------------------------
vizData <- metrics %>%
  filter(calibrated == FALSE) %>%
  mutate(x = round(x)) %>%
  mutate(x = case_when(psModel == "Local" & psMethod == "PS stratification" ~ x - 0.32,
                       psModel == "Local" & psMethod == "PS 1-on-1 matching" ~ x - 0.16,
                       psModel == "Global" & psMethod == "PS stratification" ~ x,
                       psModel == "Global" & psMethod == "PS 1-on-1 matching" ~ x + 0.16,
                       psModel == "None" ~ x + 0.32)) %>%
  mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel)),
         comparison = gsub("vs ", "vs\n", comparison)) %>%
  select(comparison, x, ease, easeCi95Lb, easeCi95Ub, adjustment, psMethod, psModel, adjustment, database)
vizData$psMethod <- factor(vizData$psMethod, levels = rev(c("No PS adjustment",
                                                            "PS 1-on-1 matching",
                                                            "PS stratification")))
vizData$psModel <- factor(vizData$psModel, levels = c("Local",
                                                      "Global",
                                                      "None"))

plot <- ggplot(vizData, aes(x = x, y = ease, ymin = easeCi95Lb, ymax = easeCi95Ub, group = adjustment, color = psModel)) +
  # geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = x$x + 0.5, color = gray(0.8), linewidth = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_errorbar(alpha = 0.8, linewidth = 0.5) +
  geom_point(aes(shape = psMethod), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("#69AED5", "#336B91", "#EB6622")) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Expected Absolute Systematic Error (EASE) with 95% CI") +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(color = "PS model", shape = "Adjustment strategy") +
  facet_grid(comparison~database) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = gray(0.8), linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = gray(0.8), linewidth = 0.5),
        strip.background = element_blank())

ggsave(file.path(plotsAndTablesFolder, "EASE_crude_all.png"), plot = plot, width = 9, height = 4.6, dpi = 300)

# Precision after calibration --------------------------
vizData <- metrics %>%
  filter(calibrated == TRUE) %>%
  mutate(x = case_when(psMethod == "PS stratification" ~ x + 0.3, 
                       psMethod == "No PS adjustment" ~ x - 0.3,
                       .default = x - 0.1)) %>%
  mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel)),
         comparison = gsub("vs ", "vs\n", comparison)) %>%
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


# Balance -----------------------------
vizData <- psMetrics %>%
  mutate(label = case_when(
    analysisId == 3 ~ "Stratification, local model",
    analysisId == 1 ~ "Matching, local model",
    analysisId == 5 ~ "Stratification, global model",
    analysisId == 4 ~ "Matching, global model")) %>%
  mutate(x = case_when(
    analysisId == 3 ~ x - 0.34,
    analysisId == 1 ~ x - 0.12,
    analysisId == 5 ~ x + 0.12,
    analysisId == 4 ~ x + 0.34)) %>%
  mutate(comparison = gsub("vs ", "vs\n", comparison)) %>%
  select(comparison, x, maxSdmMin, maxSdmP25, maxSdmMedian, maxSdmP75, maxSdmMax, label, database)
vizData$label <- factor(vizData$label, levels = c("Stratification, local model",
                                                  "Matching, local model",
                                                  "Stratification, global model",
                                                  "Matching, global model"))
plot <- ggplot(vizData, aes(x = x, y = maxSdmMedian, group = x, color = label, fill = label)) +
  geom_vline(xintercept = x$x + 0.5, color = gray(0.8), linewidth = 0.5) +
  geom_hline(yintercept = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_boxplot(
    aes(ymin = maxSdmMin, lower = maxSdmP25, middle = maxSdmMedian, upper = maxSdmP75, ymax = maxSdmMax),
    stat = "identity",
    alpha = 0.3,
    linewidth = 0.5
  ) +
  scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
  scale_y_continuous("Maximum absolute standardized difference of mean (SDM)") +
  scale_color_manual(values=c("#69AED5", "#336B91", "#FCC90C", "#EB6622")) +
  scale_fill_manual(values=c("#69AED5", "#336B91", "#FCC90C", "#EB6622")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(color = "Adjustment strategy", fill = "Adjustment strategy") +
  facet_grid(comparison~database) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = gray(0.8), linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = gray(0.8), linewidth = 0.5),
        strip.background = element_blank())
ggsave(file.path(plotsAndTablesFolder, "Balance_all.png"), plot = plot, width = 9, height = 4.5, dpi = 300)
