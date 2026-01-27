library(ggpattern)

generate_all_plots <- function(baseFolder) {
  plotsAndTablesFolder <- file.path(baseFolder, "plotsAndTables")
  dir.create(plotsAndTablesFolder)
  metrics <- bind_rows(readRDS(file.path(baseFolder, "CombinedMetrics.rds")))
  
  metrics <- metrics |>
    mutate(comparison = gsub("Lisinporil ", "Lisinopril ", comparison)) |>
    mutate(comparison = gsub("Hydrochlorothiazide", "HCTZ", comparison))
  psMetrics <- bind_rows(
    readRDS(file.path(baseFolder, "CombinedPSMetrics.rds")))
  
  psMetrics <- psMetrics |>
    mutate(comparison = gsub("Lisinporil ", "Lisinopril ", comparison)) |>
    mutate(comparison = gsub("Hydrochlorothiazide", "HCTZ", comparison))
  saveRDS(metrics, file.path(plotsAndTablesFolder, "metrics.rds"))
  saveRDS(psMetrics, file.path(plotsAndTablesFolder, "psMetrics.rds"))
  
  sampleSizes <- c(4000, 2000, 1000, 500, 250)
  x <- tibble(
    sampleSize = c(20000, sampleSizes),
    x = seq_len(length(sampleSizes) + 1)
  )
  
  # EASE -----------------------------
  vizData <- metrics |>
    filter(calibrated == FALSE) |>
    mutate(x = round(x)) |>
    mutate(x = case_when(psModel == "Local" & psMethod == "PS stratification" ~ x - 0.32,
                         psModel == "Local" & psMethod == "PS 1-on-1 matching" ~ x - 0.16,
                         psModel == "Global" & psMethod == "PS stratification" ~ x,
                         psModel == "Global" & psMethod == "PS 1-on-1 matching" ~ x + 0.16,
                         psModel == "None" ~ x + 0.32)) |>
    mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel)),
           comparison = gsub("vs ", "vs\n", comparison)) |>
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
    coord_cartesian(ylim = c(0, 0.75)) +
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
  
  ggsave(file.path(plotsAndTablesFolder, "EASE_crude_all.png"), plot = plot, width = 5, height = 7.5, dpi = 400)
  
  
  # Precision after calibration --------------------------
  vizData <- metrics |>
    filter(calibrated == TRUE) |>
    mutate(x = round(x)) |>
    mutate(x = case_when(psModel == "Local" & psMethod == "PS stratification" ~ x - 0.32,
                         psModel == "Local" & psMethod == "PS 1-on-1 matching" ~ x - 0.16,
                         psModel == "Global" & psMethod == "PS stratification" ~ x,
                         psModel == "Global" & psMethod == "PS 1-on-1 matching" ~ x + 0.16,
                         psModel == "None" ~ x + 0.32)) |>
    mutate(adjustment = sprintf("%s using %s model", psMethod, tolower(psModel)),
           comparison = gsub("vs ", "vs\n", comparison)) |>
    select(comparison, x, meanP, adjustment, psMethod, psModel, adjustment, database)
  vizData$psMethod <- factor(vizData$psMethod, levels = rev(c("No PS adjustment",
                                                              "PS 1-on-1 matching",
                                                              "PS stratification")))
  vizData$psModel <- factor(vizData$psModel, levels = c("Local",
                                                        "Global",
                                                        "None"))
  
  plot <- ggplot(vizData, aes(x = x, y = meanP, group = adjustment, fill = psModel, pattern = psMethod)) +
    geom_vline(xintercept = 0.5 + 2:5, color = "white") +
    geom_bar_pattern(stat = "identity",
                     width = 0.16,
                     color = "black", 
                     pattern_fill = "black",
                     pattern_angle = 45,
                     pattern_density = 0.025,
                     pattern_spacing = 0.1,
                     pattern_key_scale_factor = 0.25,
                     alpha = 0.8) +
    scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
    scale_y_continuous("Geometric mean precision after empirical calibration") +
    scale_fill_manual(values = c("#69AED5", "#336B91", "#EB6622")) +
    scale_pattern_manual(values = c("PS 1-on-1 matching" = "stripe", "PS stratification" = "none", "No PS adjustment" = "none")) +
    labs(fill = "Propensity model", pattern = "Adjustment strategy") +
    guides(pattern = guide_legend(override.aes = list(fill = "white")),
           fill = guide_legend(override.aes = list(pattern = "none"))) +
    facet_grid(comparison~database, scales = "free_y") +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = gray(0.8), linewidth = 0.5),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(color = gray(0.8), linewidth = 0.5),
          strip.background = element_blank())
  
  ggsave(file.path(plotsAndTablesFolder, "Precision_afer_calibration_crude_all.png"), plot = plot, width = 5, height = 7.5, dpi = 400)
  
  # Balance -----------------------------
  vizData <- psMetrics |>
    mutate(label = case_when(
      analysisId == 3 ~ "Stratification, local model",
      analysisId == 1 ~ "Matching, local model",
      analysisId == 5 ~ "Stratification, global model",
      analysisId == 4 ~ "Matching, global model")) |>
    mutate(x = case_when(
      analysisId == 3 ~ x - 0.34,
      analysisId == 1 ~ x - 0.12,
      analysisId == 5 ~ x + 0.12,
      analysisId == 4 ~ x + 0.34)) |>
    mutate(comparison = gsub("vs ", "vs\n", comparison)) |>
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
    scale_color_manual(values=c("#69AED5", "#336B91", "#FBC511", "#EB6622")) +
    scale_fill_manual(values=c("#69AED5", "#336B91", "#FBC511", "#EB6622")) +
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
  ggsave(file.path(plotsAndTablesFolder, "Balance_all.png"), plot = plot, width = 5, height = 7.5, dpi = 400)
  
  # New balance -----------------------------
  # Adding 1 to avoid infinite values on log scale:
  vizData <- psMetrics |>
    mutate(label = case_when(
      analysisId == 3 ~ "Stratification, local model",
      analysisId == 1 ~ "Matching, local model",
      analysisId == 5 ~ "Stratification, global model",
      analysisId == 4 ~ "Matching, global model")) |>
    mutate(x = case_when(
      analysisId == 3 ~ x - 0.34,
      analysisId == 1 ~ x - 0.12,
      analysisId == 5 ~ x + 0.12,
      analysisId == 4 ~ x + 0.34)) |>
    mutate(comparison = gsub("vs ", "vs\n", comparison)) |>
    select(comparison, x, significantUnbalancedMin, significantUnbalancedP25, significantUnbalancedMedian, significantUnbalancedP75, significantUnbalancedMax, label, database) |>
    mutate(
      significantUnbalancedMin = significantUnbalancedMin + 1,
      significantUnbalancedP25 = significantUnbalancedP25 + 1,
      significantUnbalancedMedian = significantUnbalancedMedian + 1,
      significantUnbalancedP75 = significantUnbalancedP75 + 1,
      significantUnbalancedMax = significantUnbalancedMax + 1,
    )
  vizData$label <- factor(vizData$label, levels = c("Stratification, local model",
                                                    "Matching, local model",
                                                    "Stratification, global model",
                                                    "Matching, global model"))
  yBreaks <- c(0, 10, 100, 1000)
  plot <- ggplot(vizData, aes(x = x, y = significantUnbalancedMedian, group = x, color = label, fill = label)) +
    geom_vline(xintercept = x$x + 0.5, color = gray(0.8), linewidth = 0.5) +
    geom_boxplot(
      aes(ymin = significantUnbalancedMin, lower = significantUnbalancedP25, middle = significantUnbalancedMedian, upper = significantUnbalancedP75, ymax = significantUnbalancedMax),
      stat = "identity",
      alpha = 0.3,
      linewidth = 0.5
    ) +
    scale_x_continuous("Sample size per site", breaks = x$x, labels = x$sampleSize, minor_breaks = NULL) +
    scale_y_log10("Number of covariates signficantly unbalanced", breaks = yBreaks + 1, labels = yBreaks ) +
    scale_color_manual(values=c("#69AED5", "#336B91", "#FBC511", "#EB6622")) +
    scale_fill_manual(values=c("#69AED5", "#336B91", "#FBC511", "#EB6622")) +
    coord_cartesian(ylim = c(1, 1000)) +
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
  plot
  ggsave(file.path(plotsAndTablesFolder, "NewBalance_all.png"), plot = plot, width = 5, height = 7.5, dpi = 400)

  # New balance meta analysis -----------------------------
  balanceMetaFolder <- file.path(baseFolder, "BalanceMetaAnalysis")

  metaFiles <- list.files(
    balanceMetaFolder,
    pattern = "^Balance_s.*\\.rds$",
    full.names = TRUE
  )

  psMetricsMeta <- purrr::map_dfr(metaFiles, function(f) {

    info <- stringr::str_match(
      basename(f),
      "Balance_s(\\d+)_a(\\d+)_t(\\d+)_c(\\d+)"
    )

    balance <- readRDS(f)

    tibble(
      sampleSize   = as.integer(info[2]),
      analysisId   = as.integer(info[3]),
      targetId     = as.integer(info[4]),
      comparatorId = as.integer(info[5]),
      significantUnbalanced =
        sum(!balance$afterMatchingBalanced, na.rm = TRUE)
    )
  })

  comparisonLookup <- psMetrics |>
    distinct(targetId, comparatorId, comparison, database)

  vizData <- psMetricsMeta |>
    left_join(comparisonLookup,
              by = c("targetId", "comparatorId")) |>
    left_join(x, by = "sampleSize") |>
    mutate(
      comparison = gsub("vs ", "vs\n", comparison),
      label = case_when(
        analysisId == 3 ~ "Stratification, local model",
        analysisId == 1 ~ "Matching, local model",
        analysisId == 5 ~ "Stratification, global model",
        analysisId == 4 ~ "Matching, global model"
      ),
      x = case_when(
        analysisId == 3 ~ x - 0.34,
        analysisId == 1 ~ x - 0.12,
        analysisId == 5 ~ x + 0.12,
        analysisId == 4 ~ x + 0.34
      )
    )

  vizData$label <- factor(
    vizData$label,
    levels = c(
      "Stratification, local model",
      "Matching, local model",
      "Stratification, global model",
      "Matching, global model"
    )
  )

  plot <- ggplot(
    vizData,
    aes(
      x = x,
      y = significantUnbalanced,
      color = label
    )
  ) +
    geom_vline(
      xintercept = x$x + 0.5,
      color = gray(0.8),
      linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_point(size = 2, alpha = 0.9) +
    scale_x_continuous(
      "Sample size per site",
      breaks = x$x,
      labels = x$sampleSize,
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      "Number of covariates significantly unbalanced (meta-analysis)"
    ) +
    scale_color_manual(
      values = c("#69AED5", "#336B91", "#FBC511", "#EB6622")
    ) +
    labs(color = "Adjustment strategy") +
    facet_grid(comparison ~ database) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      panel.background = element_blank(),
      panel.grid.major.y = element_line(color = gray(0.8), linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = gray(0.8), linewidth = 0.5),
      strip.background = element_blank()
    )

  ggsave(
    file.path(plotsAndTablesFolder, "NewBalance_MetaAnalysis_points.png"),
    plot = plot,
    width = 5,
    height = 7.5,
    dpi = 400
  )
}
