library(ggplot2)
library(dplyr)

outputFolder <- "d:/SmallSampleEstimationEvaluation"

plotsAndTablesFolder <- file.path(outputFolder, "plotsAndTables")

tcs <- read.csv(file.path(outputFolder, "allControls.csv")) |>
  distinct(targetId, targetName, comparatorId, comparatorName) |>
  mutate(comparison = sprintf("%s vs %s", targetName, comparatorName))
estimates <- readRDS(file.path(outputFolder, "largeSample", "resultsSummary.rds")) |>
  select(analysisId, targetId, comparatorId, outcomeId, logRr, seLogRr) |>
  mutate(sampleSize = 20000)
sampleSizes <- c(4000, 2000, 1000, 500, 250)
for (sampleSize in sampleSizes) {
  temp <- readRDS(file.path(outputFolder, sprintf("smallSample%d", sampleSize), "resultsSummary.rds")) |>
    select(analysisId, targetId, comparatorId, outcomeId, logRr, seLogRr) |>
    mutate(sampleSize = !!sampleSize)
  estimates <- bind_rows(estimates, temp)  
}
allControls <- readr::read_csv(file.path(outputFolder, "allControls.csv"), show_col_types = FALSE)
estimates <- estimates  |>
  inner_join(
    allControls |>
      select(targetId, comparatorId, outcomeId, targetEffectSize), 
    by = join_by(targetId, comparatorId, outcomeId)
  )

plotShift <- function(fromAnalysisIds, 
                      toAnalysisIds = NULL, 
                      from20K = TRUE, 
                      targetId,
                      comparatorId,
                      labels = c("PS 1-on-1 matching", "PS stratification"),
                      title,
                      fileName) {
  mapping <- tibble(
    analysisId = fromAnalysisIds,
    toAnalysisId = toAnalysisIds,
    label = labels
  )
  if (from20K) {
    vizData <- estimates |>
      filter(sampleSize == 20000 & targetEffectSize == 1 & analysisId %in% fromAnalysisIds & targetId == !!targetId & comparatorId == !!comparatorId) |>
      select(analysisId, targetId, comparatorId, outcomeId, startLogRr = logRr, startSeLogRr = seLogRr) |>
      inner_join(
        estimates |>
          filter(sampleSize != 20000 & targetEffectSize == 1 & analysisId %in% toAnalysisIds) |>
          rename(toAnalysisId = analysisId) |>
          inner_join(
            mapping,
            by = join_by(toAnalysisId)
          ),
        by = join_by(analysisId, targetId, comparatorId, outcomeId),
        multiple = "all"
      ) 
  } else {
    vizData <- estimates |>
      filter(sampleSize != 20000 & targetEffectSize == 1 & analysisId %in% fromAnalysisIds & targetId == !!targetId & comparatorId == !!comparatorId) |>
      select(analysisId, targetId, comparatorId, outcomeId, sampleSize, startLogRr = logRr, startSeLogRr = seLogRr) |>
      inner_join(
        estimates |>
          filter(sampleSize != 20000 & targetEffectSize == 1 & analysisId %in% toAnalysisIds) |>
          rename(toAnalysisId = analysisId) |>
          inner_join(
            mapping,
            by = join_by(toAnalysisId)
          ),
        by = join_by(analysisId, targetId, comparatorId, outcomeId, sampleSize),
        multiple = "all"
      ) 
  }
  x <- exp(seq(log(0.1), log(10), by = 0.01))
  y <- sapply(x, FUN = function(x) {
    abs(log(x)) / qnorm(0.975)
  })
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 8)
  ggplot(vizData, aes(x = exp(startLogRr), y = startSeLogRr)) +
    geom_line(aes(x = x, y = y), linetype = "dashed", data = data.frame(x = x, y = y)) +
    geom_segment(aes(xend = exp(logRr), yend = seLogRr), arrow = arrow(length = unit(0.02, "inches"), type = "closed"), alpha = 0.25) +
    scale_x_continuous("Hazard ratio", trans = "log10", breaks = breaks, labels = breaks, minor_breaks = NULL, limits = c(0.1, 10)) +
    scale_y_continuous("Standard error", breaks = c(0, 1), minor_breaks = NULL) +
    coord_cartesian(ylim = c(0, 1)) +
    facet_grid(sampleSize ~ label) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(file.path(plotsAndTablesFolder, fileName), width = 6, height = 8, dpi = 300)
}

plotShift(
  fromAnalysisIds = c(1, 3),
  toAnalysisIds = c(1, 3),
  targetId = 1,
  from20K = TRUE,
  title = "ACE inhibitors vs thiazides\nFrom single large study to pooled across smaller sites",
  fileName = "ShiftVsPooled_t1.png"
)
plotShift(
  fromAnalysisIds = c(1, 3),
  toAnalysisIds = c(1, 3),
  targetId = 3,
  from20K = TRUE,
  title = "Sitaglipton vs liraglutide\nFrom single large study to pooled across smaller sites",
  fileName = "ShiftVsPooled_t3.png"
)
plotShift(
  fromAnalysisIds = c(1, 3),
  toAnalysisIds = c(201, 203),
  from20K = TRUE,
  title = "From single large study to normal synthesis across smaller sites",
  fileName = "ShiftVsDl.png"
)
plotShift(
  fromAnalysisIds = c(1, 3),
  toAnalysisIds = c(101, 103),
  from20K = TRUE,
  title = "From single large study to non-normal synthesis across smaller sites",
  fileName = "ShiftVsProfiling.png"
)
plotShift(
  fromAnalysisIds = c(1, 3),
  toAnalysisIds = c(4, 5),
  from20K = FALSE,
  title = "From local to global propensity model",
  fileName = "ShiftLocalVsGlobalPooling.png"
)

plotEstimates <- function(analysisId, targetId, comparatorId, title, fileName) {
  vizData <- estimates |>
    filter(sampleSize != 20000 & analysisId == !!analysisId & targetId == !!targetId & comparatorId == !!comparatorId)
  
  d <- tibble(
    logRr = vizData$logRr,
    seLogRr = vizData$seLogRr,
    trueLogRr = log(vizData$targetEffectSize),
    trueRr = vizData$targetEffectSize,
    logCi95lb = logRr + qnorm(0.025) * seLogRr,
    logCi95ub = logRr + qnorm(0.975) * seLogRr,
    sampleSize = vizData$sampleSize
  )
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$seLogRr), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Significant <- d$logCi95lb > d$trueLogRr | d$logCi95ub < d$trueLogRr
  
  temp1 <- aggregate(Significant ~ trueRr + sampleSize, data = d, length)
  temp2 <- aggregate(Significant ~ trueRr + sampleSize, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  temp2$meanLabel <- paste0(
    formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
    "% of CIs includes ",
    temp2$trueRr
  )
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 10)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 10, hjust = 1)
  
  d$Group <- paste("True hazard ratio =", d$trueRr)
  dd$Group <- paste("True hazard ratio =", dd$trueRr)
  d$yGroup <- d$sampleSize
  dd$yGroup <- dd$sampleSize
  
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$logRr, y = .data$seLogRr)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.025), slope = 1 / qnorm(0.025)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.975), slope = 1 / qnorm(0.975)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    ggplot2::geom_point(
      shape = 16,
      size = 2,
      alpha = 0.5,
      color = rgb(0, 0, 0.8)
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.15), y = 0.95, alpha = 1, hjust = "left", ggplot2::aes(label = .data$nLabel), size = 3.5, data = dd) +
    ggplot2::geom_label(x = log(0.15), y = 0.8, alpha = 1, hjust = "left", ggplot2::aes(label = .data$meanLabel), size = 3.5, data = dd) +
    ggplot2::scale_x_continuous("Hazard Ratio", limits = log(c(0.1, 10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::facet_grid(yGroup ~ Group) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      legend.key = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.text.x = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      legend.position = "none"
    )
  # plot
  ggsave(file.path(plotsAndTablesFolder, fileName), plot = plot, width = 9, height = 10, dpi = 300)
}
  
# for (i in seq_len(nrow(tcs))) {
for (i in 3:4) {
  row <- tcs[i, ]
  plotEstimates(analysisId = 1,
                targetId = row$targetId, 
                comparatorId = row$comparatorId, 
                title = sprintf("%s, PS matching, pooling", row$comparison),
                fileName = sprintf("AllEstimatesPsMatchPooling_t%d_c_%d.png", row$targetId, row$comparatorId))
  plotEstimates(analysisId = 3,
                targetId = row$targetId, 
                comparatorId = row$comparatorId, 
                title = sprintf("%s, PS stratification, pooling", row$comparison),
                fileName = sprintf("AllEstimatesPsstratificationPooling_t%d_c_%d.png", row$targetId, row$comparatorId))
  plotEstimates(analysisId = 101,
                targetId = row$targetId, 
                comparatorId = row$comparatorId, 
                title = sprintf("%s, PS matching, non-normal synthesis", row$comparison),
                fileName = sprintf("AllEstimatesPsMatchNonNormal_t%d_c_%d.png", row$targetId, row$comparatorId))
  plotEstimates(analysisId = 103,
                targetId = row$targetId, 
                comparatorId = row$comparatorId, 
                title = sprintf("%s, PS stratification, non-normal synthesis", row$comparison),
                fileName = sprintf("AllEstimatesPsstratificationNonNormal_t%d_c_%d.png", row$targetId, row$comparatorId))
  plotEstimates(analysisId = 201,
                targetId = row$targetId, 
                comparatorId = row$comparatorId, 
                title = sprintf("%s, PS matching, normal synthesis", row$comparison),
                fileName = sprintf("AllEstimatesPsMatchNormal_t%d_c_%d.png", row$targetId, row$comparatorId))
  plotEstimates(analysisId = 203,
                targetId = row$targetId, 
                comparatorId = row$comparatorId, 
                title = sprintf("%s, PS stratification, normal synthesis", row$comparison),
                fileName = sprintf("AllEstimatesPsstratificationNormal_t%d_c_%d.png", row$targetId, row$comparatorId))
}
