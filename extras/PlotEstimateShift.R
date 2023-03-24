library(ggplot2)
library(dplyr)

outputFolder <- "d:/SmallSampleEstimationEvaluation"

plotsAndTablesFolder <- file.path(outputFolder, "plotsAndTables")

estimates <- readRDS(file.path(outputFolder, "largeSample", "resultsSummary.rds")) %>%
  select(analysisId, targetId, comparatorId, outcomeId, logRr, seLogRr) %>%
  mutate(sampleSize = 20000)
sampleSizes <- c(4000, 2000, 1000, 500, 250, 100)
for (sampleSize in sampleSizes) {
  temp <- readRDS(file.path(outputFolder, sprintf("smallSample%d", sampleSize), "resultsSummary.rds")) %>%
    select(analysisId, targetId, comparatorId, outcomeId, logRr, seLogRr) %>%
    mutate(sampleSize = !!sampleSize)
  estimates <- bind_rows(estimates, temp)  
}
allControls <- readr::read_csv(file.path(outputFolder, "allControls.csv"), show_col_types = FALSE)
estimates <- estimates  %>%
  inner_join(
    allControls %>%
      select(targetId, comparatorId, outcomeId, targetEffectSize), 
    by = join_by(targetId, comparatorId, outcomeId)
  )
# 
# 
# originEstimates %>%
#   bind_rows(
#     originEstimates %>%
#       mutate(analysisId = analysisId + 100)
#   ) %>%
#   bind_rows(
#     originEstimates %>%
#       mutate(analysisId = analysisId + 200)
#   ) %>%
#   select(analysisId, targetId, comparatorId, outcomeId, startLogRr = logRr, startSeLogRr = seLogRr)  %>%
#   inner_join(estimates, by = join_by(analysisId, targetId, comparatorId, outcomeId), multiple = "all") %>%
#   inner_join(
#     allControls %>%
#       select(targetId, comparatorId, outcomeId, targetEffectSize), 
#     by = join_by(targetId, comparatorId, outcomeId))

plotShift <- function(fromAnalysisIds, 
                      toAnalysisIds = NULL, 
                      from20K = TRUE, 
                      labels = c("PS 1-on-1 matching", "PS stratification"),
                      title,
                      fileName) {
  mapping <- tibble(
    analysisId = fromAnalysisIds,
    toAnalysisId = toAnalysisIds,
    label = labels
  )
  if (from20K) {
    vizData <- estimates %>%
      filter(sampleSize == 20000 & targetEffectSize == 1 & analysisId %in% fromAnalysisIds) %>%
      select(analysisId, targetId, comparatorId, outcomeId, startLogRr = logRr, startSeLogRr = seLogRr) %>%
      inner_join(
        estimates %>%
          filter(sampleSize != 20000 & targetEffectSize == 1 & analysisId %in% toAnalysisIds) %>%
          rename(toAnalysisId = analysisId) %>%
          inner_join(
            mapping,
            by = join_by(toAnalysisId)
          ),
        by = join_by(analysisId, targetId, comparatorId, outcomeId),
        multiple = "all"
      ) 
  } else {

    vizData <- estimates %>%
      filter(sampleSize != 20000 & targetEffectSize == 1 & analysisId %in% fromAnalysisIds) %>%
      select(analysisId, targetId, comparatorId, outcomeId, sampleSize, startLogRr = logRr, startSeLogRr = seLogRr) %>%
      inner_join(
        estimates %>%
          filter(sampleSize != 20000 & targetEffectSize == 1 & analysisId %in% toAnalysisIds) %>%
          rename(toAnalysisId = analysisId) %>%
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
  from20K = TRUE,
  title = "From single large study to pooled across smaller sites",
  fileName = "ShiftVsPooled.png"
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


