# Compute bias metrics per target-comparator. 
# Assumes RunCohortMethod.R has already been executed

source("extras/biasSearch/SetConnectionDetails.R")
library(readr)
library(dplyr)
library(CohortMethod)

exposures <- read_csv("extras/biasSearch/Exposures.csv", show_col_types = FALSE)

ncPlotFolder <- "extras/biasSearch/ncPlots"
if (!dir.exists(ncPlotFolder)) {
  dir.create(ncPlotFolder)
}

indications <- exposures |>
  distinct(indicationId)  |>
  pull()

for (i in seq_along(indications)) {
  indication <- indications[i]
  message("Computing bias for indication: ", indication)
  
  indicationFolder <- file.path(outputFolder, indication)

  estimates <- getResultsSummary(indicationFolder) 
  
  groups <- estimates |>
    inner_join(
      exposures |>
        select(targetId = "conceptId", targetName = "name"),
      by = join_by("targetId")
    ) |> inner_join(
      exposures |>
        select(comparatorId = "conceptId", comparatorName = "name"),
      by = join_by("comparatorId")
    ) |>
    group_by(targetId, targetName, comparatorId, comparatorName) |>
    group_split()
  
  # group = groups[[1]]
  computeMetrics <- function(group, ncPlotFolder) {
    null <- EmpiricalCalibration::fitMcmcNull(group$logRr, group$seLogRr)
    fileName <- file.path(
      ncPlotFolder, 
      sprintf("ncs_t%d_c%d.pdf", group$targetId[1], group$comparatorId[1])
    )
    EmpiricalCalibration::plotCalibrationEffect(
      logRrNegatives = group$logRr, 
      seLogRrNegatives = group$seLogRr, 
      null = null, 
      showCis = TRUE,
      showExpectedAbsoluteSystematicError = TRUE,
      fileName = fileName
    )
    easeWithCi <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
    row <- group |>
      select("targetId", "targetName", "comparatorId", "comparatorName") |>
      head(1) |>
      mutate(
        targetSubjects = max(group$targetSubjects),
        comparatorSubjects = max(group$comparatorSubjects),
        mu = null[1],
        sigma = sqrt(1 / null[2]),
        ease = easeWithCi$ease,
        easeLb = easeWithCi$ciLb,
        easeUb = easeWithCi$ciUb
      )
    return(row)
  }
  cluster <- ParallelLogger::makeCluster(8)
  ParallelLogger::clusterRequire(cluster, "dplyr")
  rows <- ParallelLogger::clusterApply(
    cluster = cluster, 
    x = groups, 
    fun = computeMetrics,
    ncPlotFolder = ncPlotFolder
  )
  ParallelLogger::stopCluster(cluster)
  
  rows <- bind_rows(rows)
  write_csv(rows, sprintf("extras/biasSearch/bias_%s.csv", indication))
}

# Generate some nice plots showing distributions ---------------------------------------------------
library(ggplot2)

ease <- list()
for (i in seq_along(indications)) {
  indication <- indications[i]
  ease[[i]] <- read_csv(sprintf("extras/biasSearch/bias_%s.csv", indication), show_col_types = FALSE) |>
    mutate(indication = indication)
}
ease <- bind_rows(ease)

ggplot(ease, aes(x = indication, y = ease)) +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_boxplot(fill = "#69AED5", alpha = 0.8) +
  scale_y_continuous("Expected Absolute Systematic Error (EASE)") +
  theme(axis.title.x = element_blank())
ggsave("extras/biasSearch/bias_boxplot.png", width = 4.5, height = 4.5, dpi = 300)
