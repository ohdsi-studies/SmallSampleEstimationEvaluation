# Compare the propensity scores from the global and the local propensity model
library(dplyr)
library(ggplot2)

options(andromedaTempFolder = "d:/andromedaTemp")

rootFolderName <- "d:/SmallSampleEstimationEvaluation_"
databases <- c("MDCD", "MDCR", "Optum_EHR")
sampleSizes <- c(4000, 2000, 1000, 500, 250)


tcs <- read.csv(file.path(paste0(rootFolderName, databases[1]), "allControls.csv")) |>
  distinct(targetId, targetName, comparatorId, comparatorName) |>
  mutate(comparison = sprintf("%s vs %s", targetName, comparatorName))

for (database in databases) {
  dbFolder <- paste0(rootFolderName, database)
  cmFolder <- file.path(dbFolder, "largeSample")
  ref <- CohortMethod::getFileReference(cmFolder) |>
    filter(analysisId == 4) |>
    distinct(targetId, comparatorId, cohortMethodDataFile, sharedPsFile)
  samplePs <- list()
  for (i in seq_len(nrow(ref))) {
    cmData <- CohortMethod::loadCohortMethodData(file.path(cmFolder, ref$cohortMethodDataFile[i]))
    rowIds <- cmData$cohorts |>
      pull("rowId")
    globalPs <- readRDS(file.path(cmFolder, ref$sharedPsFile[i])) |>
      filter(rowId %in% rowIds) |>
      select("rowId", "treatment", "propensityScore") |>
      mutate(targetId = ref$targetId[i],
             comparatorId = ref$comparatorId[i])
    # Recompute preference score so it uses the treatment ratio in the sample:
    globalPs <- CohortMethod:::computePreferenceScore(globalPs)
    globalPs <- globalPs |>
      rename(globalPs = "propensityScore", globalPref = "preferenceScore")
    for (sampleSize in sampleSizes) {
      sampleRootFolder <- file.path(dbFolder, sprintf("smallSample%d", sampleSize))
      localPs <- list()
      for (sampleFolder in list.dirs(sampleRootFolder, full.names = TRUE, recursive = FALSE)) {
        sharedPsFile <- CohortMethod::getFileReference(sampleFolder) |>
          filter(analysisId == 1 & targetId == ref$targetId[i] & comparatorId == ref$comparatorId[i]) |>
          head(1) |>
          pull(sharedPsFile)
        localPs[[length(localPs) + 1]] <- readRDS(file.path(sampleFolder, sharedPsFile)) |>
          select("rowId", "propensityScore")
      }
      localPs <- bind_rows(localPs)
      localPs <- CohortMethod:::computePreferenceScore(localPs, globalPs)
      localPs <- localPs |>
        rename(localPs = "propensityScore", localPref = "preferenceScore")
      samplePs[[length(samplePs) + 1]] <- globalPs |>
        mutate(sampleSize = !!sampleSize) |>
        left_join(localPs, by = join_by("rowId"))
    }
  }
  samplePs <- bind_rows(samplePs) |>
    inner_join(tcs, by = join_by(targetId, comparatorId))
  samplePs <- samplePs |>
    mutate(treatmentLabel = if_else(.data$treatment == 1, "Target", "Comparator"),
           sampleSizeLabel = sprintf("Sample size = %s", .data$sampleSize)) 
  samplePs$treatmentLabel <- factor(samplePs$treatmentLabel, levels = c("Target", "Comparator"))
  samplePs$sampleSizeLabel <- factor(samplePs$sampleSizeLabel, levels = sprintf("Sample size = %s", sampleSizes))
  plot <- ggplot(samplePs, aes(x = globalPref, y = localPref, color = treatmentLabel)) +
    geom_abline(slope = 1) +
    geom_point(shape = 16, alpha = 0.2, size = 0.3) +
    scale_color_manual(values = c(rgb(0.8, 0, 0), rgb(0, 0, 0.8))) +
    scale_x_continuous("Global preference score") +
    scale_y_continuous("Local preference score") +
    guides(color = guide_legend(override.aes = list(size = 3))) + 
    facet_grid(comparison~sampleSizeLabel) +
    ggtitle(gsub("_", "", database)) +
    theme(legend.title = element_blank(),
          legend.position = "top",
          plot.title = element_text(hjust = 0.5))
  ggsave(file.path(paste0(rootFolderName, databases[1]), "plotsAndTables", sprintf("localVsGlobalPs_%s.png", database)), plot, width = 14, heigh = 9, dpi = 200)
}
