process_metrics <- function(baseFolder, databaseId) {
  
  sampleSizes <- c(4000, 2000, 1000, 500, 250)
  
  # Define file paths
  plotsAndTablesFolder <- file.path(baseFolder, "plotsAndTables")
  # dir.create(plotsAndTablesFolder, recursive = TRUE)  # Uncomment if folder creation needed
  
  tcs <- read.csv(file.path(baseFolder, "allControls.csv")) |>
    distinct(targetId, targetName, comparatorId, comparatorName) |>
    mutate(comparison = sprintf("%s vs %s", targetName, comparatorName))
  
  metrics <- tibble()
  psMetrics <- tibble()
  
  for (sampleSize in sampleSizes) {
    metrics <- metrics |>
      bind_rows(
        readr::read_csv(
          file = file.path(baseFolder, sprintf("Metrics_sample_%d.csv", sampleSize)),
          show_col_types = FALSE
        ) |>
          mutate(sampleSize = !!sampleSize)
      )
    psMetrics <- psMetrics |>
      bind_rows(
        readr::read_csv(
          file = file.path(baseFolder, sprintf("PsMetrics_sample_%d.csv", sampleSize)),
          show_col_types = FALSE
        ) |>
          mutate(sampleSize = !!sampleSize)
      )
  }
  
  metricsLargeSample <- readr::read_csv(
    file = file.path(baseFolder, "Metrics_LargeSample.csv"),
    show_col_types = FALSE
  ) |>
    mutate(sampleSize = 20000)
  
  x <- tibble(
    sampleSize = c(20000, sampleSizes),
    x = seq_len(length(sampleSizes) + 1)
  )
  
  largeSampleFolder <- file.path(baseFolder, "largeSample")
  ref <- CohortMethod::getFileReference(largeSampleFolder) |>
    distinct(analysisId, targetId, comparatorId, sharedBalanceFile) |>
    filter(sharedBalanceFile != "")
  
  getMaxSdm <- function(row) {
    balance <- readRDS(file.path(largeSampleFolder, row$sharedBalanceFile))
    maxSdm <- max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)
    balanceP <- CohortMethod:::computeBalanceP(balance$afterMatchingStdDiff, balance$afterMatchingSdmVariance, 0.1)
    alpha <- 0.05
    balanced <- balanceP > (alpha / nrow(balance))
    significantUnbalanced <- sum(!balanced, na.rm = TRUE)
    row <- tibble(
      analysisId = row$analysisId,
      targetId = row$targetId,
      comparatorId = row$comparatorId,
      maxSdm = maxSdm,
      significantUnbalanced = significantUnbalanced)
    return(row)
  }
  
  balLargeSample <- lapply(split(ref, seq_len(nrow(ref))), getMaxSdm) |>
    bind_rows() |>
    mutate(sampleSize = 20000)
  
  easeP <- readr::read_csv(file.path(baseFolder, "easeP.csv"), show_col_types = FALSE)
  
  combined <- metrics |>
    filter(analysisId %in% c(1, 2, 3, 4, 5)) |>
    mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", ifelse(analysisId %in% c(3, 5), "PS stratification", "No PS adjustment"))) |>
    mutate(psModel = ifelse(analysisId %in% c(1, 3), "Local", ifelse(analysisId %in% c(4, 5), "Global", "None"))) |>
    inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
    inner_join(x, by = join_by(sampleSize)) |>
    mutate(x = ifelse(psModel == "Local", x + 0.1, x - 0.1)) |>
    left_join(easeP, by = join_by(analysisId, targetId, comparatorId, sampleSize)) |>
    mutate(significant = ifelse(!is.na(p) & p < 0.05, "Significant", "Non-sign."),
           database = databaseId)
  
  saveRDS(combined, file.path(baseFolder, "CombinedMetrics.rds"))
  
  combinedPs <- psMetrics |>
    bind_rows(
      balLargeSample |>
        filter(analysisId %in% c(1, 3, 4, 5)) |>
        mutate(maxSdmMedian = maxSdm,
               significantUnbalancedMedian = significantUnbalanced)
    ) |>
    mutate(psMethod = ifelse(analysisId %in% c(1, 4), "PS 1-on-1 matching", ifelse(analysisId %in% c(3, 5), "PS stratification", "No PS adjustment"))) |>
    inner_join(tcs, by = join_by("targetId", "comparatorId")) |>
    inner_join(x, by = join_by(sampleSize)) |>
    mutate(database = databaseId)
  
  saveRDS(combinedPs, file.path(baseFolder, "CombinedPSMetrics.rds"))
}

