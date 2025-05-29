library(dplyr)

# Pick one:
outputFolder <- "e:/SmallSampleEstimationEvaluation_ccae"
outputFolder <- "e:/SmallSampleEstimationEvaluation_optum_ehr"


sampleSizes <- c(4000)
# sampleSizes <- c(4000, 2000, 1000, 500, 250)

csvFileName <- system.file("NegativeControls.csv", package = "SmallSampleEstimationEvaluation")
negativeControls <- readr::read_csv(csvFileName, show_col_types = FALSE)  |>
  select("targetId", "comparatorId", outcomeId = "outcomeConceptId") |>
  mutate(type = "Outcome control")
tcas <- negativeControls |>
  distinct(targetId, comparatorId) |>
  cross_join(tibble(analysisId = c(1, 3)))

# row = tcas[2, ]
compareEaseOneAnalysis <- function(row, estimates) {
  estimates1 <- estimates |>
    inner_join(row, by = join_by(analysisId, targetId, comparatorId)) |>
    arrange(outcomeId)
  rowBaseline <- row |>
    mutate(analysisId = if(analysisId == 1) 4 else 5)
  estimates2 <- estimates |>
    inner_join(rowBaseline, by = join_by(analysisId, targetId, comparatorId)) |>
    arrange(outcomeId)
  result <- EmpiricalCalibration::compareEase(
    logRr1 = estimates1$logRr,
    seLogRr1 = estimates1$seLogRr,
    logRr2 = estimates2$logRr,
    seLogRr2 = estimates2$seLogRr
  )
  row <- row |>
    mutate(p = result$p)
  return(row)
}

compareEase <- function(sampleSize) {
  message(sprintf("Comparing EASE for sample size = %d", sampleSize))
  cmFolder <- file.path(outputFolder, sprintf("smallSample%d", sampleSize))
  estimates <- CohortMethod::getResultsSummary(cmFolder) |>
    filter(outcomeId %in% negativeControls$outcomeId) 
  easeP <- lapply(split(tcas, seq_len(nrow(tcas))), compareEaseOneAnalysis, estimates = estimates)
  easeP <- bind_rows(easeP) |> 
    mutate(sampleSize = !!sampleSize)
  return(easeP)
}

easeP <- lapply(sampleSizes, compareEase)
easeP <- easeP |>
  bind_rows()
readr::write_csv(easeP, file.path(outputFolder, "easeP.csv"))
