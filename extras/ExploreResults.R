library(dplyr)
library(readr)
library(ggplot2)
library(CohortMethod)
library(ggpattern)

ExploreResults <- function(baseFolder, databaseId) {
  source("extras/compareEase.R")  
  source("extras/SymposiumPlots.R")  
  source("extras/SummarizePsResults.R")  
  
  compare_ease_all(baseFolder)
  process_metrics(baseFolder, databaseId) 
  generate_all_plots(baseFolder)
}

ExploreResults(baseFolder = "e:/SmallSampleEstimationEvaluation_ccae", 
               databaseId = "CCAE")
ExploreResults(baseFolder = "e:/SmallSampleEstimationEvaluation_optum_ehr",
               databaseId = "Optum_EHR")