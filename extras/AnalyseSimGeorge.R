library(dplyr)
library(ggplot2)
library(readr)
data <- read_csv("c:/temp/SmallSampleCovariateBalanceSingleStudy.csv") %>%
  select(-1)

data <- data %>%
  filter(treatmentOnOutcome == 0)

ggplot(data, aes(x = NsampleSize, y = proportionValid)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous("Sample size (number of subjects)") +
  scale_y_continuous("Proportion declared 'balanced'") +
  facet_grid(confounderOnTreatment~ .)
