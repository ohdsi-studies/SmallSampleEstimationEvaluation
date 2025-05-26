Search for target-comparators with bias
=======================================

These scripts evaluate a large set of target-comparators to find examples where 
bias (as measured through negative control outcomes) is large when not adjusting
for any confounding. We need these examples to evaluate confounder adjustment 
techniques.

The exposure (drug) concepts as well as the negative controls outcomes are taken
from the LEGEND Depression, Hypertension, and Type-2 Diabetes Mellitus studies.

# How to run

- Set the connection details, database schemas, and local folders in `SetConnectionDetails.R`
- Run `CreateCohorts.R` to create the cohorts.
- Run `RunCohortMethod.R` to comopute the hazar ratio estimates for all target-comparator-outcome combinations.
- Run `ComputeBias.R` to compute the bias metrics per target-comparator.

The final results of these steps will be written to CSV files in this folder called:

- `bias_Depression.csv`
- `bias_Hypertension.csv`
- `bias_Type2Diabetes.csv`

Additionally, plots showing the negative controls for each target-comparator will be written to the `ncPLots` subfolder.

These files have already been generated and included in this repo.
