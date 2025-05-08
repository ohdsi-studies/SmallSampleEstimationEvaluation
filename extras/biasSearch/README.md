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
