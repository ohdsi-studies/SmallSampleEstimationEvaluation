Small-Sample Comparative-Effect Estimation Evaluation
=====================================================

<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

 Analytics use case(s): **Population-Level Estimation**
- Study type: **Methods Research**
- Tags: **-**
- Study lead: **Martijn Schuemie**
- Study lead forums tag: **[schuemie](https://forums.ohdsi.org/u/schuemie)**
- Study start date: **December 3, 2020**
- Study end date: **-**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**

Evaluation of the performance of comparative effect estimation methods when sample size (number of subjects in the target and comparator) is small.

# How to run

1. Clone this repo
2. Open the Rstudio project
3. Run `renv::restore()` (select 'Y' to activate)
4. Install the `SmallSampleEstimationEvaluation` package (In RStudio, go to 'Build' then 'Install').
5. Open the extras/CodeToRun.R, edit as needed, and run.

After this, all CohortMethod analyses should have completed. Code for analyzing the results, generating plots, can be found in the `extras` folder