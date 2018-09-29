# Kaiser Permanente Data Science Competition 2018

**Team**: Decision Tree Huggers (4th out of 30 teams)

**Members**: Chris Kennedy, [Nerissa Nance](https://github.com/nerissanance), [Makdine Dontsi](https://github.com/makdine), [Noel Pimentel](https://github.com/noelpimentel)

## Setup

* train.csv and test.csv should be placed into `data-raw/`
* Required R packages will need to be installed.
  * Run:
    ```r
    source("R/_startup.R")
    # Install the needed packages from CRAN.
    install.packages(attr(startup, "packages_cran"))
    # Install a few extra packages from Github.
    devtools::install_github(attr(startup, "packages_github"))
    ```
  * If you run into errors, you may need to install a few packages manually, e.g. h2o.
  * You also may want to run Session -> Restart R if you get any weird messages about "lazy-load database something.rdb is corrupt".

## Analyze

* Run or knit `clean.Rmd` to import the raw data and generate `data/clean.RData`.
* Run or knit `eda.Rmd` to conduct exploratory data analysis.
* Run or knit one of the model files, such as `model-sl-slower.Rmd` to build a model and generate a submission export.

## Methodology

The following summarizes the Decision Tree Huggers' work on the Higgs Boson challenge.

### Cleaning

* Categorical variables: converted pri_jet_num to a factor and then to a series of indicators.
* Missing values: added missingness indicators for missing values and imputed missing values to the median/mode. Tried random forest missing value imputation but did not find a benefit from leaderboard submission.
* We explored removing highly correlated variables (pri_jet_leading_pt, pri_jet_all_pt),  adding log transformations of skewed variables, and trimming outliers but ran out of time to submit based on that additional cleaning.

### Exploration 
* We examined univariate correlation with the outcome, OLS coefficients, and correlation heatmaps to review covariate relationships.
* We used boxplots and outlier plots to review extreme values.
* We ran out of time to complete tSNE and UMAP analyses.

### Modeling 

* Our model consisted of a SuperLearner ensemble of XGBoost models and a random forest (Ranger, 200 trees) estimated with 20-fold cross-validation. This included a grid of 12 different XGBoost configurations: {300, 1000, or 2000 trees} x {4 or 8 maximum depth} x {0.01 or 0.1 shrinkage}. Logistic regression and lasso regression were also available but were not selected in the ensemble.
* The SuperLearner ensemble was optimized to mean-squared error but with more time we might have tried AUC or negative log likelihood optimization.
* We tried h2o.ai's auto_ml() feature targeted to AUC optimization with 10-fold cross-validation but did not find increased performance compared to our SuperLearner ensemble.
* We tried feature selection based on univariate correlation (top 5, 15, 25, or 30 covariates) but found the best performance when all variables were included.

### Submission 

* We analyzed different threshold options and found that using the mean of the outcome for the training data (0.3427) was the best.

