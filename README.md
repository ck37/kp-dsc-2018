# Kaiser Permanente Data Science Competition 2018

**Team**: Decision Tree huggers

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
* Run or knit one of the model files, such as `model-sl-basic.Rmd` to build a model and generate a submission export.
