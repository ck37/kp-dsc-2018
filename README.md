# KP Data Science Competition 2018

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
  * You also may want to run Session -> Restart R if you get any weird messages about corrupt files.


