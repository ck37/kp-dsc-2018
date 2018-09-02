#' Load all packages needed, installing any missing packages if desired.
#'
#' @param auto_install Install any packages that could not be loaded.
#' @param update Update any packages that can be updated.
#' @param java_mem Amount of RAM to allocate to rJava; must happen before
#'   library is loaded. If it happens afterwards it will not change available memory.
#' @param verbose If TRUE display extra output during execution.
load_all_packages =
  function(packages_cran = NULL,
           packages_github = NULL,
           auto_install = FALSE,
           update = FALSE,
           java_mem = "2g",
           verbose = FALSE) {

    if (verbose) {
      # Output R version so we know which package versions we're using.
      cat(R.version.string, "\n")
    }

    if (verbose) {
      cat("CRAN packages:", paste(packages_cran, collapse = ", "), "\n")
      cat("Github packages:", paste(packages_github, collapse = ", "), "\n")
    }

    # Allocate 4GB to Java for bartMachine; needs to happen before we load rJava library.
    # TODO: may want to defer this step (and loading rJava) until we have setup parallelization.
    if (!is.null(java_mem)) {
      options(java.parameters = paste0("-Xmx", java_mem))
    }

    # Code is not yet run. We run afterward, possibly with messages suppressed.
    expression = quote({

      # Install devtools if we don't already have it.
      if (!require("devtools") && auto_install) {
        if (verbose) cat("No devtools detected - installing.\n")
        install.packages("devtools")
        library(devtools)
      }

      # Install any packages from github that are needed.
      if (length(packages_github) > 0) {
        if (auto_install) {
          devtools::install_github(packages_github)
        }

        if (is.null(names(packages_github))) {
          # Extract package name as the repo name of each package, if the vector itself is not named.
          # This assumes github packages are of the form: username/repo@whatev (@whatev optionally included).
          github_names = gsub("^[^/]+/([^@]+).*$", "\\1", packages_github)
        } else {
          # Support a named vector, in case the package name is not the same as the repo name.
          github_names = names(packages_github)
        }
        invisible(sapply(github_names, require, character.only = T))
      }

      # To facilitate installation on KP systems.
      httr::set_config(httr::config(ssl_verifypeer = 0L))

      # This part clearly requires that the ck37r package was already installed,
      # either via packages_cran or packages_github.
      ck37r::load_packages(packages_cran, auto_install, update, verbose = verbose)
    })

    # Now run the stored code either directly or with messages suppressed.
    if (verbose) {
      # Allow messages to be output.
      eval(expression)
    } else {
      # Suppress messages.
      suppressMessages(eval(expression))
    }
  }


#' @param packages_cran Character vector of CRAN packages to load, and possibly install if needed.
#' @param packages_github Character vector of github packages to load, and possibly install if needed.
startup = function(packages_cran = NULL,
                   packages_github = NULL,
                   ...) {
  if (is.null(packages_cran)) {
    # Extract defaults based on an attribute so that it can be examined/modified if needed.
    packages_cran = attr(startup, "packages_cran")
  }

  if (is.null(packages_github)) {
    # Extract defaults based on an attribute so that it can be examined/modified if needed.
    packages_github = attr(startup, "packages_github")
  }

  load_all_packages(packages_cran = packages_cran,
                    packages_github = packages_github, ...)
}

# Specify defaults as attributes so that they can be extracted programmatically.
# These packages could be installed manually by running:
# install.packages(attr(startup, "packages_cran"))
attr(startup, "packages_cran") = c(
  "devtools",
  "data.table",
  "magrittr", # For %$%
  "rio",

  # Output / reporting
  "knitr",
  "rmarkdown",

  # Parallelization.
  "doParallel",
  "RhpcBLASctl",
  "future",

  # Visualization
  "ggplot2",

  # Machine learning.
  "glmnet",
  "ranger",
  "xgboost"
)
# These can be installed manually using devtools::install_github()
# Or all at once:
# devtools::install_github(attr(startup, "packages_github"))
attr(startup, "packages_github") = c(
  # This is typically more up to date than the CRAN version.
  "ecpolley/SuperLearner",
  # For exploring our random forests.
  "PhilippPro/OOBCurve",
  # CRAN version is really out of date.
  "ck37/ck37r"
)
