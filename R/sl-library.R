# Improved version based on t-statistic rather than p-value
screen.corRank2 <-
  function(Y, X, family, method = 'pearson',
           rank = 2,
           # Always include covariates that match the inclusion regular expression.
           inclusion_regex = "^tx_",
           ...) {
    # if(rank > ncol(X)) {
    #     rank <- ncol(X)
    # }
    # Don't really need that check, but might want to add a warning message
    listp <- apply(X, 2, function(x, Y, method) {
      # Sort based on absolute value of the t-statistic, not the p-value
      # This will result in fewer ties.
      ifelse(var(x) == 0, 0, abs(unname(cor.test(x, y = Y, method = method)$statistic)))
    }, Y = Y, method = method)
    # Take the negative of listp to rank in descending order of t-statistic.
    # Also be sure to include any variable that match the inclusion_regex.
    whichVariable <- (rank(-listp) <= rank | grepl(inclusion_regex, colnames(X), perl = TRUE))
    return(whichVariable)
  }
