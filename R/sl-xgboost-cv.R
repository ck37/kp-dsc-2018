SL.xgboost_cv2 =
  function(Y, X, newX, family, obsWeights, id,
           ntrees = 5000L,
           early_stopping_rounds = 200L,
           nfold = 5L,
           max_depth = 4L,
           shrinkage = 0.1,
           minobspernode = 50L,
           subsample = 0.7,
           colsample_bytree = 0.8,
           gamma = 5,
           # Due to class imbalance,
           stratified = family$family == "binomial",
           # Focus on AUC and see if we do better.
           eval_metric = ifelse(family$family == "binomial", "auc", "rmse"),
           print_every_n = 400L,
           nthread = getOption("sl.cores", 1L),
           verbose = 0,
           save_period = NULL, ...) {

    #.SL.require("xgboost")
    if (utils::packageVersion("xgboost") < 0.6)
      stop("SL.xgboost requires xgboost version >= 0.6, try help('SL.xgboost') for details")

    if (!is.matrix(X)) {
      # Convert to a matrix, then remove the added intercept column.
      X = stats::model.matrix(~ ., X)[, -1]
    }

    xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)

    if (family$family == "gaussian") {
      objective = "reg:linear"
    } else if (family$family == "binomial") {
      objective = "binary:logistic"
    }

    model_cv =
      xgboost::xgb.cv(data = xgmat,#
                      objective = objective, #
                      nrounds = ntrees,#
                      nfold = nfold,#
                      metrics = list(eval_metric), #
                      stratified = stratified,#
                      verbose = verbose,#
                      #print_every_n = print_every_n,#
#                      early_stopping_rounds = early_stopping_rounds, #
                      max_depth = max_depth,
                      min_child_weight = minobspernode,
                      eta = shrinkage,
                      nthread = nthread,
                      subsample = subsample,
                      colsample_bytree = colsample_bytree,
                      gamma = gamma)#,
                      #save_period = save_period)

    if (verbose) {
      print(model_cv)
    }

    # Refit that model using the best params.
    model =
      xgboost::xgboost(data = xgmat,
                       objective = objective,
                       nrounds = model_cv$best_ntreelimit,
                       params = model_cv$params,
                       verbose = 0L,
                       print_every_n = 500L,
                       save_period = save_period)

    if (family$family == "multinomial") {
      # Not used yet.
      model = xgboost::xgboost(data = xgmat, objective = "multi:softmax",
                               nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode,
                               eta = shrinkage, verbose = verbose, num_class = length(unique(Y)),
                               nthread = nthread, save_period = save_period)
    }

    if (!is.matrix(newX)) {
      newX = stats::model.matrix(~ ., newX)[, -1]
    }
    pred = xgboost::predict(model, newdata = newX)
    #pred = xgboost:::predict.xgb.Booster(model, newdata = newX)
    fit = list(object = model)
    class(fit) = c("SL.xgboost")
    out = list(pred = pred, fit = fit)
    return(out)
  }
