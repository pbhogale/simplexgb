#' This function guesses sensible parameters for setting up the
#' xgb model. If the user has supplied parameters, the function
#' chooses the ones least likely to lead to overfitting.
#'
#' Objective functions and eval metrics are always taken
#' to be the user supplied values, if the suer has supplied them,
#' else they are guessed based on the type of the target variable.
#'
#' @param train_structure is the data structure produced by prepare_training_set
#' @param n_estimators minimum number of estimators
#' @param learning_rate maximum learning rate
#' @param depth minimum depth of trees
#' @param nrounds the number of rounds of training
#' @param objective_function
#' @param eval_metric
guess_hyperparameters <- function(train_structure,
                                  n_estimators = 1000,
                                  learning_rate = 0.1,
                                  depth = 2,
                                  nrounds = 20,
                                  objective_function = NA,
                                  eval_metric = NA){
  hyperparameters <- list()
  hyperparameters[["depth"]] <- max(depth, floor(sqrt(ncol(train_structure$data))))
  hyperparameters[["n_estimators"]] <- max(n_estimators, exp(floor(log(nrow(train_structure$data)))-2))
  hyperparameters[["learning_rate"]] <- min(learning_rate, 1/(log(hyperparameters[["n_estimators"]]*hyperparameters[["depth"]])))
  class_target <- ("target_reference" %in% names(train_structure))
  if(class_target){
    hyperparameters[["objective_function"]] <- "multi:softprob"
    hyperparameters[["eval_metric"]] <- "mlogloss"
    hyperparameters[["num_class"]] <- train_structure$data[[train_structure$target_variable]] %>% dplyr::n_distinct()
  } else {
    hyperparameters[["objective_function"]] <- "reg:linear"
    hyperparameters[["eval_metric"]] <- "mae"
  }
  if(!is.na(objective_function)){
    hyperparameters[["objective_function"]] <- objective_function
  }
  if(!is.na(eval_metric)){
    hyperparameters[["eval_metric"]] <- eval_metric
  }
  hyperparameters[["nrounds"]] <- max(nrounds, floor(hyperparameters[["depth"]]*sqrt(hyperparameters[["n_estimators"]])))
  return(hyperparameters)
}

cross_validate <- function(train_structure, hyperparameters, nfold = 5){
  xgb_params <- list("objective" = hyperparameters[["objective_function"]],
                     "eval_metric" = hyperparameters[["eval_metric"]],
                     "eta" = hyperparameters[["learning_rate"]],
                     "max_depth" = hyperparameters[["depth"]],
                     "n_estimators" = hyperparameters[["n_estimators"]])
  if("num_class" %in% names(hyperparameters)){
    xgb_params[["num_class"]] <- hyperparameters[["num_class"]]
  }
  features <-Matrix::sparse.model.matrix(as.formula(paste(train_structure$target_variable, "~ .")),
                                  data = train_structure$data)[,-1]
  lab <- train_structure$data[[train_structure$target_variable]]
  dtrain <- xgboost::xgb.DMatrix(data = features, label = lab)
  cv_model <- xgboost::xgb.cv(params = xgb_params,
                              data = dtrain,
                              verbose = F,
                              nfold = nfold,
                              nrounds = hyperparameters[["nrounds"]],
                              prediction = T)
  if("num_class" %in% names(hyperparameters)){
    OOF_prediction <- tibble::tibble(cv_model$pred) %>%
      dplyr::mutate(max_prob = max.col(., ties.method = "last")) %>%
      dplyr::mutate(label = lab+1)
    cm <- caret::confusionMatrix(factor(OOF_prediction$max_prob),
                          factor(OOF_prediction$label),
                          mode = "everything")
    print(cm)
  }
  return(cv_model)
}
