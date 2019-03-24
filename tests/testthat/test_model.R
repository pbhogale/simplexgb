data(iris)


test_that("the hyperparameters array makes sense", {
  expect_equal(class(guess_hyperparameters(prepare_training_set(iris, "Species"))), "list")
  expect_equal(guess_hyperparameters(prepare_training_set(iris, "Species"))[["nrounds"]], 63)
  expect_equal(guess_hyperparameters(prepare_training_set(iris, "Sepal.Length"))[["objective_function"]], "reg:linear")
})

test_that("the cv function works as intended", {
  expect_equal(cross_validate(train_structure = prepare_training_set(iris, "Sepal.Length"),
                              hyperparameters = guess_hyperparameters(prepare_training_set(iris, "Sepal.Length")))$cv_model$evaluation_log$test_mae_mean[60],
               0.27, tolerance=1e-1)
  expect_match(cross_validate(train_structure = prepare_training_set(iris, "Species"),
                              hyperparameters = guess_hyperparameters(prepare_training_set(iris, "Species")))$cv_model$params$eval_metric, "mlogloss")
  })
