data(iris)


test_that("the hyperparameters array makes sense", {
  expect_equal(class(guess_hyperparameters(prepare_training_set(iris, "Species"))), "list")
  expect_equal(length(guess_hyperparameters(prepare_training_set(iris, "Species"))), 7)
  expect_equal(guess_hyperparameters(prepare_training_set(iris, "Species"))[["nrounds"]], 63)
  expect_equal(guess_hyperparameters(prepare_training_set(iris, "Sepal.Length"))[["objective_function"]], "reg:squarederror")
})

