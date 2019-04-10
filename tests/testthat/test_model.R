data(iris)

tdf_c <- prepare_training_set(iris,"Species")
tdf_r <- prepare_training_set(iris,"Sepal.Length")
hyp_c <- guess_hyperparameters(tdf_c)
hyp_r <- guess_hyperparameters(tdf_r)
tmo_c <- train_model(tdf_c, hyp_c)
tmo_r <- train_model(tdf_r, hyp_r)
test_that("the hyperparameters array makes sense", {
  expect_equal(class(hyp_c), "list")
  expect_equal(hyp_c[["nrounds"]], 178)
  expect_equal(hyp_r[["objective_function"]], "reg:linear")
})

test_that("the cv function works as intended", {
  expect_equal(cross_validate(train_structure = tdf_r,
                              hyperparameters = hyp_r)$cv_model$evaluation_log$test_mae_mean[60],
               0.28, tolerance=5e-2)
  expect_match(cross_validate(train_structure = tdf_c,
                              hyperparameters = hyp_c)$cv_model$params$eval_metric, "mlogloss")
})

test_that("the predictions from the model are alright", {
  expect_equal(get_predictions(tmo_r, iris[1:1,])[["Sepal.Length"]][1], 5.16, tolerance = 0.1)
  # expect_match(get_predictions(train_model(tdf_c, hyp_c), iris[1:1,])[["category"]][1], "setosa")
})
