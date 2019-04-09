data(iris)
test_normalize_df <- function(){
  test_df <- normalize_df(iris, facs_df = get_normalizing_factors(iris))
  return(mean(test_df$Sepal.Length))
}

test_that("normalizing gives means close to 0", {
  expect_equal(test_normalize_df(), 0.0)
})

test_normalizing_factors <- function(){
  test_facs <- get_normalizing_factors(iris)
  test_df <- normalize_df(iris, facs_df = test_facs)
  colns <- colnames(test_facs)
  for(i in 1:length(colns)){
    test_df[[colns[i]]] <- test_df[[colns[i]]] * test_facs[[colns[i]]][2]
    test_df[[colns[i]]] <- test_df[[colns[i]]] + test_facs[[colns[i]]][1]
  }
  return(test_df)
}

test_const <- iris$Sepal.Length[1]
test_that("normalizing factors can be used to recreate original df", {
  expect_equal(test_normalizing_factors()[["Sepal.Length"]][1], test_const)
  expect_equal(colnames(test_normalizing_factors()), colnames(iris))
})


iris_test <- iris
iris_test[["bad_column"]] <- rep(0, nrow(iris))

test_that("uninformative columns are removed",{
  expect_equal(colnames(remove_uninformative(iris_test)), colnames(iris))
})

test_that("the train levels returned are correct",{
  expect_equal(get_train_levels(iris)[["Species"]], unique(as.integer(iris$Species)))
})

test_that("making sure that the categorical target variable map is okay", {
  expect_match(as.character(transform_target_variable(iris, "Species")[["target_reference"]][["original_Species"]][1]), "setosa")
})

test_that("the prepared training data can be used",{
  expect_equal(prepare_training_set(iris, "Species")$data[["Sepal.Length"]][1],-0.89767388, tolerance=1e-3)
  expect_equal(prepare_training_set(iris, "Species")$normalize_by[["Sepal.Length"]][1],5.8433, tolerance=1e-3)
  expect_match(as.character(prepare_training_set(iris, "Species")$levels[["Species"]][1]),"1")
})
