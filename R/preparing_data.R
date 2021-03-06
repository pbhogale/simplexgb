#' this function helps compute the class of a given column
#' and returns only one character value as a result.
#' @param column of the data frame whose class we are interested in.
#' @export
get_class <- function(column){
  return_class <- class(column) %>%
    paste(collapse = '')
  return(return_class)
}

#' this function returns the factors by which a df was normalized,
#' so that the test dataset can be processed similarly
#' @inheritParams normalize_df
#' @return data frame with the normalizing factors
#' @export
get_normalizing_factors <- function(df, target_variable=NA){
  colns <- colnames(df)
  coln_class <- df %>%
    dplyr::summarise_all(get_class)
  selected_colns <- colns
  if(length(colns)>0){
    for(i in 1:length(colns)){
      if(coln_class[[colns[i]]][1]!="numeric"){
        selected_colns <- selected_colns[selected_colns!=colns[i]]
      }
    }
  }
  if(!is.na(target_variable)){
    selected_colns <- selected_colns[selected_colns!=target_variable]
  }
  facs <- list()
  if(length(selected_colns)>0){
    for(i in 1:length(selected_colns)){
      temp <- df[[selected_colns[i]]] - mean(df[[selected_colns[i]]], na.rm = T)
      facs[[selected_colns[i]]] <- c(mean(df[[selected_colns[i]]], na.rm = T), sd(temp, na.rm = T))
    }
    facs_df <- facs %>% dplyr::as_tibble() %>% tibble::rownames_to_column() %>% dplyr::select(-rowname)
  } else {
    facs_df <- NA
  }
  return(facs_df)
}


#' This function normalizes the n umeric columns in a data frame by
#' subtracting the mean and dividing by the standard deviation
#' given by the df from get_normalizing_factors
#' @param df a data frame
#' @param target_variable a string name for the target variable
#' @param facs_df the factors to normalize by
#' @return a data frame with numerical columns normalized
#' @export
normalize_df <- function(df, target_variable=NA, facs_df){
  if(!is.na(facs_df)){
    norm_colns <- colnames(facs_df)
    for(i in 1:length(norm_colns)){
      df[[norm_colns[i]]] <- (df[[norm_colns[i]]] - facs_df[[norm_colns[i]]][1])
      df[[norm_colns[i]]] <- (df[[norm_colns[i]]] / facs_df[[norm_colns[i]]][2])
    }
    other_columns <- colnames(df)
    for(i in 1:length(other_columns)){
      if(!(other_columns[i] %in% norm_colns) & (other_columns[i] != target_variable)){
        df[[other_columns[i]]] <- df[[other_columns[i]]] %>% as.factor()
      }
    }
  }
  return(df)
}


#' remove columns from the df that contain no information,
#' like factors with only one level
#' @param df a data frame
#' @return data frame with uninformative columns removed
remove_uninformative <- function(df){
  colns <- colnames(df)
  for(i in 1:length(colns)){
    if(length(unique(df[[colns[i]]]))<2){
      df[[colns[i]]] <- NULL
    }
  }
  return(df)
}



#' This function returns a data frame of all the various categories in the
#' training set, so that these are available while pre-processing the test set
#' @param df the training set data frame
#' @return a data frame with all the columns and all levels in the categorical columns
#' @export
get_train_levels <- function(df){
  colns <- colnames(df)
  coln_class <- df %>%
    dplyr::summarise_all(get_class)
  max_levels <- 1
  levels_store <- list()
  for(i in 1:length(colns)){
    if(coln_class[[colns[i]]][1]!="numeric"){
      levels_store[[colns[i]]] <- df[[colns[i]]] %>% as.factor() %>% forcats::fct_unique()
      if(length(levels_store[[colns[i]]])>max_levels){
        max_levels <- length(levels_store[[colns[i]]])
      }
    }
    else {
      levels_store[[colns[i]]] <- c(0)
    }
  }

  for(i in 1:length(colns)){
    if(coln_class[[colns[i]]][1]!="numeric"){
      levels_store[[colns[i]]] <- as.character(levels_store[[colns[i]]])
    }
    levels_store[[colns[i]]] <- c(levels_store[[colns[i]]], rep(levels_store[[colns[i]]][1],max_levels - length(levels_store[[colns[i]]])))
    if(coln_class[[colns[i]]][1]!="numeric"){
      levels_store[[colns[i]]] <- as.factor(levels_store[[colns[i]]])
    }
    if(coln_class[[colns[i]]][1]=="numeric"){
      levels_store[[colns[i]]] <- rnorm(length(levels_store[[colns[i]]]), 0, 1)
    }
  }
  if(length(levels_store)>0){
    levels_df <- levels_store %>% dplyr::as_tibble() %>% tibble::rownames_to_column() %>% dplyr::select(-rowname)
  } else{
    levels_df <- NA
  }
  return(levels_df)
}


#' This function converts the target variable to integer if it is a
#' categorical variable. and builds a map of the transformation.
#' @inheritParams normalize_df
#' @return list
#' @export
transform_target_variable <- function(df, target_variable){
  return_structure <-  list()
  return_df <- list()
  if(class(df[[target_variable]]) != "numeric"){
    return_df[[target_variable]] <- as.integer(as.integer(as.factor(df[[target_variable]]))-1)
    return_df[[paste("original",target_variable,sep = "_")]] <- df[[target_variable]]
    return_df <- tibble::as_tibble(return_df)
    reference_df <- return_df %>%
      dplyr::group_by_at(paste("original",target_variable,sep = "_")) %>%
      dplyr::summarise_at(.vars = target_variable, .funs = min) %>%
      dplyr::mutate_at(.vars = target_variable, .funs = as.integer)
    return_df[[paste("original",target_variable,sep = "_")]] <- NULL
    return_structure[["new_target"]] <- return_df
    return_structure[["target_reference"]] <- reference_df
  } else {
    return_structure[["new_target"]] <- NA
    return_structure[["target_reference"]] <- NA
  }
  return(return_structure)
}


#' This function converts the categorical variables into either numeric (if ordered)
#' or characters, if not.
#' @inheritParams normalize_df
#' @return list
#' @export
rationalize_categoricals <- function(df, target_variable = "y"){
  colns <- colnames(df)
  coln_class <- df %>%
    dplyr::summarise_all(get_class)
  for(i in 1:length(coln_class)){
    if((colns[i]!=target_variable) & (coln_class[[colns[i]]][1]!="numeric")){
      if(coln_class[[colns[i]]][1]=="orderedfactor"){
        temp_col <- as.character(df[[colns[i]]])
        df[[colns[i]]] <- as.factor(temp_col)
      } else {
        temp_col <- as.character(df[[colns[i]]])
        df[[colns[i]]] <- as.factor(temp_col)
      }
    }
  }
  return(df)
}


#' tThis function implements a simple NA handling system for
#' categorical and numeric variables.
#' @inheritParams normalize_df
#' @return list of data frames
#' @export
handle_missing_values <- function(df, target_variable = "y", train_facs){
  df <- df %>%
    dplyr::filter(!is.na(target_variable))
  colns <- colnames(df)
  coln_class <- df %>%
    dplyr::summarise_all(get_class)
  for(i in 1:length(coln_class)){
    if((colns[i]!=target_variable) & (coln_class[[colns[i]]][1]!="numeric") & (sum(is.na(df[[colns[i]]]))>0)){
      df[[colns[i]]] <- as.character(df[[colns[i]]])
      df[[colns[i]]][is.na(df[[colns[i]]])] <- "not_available"
      df[[colns[i]]] <- as.factor(df[[colns[i]]])
    } else {
      if(sum(is.na(df[[colns[i]]]))>0){
        df[[colns[i]]][is.na(df[[colns[i]]])] <- rnorm(sum(is.na(df[[colns[i]]])), train_facs[[colns[i]]][1], train_facs[[colns[i]]][2])
        }
      }
  }
  return(df)
}

#' this function takes a data frame and the target variable and
#' returns a data structure with everything needed to train and
#' predict with the model.
#' @inheritParams normalize_df
#' @return list of data frames
#' @export
prepare_training_set <- function(df, target_variable = "y"){
  # print("aligning all factor variables")
  df <- rationalize_categoricals(df, target_variable)
  # print("normalising all numerical variables")
  train_facs <- get_normalizing_factors(df, target_variable)
  train_data <- normalize_df(df, target_variable, train_facs)
  train_data <- handle_missing_values(train_data, target_variable = target_variable, train_facs = train_facs)
  target_reference <- transform_target_variable(train_data, target_variable)
  train_levels <- get_train_levels(train_data)
  train_levels[[target_variable]] <- NULL
  train_structure <- list()
  if(class(df[[target_variable]]) != "numeric"){
    train_data[[target_variable]] <- target_reference[["new_target"]][[target_variable]]
    train_structure[["target_reference"]] <- target_reference[["target_reference"]]
  }
  train_structure[["data"]] <- train_data
  train_structure[["normalize_by"]] <- train_facs
  train_structure[["levels"]] <- train_levels
  train_structure[["target_variable"]] <- target_variable
  return(train_structure)
}
