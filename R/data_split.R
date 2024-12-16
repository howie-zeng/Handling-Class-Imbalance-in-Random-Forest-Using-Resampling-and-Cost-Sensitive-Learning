#' Split Data for Multiple Datasets
#'
#' This function processes a list of datasets, applying different splitting strategies based on the dataset name.
#' For `"Bank Fraud Dataset"`, it uses `split_train_test_by_month` with predefined parameters.
#' For all other datasets, it uses `split_train`.
#'
#' @param df_list A named list of data frames to process.
#' @param target_col A character string specifying the target column name (assumed to be consistent across all datasets).
#' @param train_ratio Proportion of data to use for training (for `split_train`). Default is 0.75.
#' @param maxiter Number of iterations for imputation (for `split_train`). Default is 2.
#' @param ntree Number of trees for imputation (for `split_train`). Default is 20.
#' @param scale_cols Columns to scale (for `split_train`). If NULL, numeric columns are scaled. Default is NULL.
#' @param seed Random seed for reproducibility. Default is 2024.
#' @export
split_data <- function(df_list, target_col, train_ratio = 0.75, maxiter = 2, ntree = 20, scale_cols = NULL, seed = 2024) {
  set.seed(seed)

  split_results <- list()  # Initialize an empty list to store results

  for (name in names(df_list)) {
    cat("Processing dataset:", name, "\n")
    data <- df_list[[name]]  # Extract the dataset
    data <- convert_to_numeric(data)

    if (name == "Bank Fraud Dataset") {
      # Use month-based split for Bank Fraud Dataset
      split_results[[name]] <- split_train_test_by_month(
        data = data,
        target_col = target_col,
        month_col = "month",
        train_months = 0:5,
        test_months = 6:7,
        seed = seed
      )
    } else {
      # Use generic split for other datasets
      split_results[[name]] <- split_train(
        data = data,
        target_col = target_col,
        train_ratio = train_ratio,
        maxiter = maxiter,
        ntree = ntree,
        scale_cols = scale_cols,
        seed = seed
      )
    }
  }

  return(split_results)
}

split_train_test_by_month <- function(data, target_col, month_col, train_months, test_months, seed = 2024) {
  set.seed(seed)

  imputed_data <- missForest(xmis = data, maxiter = 2, ntree = 50)$ximp

  train_data <- imputed_data %>% filter(!!sym(month_col) %in% train_months)
  test_data <- imputed_data %>% filter(!!sym(month_col) %in% test_months)

  scale_cols <- names(imputed_data)[sapply(imputed_data, is.numeric) & names(imputed_data) != target_col]

  # robust_scaler <- function(x) {
  #   (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
  # }

  scaling_params <- lapply(train_data[scale_cols], function(col) scale(col, center = TRUE, scale = TRUE))
  # Apply scaling to train data
  # train_data[scale_cols] <- lapply(seq_along(scale_cols), function(i) {
  #   scale(
  #     train_data[[scale_cols[i]]],
  #     center = attr(scaling_params[[i]], "scaled:center"),
  #     scale = attr(scaling_params[[i]], "scaled:scale")
  #   )
  # })
  #
  # # Apply scaling to test data using training parameters
  # test_data[scale_cols] <- lapply(seq_along(scale_cols), function(i) {
  #   scale(
  #     test_data[[scale_cols[i]]],
  #     center = attr(scaling_params[[i]], "scaled:center"),
  #     scale = attr(scaling_params[[i]], "scaled:scale")
  #   )
  # })

  list(
    train = train_data,
    validation = test_data
  )
}

split_train <- function(data, target_col, train_ratio = 0.75, maxiter = 2, ntree = 50, scale_cols = NULL, seed = 2024) {
  set.seed(seed)

  # Step 1: Impute missing values
  imputed_data <- missForest(xmis = data, maxiter = maxiter, ntree = ntree)$ximp

  # Step 2: Convert the target column to factor if not already
  imputed_data[[target_col]] <- as.factor(imputed_data[[target_col]])

  # Step 3: Automatically detect numerical columns to scale if scale_cols is NULL
  if (is.null(scale_cols)) {
    scale_cols <- names(imputed_data)[sapply(imputed_data, is.numeric) & names(imputed_data) != target_col]
  }

  # Step 4: Apply Robust Scaling to specified columns
  # robust_scaler <- function(x) {
  #   (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
  # }
  # imputed_data[scale_cols] <- lapply(imputed_data[scale_cols], robust_scaler)
  # Apply scaling to train data
  # Step 6: Split into training and validation sets
  # Step 5: Stratified split using caret's createDataPartition
  trainIndex <- createDataPartition(imputed_data[[target_col]], p = train_ratio, list = FALSE)
  train_data <- imputed_data[trainIndex, ]
  test_data <- imputed_data[-trainIndex, ]



  # scaling_params <- lapply(train_data[scale_cols], function(col) scale(col, center = TRUE, scale = TRUE))
  # train_data[scale_cols] <- lapply(seq_along(scale_cols), function(i) {
  #   scale(
  #     train_data[[scale_cols[i]]],
  #     center = attr(scaling_params[[i]], "scaled:center"),
  #     scale = attr(scaling_params[[i]], "scaled:scale")
  #   )
  # })
  #
  # # Apply scaling to test data using training parameters
  # test_data[scale_cols] <- lapply(seq_along(scale_cols), function(i) {
  #   scale(
  #     test_data[[scale_cols[i]]],
  #     center = attr(scaling_params[[i]], "scaled:center"),
  #     scale = attr(scaling_params[[i]], "scaled:scale")
  #   )
  # })

  # Return both datasets
  list(train = train_data, validation = test_data)
}

convert_to_numeric <- function(df, target_col='target') {
  df[] <- lapply(names(df), function(col_name) {
    if (col_name == target_col) {
      return(df[[col_name]])  # Keep the target column as is
    } else if (is.factor(df[[col_name]]) || is.character(df[[col_name]])) {
      return(as.numeric(as.factor(df[[col_name]])))  # Convert factor/character to numeric
    } else {
      return(df[[col_name]])  # Leave numeric columns unchanged
    }
  })
  return(as.data.frame(df))
}
