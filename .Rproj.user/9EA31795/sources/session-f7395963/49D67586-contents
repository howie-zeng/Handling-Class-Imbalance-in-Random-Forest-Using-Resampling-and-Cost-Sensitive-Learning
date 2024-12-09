#' Calculate Percentage of Target Variable
#'
#' This function calculates the count and percentage of each unique value in the specified target column.
#'
#' @param data A data frame containing the dataset to analyze.
#' @param target_col The name of the target column to analyze.
#' @return A data frame with counts and percentages of each unique value in the target column.
#' @importFrom dplyr %>% mutate group_by summarize
#' @importFrom rlang sym
#' @export
calculate_percentage <- function(data, target_col) {
  if (!(target_col %in% names(data))) {
    stop(paste("Column", target_col, "not found in the dataset."))
  }

  data %>%
    mutate(!!sym(target_col) := as.character(!!sym(target_col))) %>%
    group_by(!!sym(target_col)) %>%
    summarize(Count = n(), .groups = "drop") %>%
    mutate(Percentage = (Count / sum(Count)) * 100)
}

#' Explore Target Variable Across Datasets
#'
#' This function explores the target variable across a list of datasets, calculating counts and percentages for each unique target value.
#'
#' @param data_list A named list of data frames to analyze. Each dataset should have a "target" column.
#' @return A combined data frame containing counts and percentages for the target variable across all datasets.
#' @importFrom dplyr %>% bind_rows mutate
#' @export
explore_datasets <- function(data_list) {
  if (!is.list(data_list) || is.null(names(data_list))) {
    stop("`data_list` must be a named list of data frames.")
  }

  combined_results <- data.frame()  # Initialize an empty data frame

  for (name in names(data_list)) {
    cat("Dataset:", name, "\n")  # Print the dataset name for context

    # Check if the dataset contains a "target" column
    if ("target" %in% colnames(data_list[[name]])) {
      # Print unique target values
      target_values <- unique(data_list[[name]]$target)
      cat("Unique target values:", paste(target_values, collapse = ", "), "\n")

      # Calculate percentages
      result_table <- calculate_percentage(data_list[[name]], "target")
      result_table <- result_table %>% mutate(Dataset = name)  # Add dataset name for identification

      # Append the results to the combined results data frame
      combined_results <- bind_rows(combined_results, result_table)
    } else {
      warning(paste("Target column not found in dataset:", name))
    }

    cat("\n")  # Add a blank line for readability
  }

  return(combined_results)
}
