#' Balance a Dataset Using Specified Sampling Methods
#'
#' This function addresses class imbalance by applying a specified sampling method
#' to a given dataset. It supports modern, recipe-based transformations through
#' the **`themis`** package, which is part of the **tidymodels** ecosystem.
#' Supported methods currently include no sampling, SMOTE (Synthetic Minority Oversampling
#' Technique), Tomek Links, and ADASYN.
#'
#' @param data A `data.frame` containing the dataset to be balanced. Must include
#'   the target column.
#' @param target_col A string specifying the name of the target column (the dependent variable).
#'   The target column should be a factor with two or more levels.
#' @param method A string specifying the sampling method to use. Options include:
#'   \describe{
#'     \item{"none"}{No sampling, returns the input dataset as is.}
#'     \item{"smote"}{Apply SMOTE oversampling via \code{\link[themis]{step_smote}}.}
#'     \item{"tomek"}{Apply Tomek Links cleaning via \code{\link[themis]{step_tomek}}.}
#'     \item{"adasyn"}{Apply ADASYN oversampling via \code{\link[themis]{step_adasyn}}.}
#'   }
#' @return A `data.frame` with balanced classes based on the specified method.
#' @import recipes
#' @import themis
#' @export
apply_sampling <- function(data, target_col, method = c("none", "smote", "tomek", "adasyn")) {
  method <- match.arg(method)

  # Input validation
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!target_col %in% colnames(data)) {
    stop("`target_col` must be a valid column name in `data`.")
  }
  if (!is.factor(data[[target_col]])) {
    stop("`target_col` must be a factor with two or more levels.")
  }

  # If no sampling is requested, return the data unchanged
  if (method == "none") {
    return(data)
  }

  # Construct a recipe for data balancing
  rec <- recipes::recipe(as.formula(paste(target_col, "~ .")), data = data)

  # Add the appropriate step based on the selected method
  rec <- switch(
    method,
    smote = rec %>% themis::step_smote(all_predictors(), under_ratio = 1),
    tomek = rec %>% themis::step_tomek(all_predictors(), all_outcomes()),
    adasyn = rec %>% themis::step_adasyn(all_predictors(), under_ratio = 1)
  )

  # Prepare and bake the recipe to get the balanced dataset
  balanced_data <- recipes::prep(rec, training = data) %>% recipes::bake(new_data = NULL)

  # Ensure the target column is named consistently (should already be, but just in case)
  if (!target_col %in% colnames(balanced_data)) {
    colnames(balanced_data)[ncol(balanced_data)] <- target_col
  }

  return(balanced_data)
}
