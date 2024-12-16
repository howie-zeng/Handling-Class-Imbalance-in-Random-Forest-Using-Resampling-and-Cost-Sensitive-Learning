#' Balance a Dataset Using Specified Sampling Methods
#'
#' This function addresses class imbalance by applying a specified sampling method
#' to a given dataset. Supported methods currently include no sampling, SMOTE
#' (Synthetic Minority Oversampling Technique), Tomek Links, and ADASYN.
#'
#' @param data A `data.frame` containing the dataset to be balanced. Must include
#'   the target column.
#' @param target_col A string specifying the name of the target column (the dependent variable).
#'   The target column should be a factor with two levels.
#' @param method A string specifying the sampling method to use. Options include:
#'   \describe{
#'     \item{"none"}{No sampling, returns the input dataset as is.}
#'     \item{"smote"}{Apply SMOTE oversampling via \code{\link[smotefamily]{SMOTE}}.}
#'     \item{"tomek"}{Apply Tomek Links cleaning via \code{\link[smotefamily]{TomekLinkRemoval}}.}
#'     \item{"adasyn"}{Apply ADASYN oversampling via \code{\link[smotefamily]{ADAS}}.}
#'   }
#' @return A `data.frame` with balanced classes based on the specified method.
#' @import smotefamily
#' @export
apply_sampling <- function(data, target_col, method = c("none", "smote", "tomek", "adasyn", "smotemek")) {
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

  # Identify categorical predictors (for SMOTENC)
  predictors <- setdiff(names(data), target_col)
  original_factors <- sapply(data[predictors], is.factor)

  # If no sampling is requested, return the data unchanged
  if (method == "none") {
    return(data)
  } else if (method %in% c("tomek", "adasyn", "smotemek")) {
    predictors <- setdiff(names(data), target_col)

    # Convert non-numeric predictors to numeric
    for (col in predictors) {
      if (is.factor(data[[col]])) {
        data[[col]] <- as.numeric(as.factor(data[[col]]))  # Convert factor to numeric
      } else if (is.character(data[[col]])) {
        data[[col]] <- as.numeric(as.factor(data[[col]]))  # Convert character to numeric
      }
    }
  }


  # Add the appropriate step based on the selected method
  result <- switch(
    method,
    smote = themis::smotenc(df=data, var=target_col),
    tomek = themis::tomek(df=data, var=target_col),
    adasyn = themis::adasyn(df=data, var=target_col),
    smotemek = SMOTETomek(x = data %>% select(-target), data$target)
  )

  if (method == "smotemek") {
    colnames(result)[colnames(result) == 'y'] <- 'target'
  }

  for (col in names(original_factors[original_factors])) {
    result[[col]] <- factor(result[[col]])
  }
  # Prepare and bake the recipe to get the balanced dataset
  return(result)
}

