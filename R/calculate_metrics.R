#' Calculate Performance Metrics
#'
#' This function calculates performance metrics for a classification model, including the confusion matrix and ROC AUC.
#'
#' @param true_labels A vector of true class labels.
#' @param predicted_labels A vector of predicted class labels.
#' @param predicted_probs A vector of predicted probabilities for the positive class (optional).
#' @param weights Optional weights for observations (default is NULL).
#' @return A list containing:
#' \item{ConfusionMatrix}{The confusion matrix.}
#' \item{ROCAUC}{The area under the ROC curve (AUC).}
#' @export
#' @examples
#' true_labels <- c(0, 1, 1, 0, 1)
#' predicted_labels <- c(0, 1, 0, 0, 1)
#' predicted_probs <- c(0.2, 0.8, 0.4, 0.1, 0.9)
#' calculate_metrics(true_labels, predicted_labels, predicted_probs)
calculate_metrics <- function(true_labels, predicted_labels, predicted_probs, weights = NULL) {
  # Input validation
  if (length(true_labels) != length(predicted_labels)) {
    stop("`true_labels` and `predicted_labels` must have the same length.")
  }

  # Confusion Matrix
  conf_matrix <- confusionMatrix(factor(predicted_labels), factor(true_labels))

  # ROC AUC
  if (!is.null(predicted_probs)) {
    roc_obj <- roc(true_labels, predicted_probs, levels = c(0, 1), direction = "<")
    auc_value <- auc(roc_obj)
  } else {
    auc_value <- NA
  }

  TP <- conf_matrix$table[2, 2]
  FP <- conf_matrix$table[1, 2]
  TN <- conf_matrix$table[1, 1]
  FN <- conf_matrix$table[2, 1]

  accuracy <- (TP + TN) / sum(conf_matrix$table)
  weighted_accuracy <- (TP / (TP + FN) + TN / (TN + FP)) / 2
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2 * (precision * recall) / (precision + recall)
  specificity <- TN / (TN + FP)
  gmean <- sqrt(recall * specificity)

  # Output all metrics as a list
  list(
    ConfusionMatrix = conf_matrix,
    Accuracy = accuracy,
    WeightedAccuracy = weighted_accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    Specificity = specificity,
    GMean = gmean,
    ROCAUC = auc_value
  )
}
