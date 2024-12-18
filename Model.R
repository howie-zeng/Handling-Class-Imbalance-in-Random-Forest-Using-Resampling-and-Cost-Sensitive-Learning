library(ebmc)
library(ranger)
library(pROC)
library(ImbalanceRF)


num_cores <- parallel::detectCores() - 3


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


optimize_model <- function(objective_function, bounds, init_points = 5, iter_points = 10, ...) {
  opt_result <- bayesOpt(
    FUN = objective_function,
    bounds = bounds,
    initPoints = 7,  # Specify either `initPoints`
    iters.n = num_cores,    # Number of optimization iterations
    verbose = 1,
    parallel = TRUE
  )
  return(opt_result)
}


compute_roc_auc <- function(model, data_validation, target_col, type = "prob") {
  predictions <- predict(model, newdata = data_validation, type = type)
  if (type == "prob") {
    predicted_probs <- predictions[, 2]
  } else {
    predicted_probs <- as.numeric(predictions)
  }
  roc_auc <- pROC::auc(pROC::roc(data_validation[[target_col]], predicted_probs))
  return(roc_auc)
}





run_model_with_bayesopt <- function(train, test, target_col, method) {
  train[[target_col]] <- factor(train[[target_col]])
  test[[target_col]] <- factor(test[[target_col]])

  target_formula <- as.formula(paste(target_col, "~ ."))

  # Define parameter bounds specific to the method
  if (method == "SMOTEBagging" || method == "SMOTEBoosting") {
    bounds <- list(
      size = c(10, 50),
      smote.k = c(3, 10),
      rf.ntree = c(5, 200)
    )
  } else if (method == "BalanceCascade" || method == "EasyEnsemble") {
    bounds <- list(
      iter = c(4, 50)
    )
  } else if (method == "cost-sensitive-ranger") {
    bounds <- list(
      mtry = c(max(1, floor(ncol(train) * 0.2)), max(1, floor(ncol(train) * 0.9))),
      min_node_size = c(1, 20),
      num_trees = c(5, 200),
      class_weight1 = c(1, 100)      # Class weight for minority
    )
  } else {
    stop(paste("Unsupported method:", method))
  }

  # Define the objective function for Bayesian Optimization
  objective_function <- function(...) {
    params <- list(...)

    # Train the model based on the method
    if (method == "SMOTEBagging") {
      model <- sbag(
        formula = target_formula,
        data = train,
        size = round(params$size),
        alg = "rf",
        smote.k = round(params$smote.k),
        rf.ntree = round(params$rf.ntree)
      )
    } else if (method == "SMOTEBoosting") {
      model <- sbo(
        formula = target_formula,
        data = train,
        size = round(params$size),
        alg = "rf",
        smote.k = round(params$smote.k),
        over = 200,
        rf.ntree = round(params$rf.ntree)
      )
    } else if (method == "BalanceCascade") {
      model <- BalanceCascade(
        x = train[, !colnames(train) %in% target_col],
        y = train[[target_col]],
        iter = round(params$iter)
      )
    } else if (method == "EasyEnsemble") {
      model <- EasyEnsemble(
        x = train[, !colnames(train) %in% target_col],
        y = train[[target_col]],
        iter = round(params$iter),
        allowParallel = TRUE
      )
    } else if (method == "cost-sensitive-ranger") {
      class_weights <- c(1, params$class_weight1)
      model <- ranger::ranger(
        formula = target_formula,
        data = train,
        mtry = params$mtry,
        num.trees = round(params$num_trees),
        min.node.size = round(params$min_node_size),
        class.weights = class_weights,
        probability = TRUE,
      )
    }

    # Evaluate the model

    if (method %in% c("BalanceCascade", "EasyEnsemble")) {
      predictions <- predict(model, test[, !colnames(test) %in% target_col], type = "probability")[, 2]
      roc_auc <- pROC::auc(pROC::roc(test[[target_col]], predictions))
    } else if (method == "cost-sensitive-ranger") {
      predictions <- predict(model, test, type = "response")$predictions[, 2]
      roc_auc <- pROC::auc(pROC::roc(test[[target_col]], predictions))
    } else {
      predictions <- predict(model, newdata = test, type = "prob")
      roc_auc <- pROC::auc(pROC::roc(test[[target_col]], predictions))
    }
    return(list(Score = roc_auc))
  }
  # Run Bayesian Optimization
  opt_result <- bayesOpt(
    FUN = objective_function,
    bounds = bounds,
    initPoints = 7,
    iters.n = num_cores,
    iters.k = round(num_cores / 2),
    verbose = 1,
    parallel = TRUE
  )

  print("Evaluating model")
  # Get the best parameters and train the final model
  best_params <- getBestPars(opt_result)

  if (method == "SMOTEBagging") {
    final_model <- sbag(
      formula = target_formula,
      data = train,
      size = round(best_params$size),
      alg = "rf",
      smote.k = round(best_params$smote.k),
      rf.ntree = round(best_params$rf.ntree)
    )
  } else if (method == "SMOTEBoosting") {
    final_model <- sbo(
      formula = target_formula,
      data = train,
      size = round(best_params$size),
      alg = "rf",
      over = 200,
      smote.k = round(best_params$smote.k),
      rf.ntree = round(best_params$rf.ntree)
    )
  } else if (method == "BalanceCascade") {
    final_model <- ImbalanceRF::BalanceCascade()(
      x = train[, !colnames(train) %in% target_col],
      y = train[[target_col]],
      iter = round(best_params$iter)
    )
  } else if (method == "EasyEnsemble") {
    final_model <- ImbalanceRF::EasyEnsemble(
      x = train[, !colnames(train) %in% target_col],
      y = train[[target_col]],
      iter = round(best_params$iter),
      allowParallel = TRUE
    )
  } else if (method == "cost-sensitive-ranger") {
    class_weights <- c(1, best_params$class_weight1)
    final_model <- ranger::ranger(
      formula = target_formula,
      data = train,
      class.weights = class_weights,
      probability = TRUE,
      mtry = best_params$mtry,
      min.node.size = round(best_params$min_node_size),
      num.trees = round(best_params$num_trees)
    )
  }

  # Evaluate the final model
  if (method %in% c("BalanceCascade", "EasyEnsemble")) {
    predictions <- predict(final_model, test[, !colnames(test) %in% target_col], type = "probability")[, 2]
    predicted_labels <- ifelse(predictions >= 0.5, 1, 0)
  } else if (method == "cost-sensitive-ranger") {
    predictions <- predict(final_model, test, type = "response")$predictions[, 2]
    predicted_labels <- ifelse(predictions >= 0.5, 1, 0)
  } else {
    predictions <- predict(final_model, test, type = "prob")
    predicted_labels <- ifelse(predictions >= 0.5, 1, 0)
  }
  metrics <- calculate_metrics(test[[target_col]], predicted_labels, predictions)
  return(list(Model = final_model, Metrics = metrics, BestParams = best_params))
}


evaluate_model <- function(train, test, target_col, method) {
  train[[target_col]] <- factor(train[[target_col]], levels = c(0, 1))
  test[[target_col]] <- factor(test[[target_col]], levels = c(0, 1))

  target_formula <- as.formula(paste(target_col, "~ ."))

  # Train models based on the method
  if (method == "SMOTEBagging") {
    model <- sbag(formula = target_formula, data = train, size = 20, alg = "rf", smote.k = 5, rf.ntree = 100)
    predictions <- predict(model, newdata = test, type = "prob")
  } else if (method == "SMOTEBoosting") {
    model <- sbo(formula = target_formula, data = train, size = 20, alg = "rf", over = 200, rf.ntree = 100)
    predictions <- predict(model, newdata = test, type = "prob")
  } else if (method == "EasyEnsemble") {
    model <- EasyEnsemble(train[, !names(train) %in% target_col], train[[target_col]], iter = 5, allowParallel=TRUE)
    predictions <- predict(model, test[, !names(test) %in% target_col], type = "probability")[, 2]
  } else if (method == "BalanceCascade") {
    model <- BalanceCascade(train[, !names(train) %in% target_col], train[[target_col]], iter = 5)
    predictions <- predict(model, test[, !names(test) %in% target_col], type = "probability")[, 2]
  }

  predicted_labels <- ifelse(predictions >= 0.5, 1, 0)
  metrics <- calculate_metrics(test[[target_col]], predicted_labels, predictions)

  return(list(Metrics = metrics))
}

