#=========================================================
# SmoteExs: Generate SMOTE instances for minority class
#=========================================================

#' Generate synthetic SMOTE instances for a minority class
#'
#' @param data A data frame containing the minority class instances
#' @param percOver Percentage of oversampling (e.g., 200 for 200%)
#' @param k Number of nearest neighbors to consider
#'
#' @return A data frame containing synthetic instances for the minority class
#'
#' @importFrom RANN nn2
#' @export
SmoteExs <- function(data, percOver, k) {
  # Input:
  #   data     : Dataset containing minority class instances
  #   percOver : Percentage of oversampling (e.g., 200 for 200%)
  #   k        : Number of nearest neighbors

  # Initialize variables
  nomAtt <- c()  # Indices of nominal (factor) attributes
  numRow <- nrow(data)  # Number of rows in the data
  numCol <- ncol(data)  # Number of columns in the data
  dataX <- data[, -numCol]  # Exclude the target column for processing
  dataTransformed <- matrix(nrow = numRow, ncol = numCol - 1)

  # Transform factors to integers for processing
  for (col in 1:(numCol - 1)) {
    if (is.factor(data[, col])) {
      dataTransformed[, col] <- as.integer(data[, col])
      nomAtt <- c(nomAtt, col)
    } else {
      dataTransformed[, col] <- data[, col]
    }
  }

  # Calculate the number of synthetic instances to generate
  numExs <- round(percOver / 100)
  newExs <- matrix(ncol = numCol - 1, nrow = numRow * numExs)

  # Ensure columns with unique values are processed correctly
  indexDiff <- sapply(dataX, function(x) length(unique(x)) > 1)
  require("RANN")  # Load RANN for nearest neighbor search

  # Find k nearest neighbors
  numerMatrix <- as.matrix(dataX[, indexDiff])
  nnIndices <- nn2(numerMatrix, numerMatrix, k + 1)$nn.idx

  # Generate synthetic instances for each row
  for (i in 1:numRow) {
    kNNs <- nnIndices[i, 2:(k + 1)]  # Exclude the first neighbor (itself)
    newInstances <- InsExs(dataTransformed[i, ], dataTransformed[kNNs, ], numExs, nomAtt)
    newExs[((i - 1) * numExs + 1):(i * numExs), ] <- newInstances
  }

  # Convert generated data back to original format
  newExs <- data.frame(newExs)
  for (i in nomAtt) {
    newExs[, i] <- factor(newExs[, i], levels = 1:nlevels(data[, i]), labels = levels(data[, i]))
  }

  # Assign the target column with the minority class label
  newExs[, numCol] <- factor(rep(data[1, numCol], nrow(newExs)), levels = levels(data[, numCol]))
  colnames(newExs) <- colnames(data)

  return(newExs)
}

#=================================================================
# InsExs: Generate synthetic instances using nearest neighbors
#=================================================================

#' Generate synthetic instances from nearest neighbors
#'
#' @param instance A single instance to oversample from
#' @param dataknns Nearest neighbor instances
#' @param numExs Number of synthetic instances to generate
#' @param nomAtt Indices of nominal (factor) attributes
#'
#' @return A matrix containing generated synthetic instances
#' @export
InsExs <- function(instance, dataknns, numExs, nomAtt) {
  # Input:
  #   instance  : Single instance to oversample from
  #   dataknns  : Nearest neighbor instances
  #   numExs    : Number of synthetic instances to generate
  #   nomAtt    : Indices of nominal (factor) attributes

  numRow <- nrow(dataknns)  # Number of nearest neighbors
  numCol <- ncol(dataknns)  # Number of attributes
  newIns <- matrix(nrow = numExs, ncol = numCol)

  # Randomly sample neighbors
  neighbors <- sample(1:numRow, size = numExs, replace = TRUE)

  # Generate synthetic instances
  insRep <- matrix(rep(instance, numExs), nrow = numExs, byrow = TRUE)
  diffs <- dataknns[neighbors, ] - insRep
  newIns <- insRep + runif(numExs) * diffs

  # Adjust nominal attributes
  for (j in nomAtt) {
    newIns[, j] <- dataknns[neighbors, j]
    indexChange <- runif(numExs) < 0.5
    newIns[indexChange, j] <- insRep[indexChange, j]
  }

  return(newIns)
}
