#=========================================================
# Numeralize: Convert dataset into numeric matrix
#=========================================================

#' Convert a dataset into a numeric matrix
#'
#' @param data A data frame containing the dataset to convert
#' @param form (Optional) A formula specifying a target column to separate
#'
#' @return A numeric matrix where nominal attributes are one-hot encoded
#'         and numerical attributes are scaled between 0 and 1
#'
#' @export
Numeralize <- function(data, form = NULL) {
  # Validate inputs
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")

  # Separate target column if form is provided
  if (!is.null(form)) {
    tgt <- which(names(data) == as.character(form[[2]]))
    dataY <- data[drop = FALSE, , tgt]
    dataX <- data[, -tgt]
  } else {
    dataX <- data
  }

  numRow <- nrow(dataX)  # Number of rows in the data

  # Identify attribute types
  indexOrder <- sapply(dataX, is.ordered)
  indexMultiValue <- sapply(dataX, nlevels) > 2
  indexNominal <- !indexOrder & indexMultiValue

  numerMatrixNames <- NULL  # Placeholder for numeric matrix column names

  # Process numeric attributes
  if (all(indexNominal)) {
    numerMatrix <- NULL
  } else {
    numerMatrix <- dataX[drop = FALSE, , !indexNominal]
    numerMatrixNames <- colnames(numerMatrix)
    numerMatrix <- data.matrix(numerMatrix)

    # Scale numeric attributes between 0 and 1
    Min <- apply(numerMatrix, 2, min)
    range <- apply(numerMatrix, 2, max) - Min
    numerMatrix <- scale(numerMatrix, center = Min, scale = range)[, ]
  }

  # Process nominal attributes (one-hot encoding)
  if (any(indexNominal)) {
    BiNames <- NULL  # Names for binary columns
    dataNominal <- dataX[drop = FALSE, , indexNominal]
    numNominal <- sum(indexNominal)

    # Determine total binary columns to generate
    if (numNominal > 1) {
      dimEx <- sum(sapply(dataX[, indexNominal], nlevels))
    } else {
      dimEx <- nlevels(dataX[, indexNominal])
    }

    # Initialize binary matrix
    dataBinary <- matrix(nrow = numRow, ncol = dimEx)
    cl <- 0  # Column counter

    # Generate binary columns
    for (i in 1:numNominal) {
      numCat <- nlevels(dataNominal[, i])
      for (j in 1:numCat) {
        value <- levels(dataNominal[, i])[j]
        ind <- (dataNominal[, i] == value)
        dataBinary[, cl + 1] <- as.integer(ind)
        BiNames[cl + 1] <- paste(names(dataNominal)[i], "_", value, sep = "")
        cl <- cl + 1
      }
    }

    # Combine numeric and binary matrices
    numerMatrix <- cbind(numerMatrix, dataBinary)
    colnames(numerMatrix) <- c(numerMatrixNames, BiNames)
  }

  # Add target column back if form is provided
  if (!is.null(form)) {
    numerMatrix <- data.frame(numerMatrix)
    numerMatrix <- cbind(numerMatrix, dataY)
  }

  return(numerMatrix)
}
