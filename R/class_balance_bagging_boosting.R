#' @export
bbaging <-
  function(x, ...)
    UseMethod("bbaging")
#' @export
bbaging.data.frame <-
  function(x, y, numBag = 40, base = treeBag, type = "SMOTEBagging", allowParallel = FALSE, ...)
  {
    # Input:
    #         x: A data frame of predictors from the training set
    #         y: A vector of the response variable from the training set
    #    numBag: Number of bags
    #      base: Base learner (e.g., treeBag)
    #      type: Type of bagging-based algorithm ("SMOTEBagging","RUSBagging","RBBagging","ROSBagging")
    # allowParallel: If TRUE, run using parallel computing

    library(foreach)
    if (allowParallel) library(doParallel)

    funcCall <- match.call(expand.dots = FALSE)
    if (!type %in% c("RUSBagging", "ROSBagging", "SMOTEBagging", "RBBagging"))
      stop("Invalid method type for `type` argument.")

    data <- data.frame(x, y)
    tgt <- length(data)
    x.nam <- names(x)
    form <- as.formula(paste("y ~ ", paste(x.nam, collapse = "+")))
    classTable <- table(data[, tgt])
    classTable <- sort(classTable, decreasing = TRUE)
    classLabels <- names(classTable)

    CreateResample <- function(data, tgt, classLabels, type, numBag, iter, ...)
    {
      indexMaj <- which(data[, tgt] == classLabels[1])
      indexMin <- which(data[, tgt] == classLabels[2])
      numMin   <- length(indexMin)
      numMaj   <- length(indexMaj)

      if (type == "RUSBagging") {
        # RUSBagging
        indexMajSampled <- sample(indexMaj, numMin, replace = FALSE)
        indexMinSampled <- sample(indexMin, numMin, replace = TRUE)
        indexNew <- c(indexMajSampled, indexMinSampled)
        newData  <- data[indexNew, ]
      }

      if (type == "ROSBagging") {
        # ROSBagging
        indexMajSampled <- sample(indexMaj, numMaj, replace = TRUE)
        numMinsampled   <- numMaj - numMin
        indexMinSampled <- sample(indexMin, numMinsampled, replace = TRUE)
        indexNew <- c(indexMajSampled, indexMinSampled, indexMin)
        newData  <- data[indexNew, ]
      }

      if (type == "RBBagging") {
        # RBBagging
        numMajSampled   <- rnbinom(1, numMin, 0.5)
        indexMajSampled <- sample(indexMaj, numMajSampled, replace = FALSE)
        indexMinSampled <- sample(indexMin, numMin, replace = TRUE)
        indexNew <- c(indexMajSampled, indexMinSampled)
        newData  <- data[indexNew, ]
      }

      if (type == "SMOTEBagging") {
        # SMOTEBagging
        numCol <- dim(data)[2]
        n <- (iter - 1) %/% (numBag / 10) + 1
        numMinSampled   <- round(numMaj * n / 10)
        indexMinSampled <- sample(indexMin, numMinSampled, replace = TRUE)
        indexMajSampled <- sample(indexMaj, numMaj, replace = TRUE)
        indexNew <- c(indexMajSampled, indexMinSampled)
        dataROS  <- data[indexNew, ]
        perOver  <- round((numMaj - numMinSampled) / numMin) * 100

        if (perOver > 0) {
          if (tgt < numCol) {
            cols <- 1:numCol
            cols[c(tgt, numCol)] <- cols[c(numCol, tgt)]
            data <- data[, cols]
          }
          newExs <- SmoteExs(data[indexMin, ], perOver, k = 5)
          if (tgt < numCol) {
            newExs <- newExs[, cols]
            data   <- data[, cols]
          }
          newData <- rbind(dataROS, newExs)
        } else {
          newData <- dataROS
        }
      }
      return(newData)
    }

    fitter <- function(form, data, tgt, classLabels, type, base, numBag, iter, ...)
    {
      dataSampled <- CreateResample(data, tgt, classLabels, type, numBag, iter, ...)
      model <- base$fit(form, dataSampled)
    }

    if (allowParallel) {
      `%op%` <- `%dopar%`
      cl <- makeCluster(detectCores()/2)
      registerDoParallel(cl)
    } else {
      `%op%` <- `%do%`
    }

    btFits <- foreach(iter = seq_len(numBag),
                      .verbose = FALSE,
                      .errorhandling = "stop") %op% fitter(form, data, tgt, classLabels, type, base, numBag, iter, ...)

    if (allowParallel) stopCluster(cl)

    structure(
      list(call         = funcCall,
           base         = base,
           type         = type,
           numBag       = numBag,
           classLabels  = classLabels,
           fits         = btFits),
      class = "bbag"
    )
  }

#' @export
predict.bbag <-
  function(obj, x, type = "class")
  {
    if (is.null(x)) stop("Please provide predictors for prediction")
    data <- x
    btPred <- sapply(obj$fits, obj$base$pred, data = data)
    obj$base$aggregate(btPred, obj$classLabels, type)
  }

treeBag <- list(
  fit = function(form, data) {
    library(rpart)
    out <- rpart(form, data)
    return(out)
  },

  pred = function(object, data) {
    out <- predict(object, data, type = "class")
  },

  aggregate = function(x, classLabels, type) {
    if (!type %in% c("class", "probability"))
      stop("Invalid `type` argument.")
    numClass <- length(classLabels)
    numIns   <- nrow(x)
    numBag   <- ncol(x)
    classfinal <- matrix(0, ncol = numClass, nrow = numIns)
    colnames(classfinal) <- classLabels

    for (i in seq_len(numClass)) {
      classfinal[, i] <- (x == classLabels[i]) %*% rep(1, numBag)
    }

    if (type == "class") {
      out <- factor(classLabels[apply(classfinal, 1, which.max)], levels = classLabels)
    } else {
      out <- as.data.frame(classfinal / numBag)
    }
    out
  }
)


#' @export
bboost <-
  function(x, ...)
    UseMethod("bboost")
#' @export
bboost.data.frame <-
  function(x, y, iter = 40, base = treeBoost, type = "SMOTEBoost", costRatio = 56/11, ...)
  {
    # Input:
    #       x: A data frame of the predictors from the training set
    #       y: A vector of the response variable from the training set
    #    iter: Number of training iterations
    #    base: Base learner (e.g., treeBoost)
    #    type: Type of boosting-based algorithm: "AdaBoost", "SMOTEBoost", "RUSBoost", "AdaC2"
    # costRatio: Cost ratio for AdaC2

    if (!type %in% c("AdaBoost", "SMOTEBoost", "RUSBoost", "AdaC2"))
      stop("`type` must be one of 'AdaBoost', 'SMOTEBoost', 'RUSBoost', or 'AdaC2'.")

    funcCall <- match.call(expand.dots = FALSE)
    data <- data.frame(x, y)
    tgt <- length(data)
    classTable <- table(data[, tgt])
    classTable <- sort(classTable, decreasing = TRUE)
    classLabels <- names(classTable)
    indexMaj <- which(data[, tgt] == classLabels[1])
    indexMin <- which(data[, tgt] == classLabels[2])
    numMin <- length(indexMin)
    numMaj <- length(indexMaj)
    numRow <- nrow(data)

    x.nam <- names(x)
    form <- as.formula(paste("y ~ ", paste(x.nam, collapse = "+")))
    H      <- list()
    alpha  <- rep(0, iter)
    oldWeight <- rep(1/numRow, numRow)
    newWeight <- rep(NA, numRow)
    count <- 0
    t <- 0
    earlyStop <- FALSE

    if (type == "AdaC2") {
      cost <- rep(1, numRow)
      cost[indexMin] <- costRatio
    }

    while (t < iter) {
      t <- t + 1
      # Data preparation
      if (type %in% c("AdaBoost", "AdaC2")) {
        indexBootstrap <- sample(seq_len(numRow), replace = TRUE, prob = oldWeight)
        dataResample   <- data[indexBootstrap, ]
      }

      if (type == "SMOTEBoost") {
        perOver <- ((numMaj - numMin) / numMin) * 100
        dataSmoteSample <- SMOTE(form, data, perOver)
        numNew <- nrow(dataSmoteSample)
        resampleWeight <- rep(NA, numNew)
        resampleWeight[1:numRow] <- oldWeight
        resampleWeight[(numRow+1):numNew] <- 1/numNew
        indexBootstrap <- sample(seq_len(numNew), replace = TRUE, prob = resampleWeight)
        dataResample   <- dataSmoteSample[indexBootstrap, ]
      }

      if (type == "RUSBoost") {
        indexMajRUS <- sample(seq_len(numMaj), numMin)
        indexNew <- c(indexMaj[indexMajRUS], indexMin)
        resampleWeight <- oldWeight[indexNew]
        indexBootstrap <- sample(seq_len(2*numMin), replace = TRUE, prob = resampleWeight)
        dataResample <- data[indexNew[indexBootstrap], ]
      }

      # Build classifier with resampling
      H[[t]] <- base$fit(form, dataResample)

      # Compute (pseudo) loss of hypothesis
      if (type == "AdaBoost") {
        weakPrediction <- base$pred(H[[t]], data, type = "class")
        ind <- data[, tgt] == weakPrediction
        loss <- sum(oldWeight * as.numeric(!ind))
        beta <- loss / (1 - loss)
        alpha[t] <- log(1 / beta)
      }

      if (type %in% c("RUSBoost", "SMOTEBoost")) {
        weakPrediction <- base$pred(H[[t]], data, type = "probability")
        loss <- sum(oldWeight * abs(weakPrediction[, 2] - as.numeric(data[, tgt]) + 1))
        beta <- loss / (1 - loss)
        alpha[t] <- log(1 / beta)
      }

      if (type == "AdaC2") {
        weakPrediction <- base$pred(H[[t]], data, type = "class")
        ind <- data[, tgt] == weakPrediction
        alpha[t] <- 0.5 * log(sum(oldWeight[ind] * cost[ind]) / sum(oldWeight[!ind] * cost[!ind]))
      }

      if (alpha[t] < 0) {
        count <- count + 1
        t <- t - 1
        if (count > 5) {
          earlyStop <- TRUE
          warning("Stopped due to too many large errors")
          break
        } else {
          next
        }
      } else {
        count <- 1
      }

      # Update weights
      if (type == "AdaBoost") {
        ind <- data[, tgt] == weakPrediction
        newWeight[ind] <- oldWeight[ind] * beta
        newWeight[!ind] <- oldWeight[!ind]
      }

      if (type %in% c("RUSBoost", "SMOTEBoost")) {
        newWeight <- oldWeight * beta^(1 - abs(weakPrediction[, 2] - as.numeric(data[, tgt]) + 1))
      }

      if (type == "AdaC2") {
        ind <- data[, tgt] == weakPrediction
        newWeight[ind] <- oldWeight[ind] * exp(-alpha[t]) * cost[ind]
        newWeight[!ind] <- oldWeight[!ind] * exp(alpha[t]) * cost[!ind]
      }

      newWeight <- newWeight / sum(newWeight)
      oldWeight <- newWeight
    }

    if (earlyStop) {
      iter <- t
      alpha <- alpha[1:iter]
      H <- H[1:iter]
    }

    structure(
      list(call        = funcCall,
           type        = type,
           base        = base,
           classLabels = classLabels,
           iter        = iter,
           fits        = H,
           alpha       = alpha),
      class = "bboost"
    )
  }
#' @export
predict.bboost <-
  function(obj, x, type = "class")
  {
    if (is.null(x)) stop("Please provide predictors for prediction")
    data <- x
    btPred <- sapply(obj$fits, obj$base$pred, data = data)
    obj$base$aggregate(btPred, obj$alpha, obj$classLabels, type = type)
  }

treeBoost <- list(
  fit = function(form, data) {
    library(rpart)
    out <- rpart(form, data)
    return(out)
  },

  pred = function(object, data, type = "class") {
    out <- predict(object, data, type = type)
  },

  aggregate = function(x, weight, classLabels, type = "class") {
    if (!type %in% c("class", "probability"))
      stop("Invalid `type` argument.")
    numClass   <- length(classLabels)
    numIns     <- nrow(x)
    classfinal <- matrix(0, ncol = numClass, nrow = numIns)
    colnames(classfinal) <- classLabels

    for (i in seq_len(numClass)) {
      classfinal[, i] <- (x == classLabels[i]) %*% weight
    }

    if (type == "class") {
      out <- factor(classLabels[apply(classfinal, 1, which.max)], levels = classLabels)
    } else {
      out <- classfinal / rowSums(classfinal)
    }
    out
  }
)

#' @export
BalanceCascade <-
  function(x, ...)
    UseMethod("BalanceCascade")
#' @export
BalanceCascade.data.frame <-
  function(x, y, iter = 4)
  {
    # Input:
    #        x: A data frame of predictors from the training set
    #        y: A vector of the response variable from the training set
    #     iter: Number of iterations to train base classifiers

    funcCall <- match.call(expand.dots = FALSE)
    data <- data.frame(x, y)
    tgt <- length(data)
    classTable  <- table(data[, tgt])
    classTable  <- sort(classTable, decreasing = TRUE)
    classLabels <- names(classTable)
    indexMaj <- which(data[, tgt] == classLabels[1])
    indexMin <- which(data[, tgt] == classLabels[2])
    numMin <- length(indexMin)
    numMaj <- length(indexMaj)
    FP <- (numMin / numMaj)^(1/(iter-1))

    x.nam <- names(x)
    form <- as.formula(paste("y ~ ", paste(x.nam, collapse = "+")))
    H <- list()
    thresh <- rep(NA, iter)

    for (i in seq_len(iter)) {
      if (length(indexMaj) < numMin)
        numMin  <- length(indexMaj)
      indexMajSampling <- sample(indexMaj, numMin)
      dataCurrent <- data[c(indexMin, indexMajSampling), ]
      H[[i]] <- bboost.data.frame(dataCurrent[, -tgt], dataCurrent[, tgt], type = "AdaBoost")
      pred   <- predict(H[[i]], data[indexMaj, -tgt], type = "probability")
      sortIndex <- order(pred[, 2], decreasing = TRUE)
      numkeep   <- round(length(indexMaj) * FP)
      thresh[i] <- pred[sortIndex[numkeep], 2] * sum(H[[i]]$alpha)
      indexMaj  <- indexMaj[sortIndex[1:numkeep]]
    }

    iter   <- sum(sapply(H, "[[", 5))
    fits   <- unlist(lapply(H, "[[", 6), recursive = FALSE)
    alphas <- unlist(lapply(H, "[[", 7))

    structure(
      list(call        = funcCall,
           iter        = iter,
           classLabels = classLabels,
           base        = H[[1]]$base,
           alphas      = alphas,
           fits        = fits,
           thresh      = sum(thresh)),
      class = "BalanceCascade"
    )
  }
#' @export
predict.BalanceCascade <-
  function(obj, x, type = "class")
  {
    if (is.null(x)) stop("Please provide predictors for prediction")
    if (!type %in% c("class", "probability"))
      stop("Invalid `type` argument.")

    data <- x
    classLabels <- obj$classLabels
    numClass    <- length(classLabels)
    numIns      <- nrow(data)
    weight      <- obj$alphas
    btPred      <- sapply(obj$fits, obj$base$pred, data = data, type ="class")
    classfinal  <- matrix(0, ncol = numClass, nrow = numIns)
    colnames(classfinal) <- classLabels

    for (i in seq_len(numClass)) {
      classfinal[, i] <- (btPred == classLabels[i]) %*% weight
    }

    if (type == "class") {
      classfinal <- classfinal - obj$thresh
      out <- factor(classLabels[apply(classfinal, 1, which.max)], levels = classLabels)
    } else {
      out <- data.frame(classfinal / rowSums(classfinal))
    }
    out
  }


#' @export
EasyEnsemble <-
  function(x, ...)
    UseMethod("EasyEnsemble")
#' @export
EasyEnsemble.data.frame <-
  function(x, y, iter = 4, allowParallel = FALSE, ...)
  {
    # Input:
    #       x: A data frame of predictors from the training set
    #       y: A vector of the response variable from the training set
    #    iter: Iterations to train base classifiers
    # allowParallel: If TRUE, run using parallel computing

    library(foreach)
    if (allowParallel) library(doParallel)

    funcCall <- match.call(expand.dots = FALSE)
    data <- data.frame(x, y)
    tgt <- length(data)
    classTable <- table(data[, tgt])
    classTable <- sort(classTable, decreasing = TRUE)
    classLabels <- names(classTable)
    indexMaj <- which(data[, tgt] == classLabels[1])
    indexMin <- which(data[, tgt] == classLabels[2])
    numMin <- length(indexMin)
    numMaj <- length(indexMaj)
    H <- list()

    fitter <- function(tgt, data, indexMaj, numMin, indexMin) {
      indexMajCurrent <- sample(indexMaj, numMin)
      dataCurrent <- data[c(indexMin, indexMajCurrent),]
      out <- bboost.data.frame(dataCurrent[, -tgt], dataCurrent[, tgt], type = "AdaBoost")
    }

    if (allowParallel) {
      `%op%` <- `%dopar%`
      cl <- makeCluster(2)
      registerDoParallel(cl)
    } else {
      `%op%` <- `%do%`
    }

    H  <- foreach(i = seq_len(iter),
                  .verbose = FALSE,
                  .errorhandling = "stop") %op% fitter(tgt, data, indexMaj, numMin, indexMin)

    if (allowParallel) stopCluster(cl)

    iter   <- sum(sapply(H, "[[", 5))
    fits   <- unlist(lapply(H, "[[", 6), recursive = FALSE)
    alphas <- unlist(lapply(H, "[[", 7))

    structure(
      list(call        = funcCall,
           iter        = iter,
           fits        = fits,
           base        = H[[1]]$base,
           alphas      = alphas,
           classLabels = classLabels),
      class = "EasyEnsemble"
    )
  }

#' @export
predict.EasyEnsemble <-
  function(obj, x, type = "class")
  {
    if (is.null(x)) stop("Please provide predictors for prediction")
    if (!type %in% c("class", "probability"))
      stop("Invalid `type` argument.")
    data <- x
    classLabels <- obj$classLabels
    numClass    <- length(classLabels)
    numIns      <- nrow(data)
    weight      <- obj$alphas
    btPred      <- sapply(obj$fits, obj$base$pred, data = data, type ="class")
    classfinal  <- matrix(0, ncol = numClass, nrow = numIns)
    colnames(classfinal) <- classLabels

    for (i in seq_len(numClass)) {
      classfinal[, i] <- (btPred == classLabels[i]) %*% weight
    }

    if (type == "class") {
      out <- factor(classLabels[apply(classfinal, 1, which.max)], levels = classLabels)
    } else {
      out <- data.frame(classfinal / rowSums(classfinal))
    }
    out
  }


#' @export
SMOTETomek <-
  function(x, y, percOver = 1400, k = 5)
  {
    # Inputs
    #      x    : A data frame of predictors from the training set
    #      y    : A vector of response variable from the training set
    #   perOver : Number of new instances generated for each minority instance (in SMOTE)
    #   k       : Number of nearest neighbors in SMOTE

    newData <- SMOTE(x, y, percOver, k)
    tgt <- length(newData)
    indexTL <- TomekLink(tgt, newData)
    newDataRemoved <- newData[!indexTL, ]
    return(newDataRemoved)
  }
#' @export
TomekLink <-
  function(tgt, data)
  {
    # Identify Tomek links
    indexTomek <- rep(FALSE, nrow(data))
    classTable <- table(data[, tgt])
    majCl <- names(which.max(classTable))
    minCl <- names(which.min(classTable))
    indexMin <- which(data[, tgt] == minCl)

    dataTransformed <- Numeralize(data[, -tgt])

    require("RANN")
    indexOrder1 <- nn2(dataTransformed, dataTransformed[indexMin, ], k = 2)$nn.idx
    indexTomekCa <- data[indexOrder1[, 2], tgt] == majCl
    if (sum(indexTomekCa) > 0) {
      TomekCa <- cbind(indexMin[indexTomekCa], indexOrder1[indexTomekCa, 2])
      indexOrder2 <- nn2(dataTransformed, dataTransformed[TomekCa[, 2], ], k = 2)$nn.idx
      indexPaired <- indexOrder2[, 2] == TomekCa[, 1]
      if (sum(indexPaired) > 0) {
        indexTomek[TomekCa[indexPaired, 1]] <- TRUE
        indexTomek[TomekCa[indexPaired, 2]] <- TRUE
      }
    }
    return(indexTomek)
  }
#' @export
SMOTE <-
  function(x, y, percOver = 1400, k = 5)
  {
    # SMOTE sampling
    # Inputs:
    #    x: Predictors (data frame)
    #    y: Response variable (factor)
    #    percOver/100: Number of new instances generated for each minority instance
    #    k: Number of nearest neighbors

    data <- data.frame(x, y)
    classTable <- table(y)
    tgt <- length(data)
    minClass <- names(which.min(classTable))
    indexMin <- which(data[, tgt] == minClass)
    numMin <- length(indexMin)
    majClass <- names(which.max(classTable))
    indexMaj <- which(data[, tgt] == majClass)

    if (percOver < 100) {
      indexMinSelect <- sample(seq_len(numMin), round(numMin * percOver / 100))
      dataMinSelect  <- data[indexMin[indexMinSelect], ]
      percOver <- 100
    } else {
      dataMinSelect <- data[indexMin, ]
    }

    newExs <- SmoteExs(dataMinSelect, percOver, k)
    newData <- rbind(data, newExs)
    return(newData)
  }

Numeralize <-
  function(data, form = NULL)
  {
    if (!is.null(form))
    {
      tgt    <- which(names(data) == as.character(form[[2]]))
      dataY <- data[drop = FALSE,, tgt]
      dataX <- data[, -tgt]
    } else {
      dataX <- data
    }
    numRow      <- dim(dataX)[1]
    #numCol      <- dim(dataX)[2]
    indexOrder      <- sapply(dataX, is.ordered)
    indexMultiValue <- sapply(dataX, nlevels)>2
    indexNominal    <- !indexOrder & indexMultiValue
    numerMatrixNames<- NULL
    if (all(indexNominal))
    {
      numerMatrix   <- NULL
    } else {
      numerMatrix      <- dataX[drop = FALSE, ,!indexNominal]
      numerMatrixNames <- colnames(numerMatrix)
      numerMatrix      <- data.matrix(numerMatrix)
      Min              <- apply(numerMatrix, 2, min)
      range            <- apply(numerMatrix, 2, max)-Min
      numerMatrix      <- scale(numerMatrix, Min, range)[, ]
    }

    if (any(indexNominal))
    {

      BiNames     <- NULL
      dataNominal <- dataX[drop = FALSE, ,indexNominal]
      numNominal  <- sum(indexNominal)
      if (numNominal>1)
      {
        dimEx <- sum(sapply(dataX[,indexNominal], nlevels))
      } else {
        dimEx <- nlevels(dataX[, indexNominal])
      }
      dataBinary  <- matrix(nrow = numRow, ncol = dimEx )
      cl <- 0
      for (i in 1:numNominal)
      {
        numCat <- nlevels(dataNominal[, i])
        for (j in 1:numCat)
        {
          value <- levels(dataNominal[, i])[j]
          ind  <- (dataNominal[,i] == value)
          dataBinary[, cl+1] <- as.integer(ind)
          BiNames[cl+1]   <- paste(names(dataNominal)[i], "_", value, sep="")
          cl <- cl+1
        }
      }
      numerMatrix  <- cbind(numerMatrix, dataBinary)
      colnames(numerMatrix) <- c(numerMatrixNames, BiNames)
    }

    if (!is.null(form))
    {
      numerMatrix <- data.frame(numerMatrix)
      numerMatrix <- cbind(numerMatrix, dataY)
    }
    return(numerMatrix)
  }

SmoteExs<-
  function(data, percOver, k)
    # Input:
    #     data      : dataset of the minority instances
    #     percOver   : percentage of oversampling
    #     k         : number of nearest neighours

  {
    # transform factors into integer
    nomAtt  <- c()
    numRow  <- dim(data)[1]
    numCol  <- dim(data)[2]
    dataX   <- data[ ,-numCol]
    dataTransformed <- matrix(nrow = numRow, ncol = numCol-1)
    for (col in 1:(numCol-1))
    {
      if (is.factor(data[, col]))
      {
        dataTransformed[, col] <- as.integer(data[, col])
        nomAtt <- c(nomAtt , col)
      } else {
        dataTransformed[, col] <- data[, col]
      }
    }
    numExs  <-  round(percOver/100) # this is the number of artificial instances generated
    newExs  <-  matrix(ncol = numCol-1, nrow = numRow*numExs)

    indexDiff <- sapply(dataX, function(x) length(unique(x)) > 1)
    numerMatrix <- Numeralize(dataX[ ,indexDiff])
    require("RANN")
    id_order <- nn2(numerMatrix, numerMatrix, k+1)$nn.idx
    for(i in 1:numRow)
    {
      kNNs   <- id_order[i, 2:(k+1)]
      newIns <- InsExs(dataTransformed[i, ], dataTransformed[kNNs, ], numExs, nomAtt)
      newExs[((i-1)*numExs+1):(i*numExs), ] <- newIns
    }

    # get factors as in the original data.
    newExs <- data.frame(newExs)
    for(i in nomAtt)
    {
      newExs[, i] <- factor(newExs[, i], levels = 1:nlevels(data[, i]), labels = levels(data[, i]))
    }
    newExs[, numCol] <- factor(rep(data[1, numCol], nrow(newExs)), levels=levels(data[, numCol]))
    colnames(newExs) <- colnames(data)
    return(newExs)
  }

#=================================================================
# InsExs: generate Synthetic instances from nearest neighborhood
#=================================================================

InsExs <-
  function(instance, dataknns, numExs, nomAtt)
    # Input:
    #    instance : selected instance
    #    dataknns : nearest instance set
    #    numExs   : number of new intances generated for each instance
    #    nomAtt   : indicators of factor variables
  {
    numRow  <- dim(dataknns)[1]
    numCol  <- dim(dataknns)[2]
    newIns <- matrix (nrow = numExs, ncol = numCol)
    neig   <- sample(1:numRow, size = numExs, replace = TRUE)

    # generated  attribute values
    insRep  <- matrix(rep(instance, numExs), nrow = numExs, byrow = TRUE)
    diffs   <- dataknns[neig,] - insRep
    newIns  <- insRep + runif(1)*diffs
    # randomly change nominal attribute
    for (j in nomAtt)
    {
      newIns[, j]   <- dataknns[neig, j]
      indexChange   <- runif(numExs) < 0.5
      newIns[indexChange, j] <- insRep[indexChange, j]
    }
    return(newIns)
  }

