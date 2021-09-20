.pca_svd <- function(x, npc, center = TRUE, scale = FALSE) {
  call <- match.call()
  pc <- prcomp(x, center = center, scale. = scale)
  imp <- summary(pc)$importance
  out <- pc$x[, 1:npc]
  loadings <- pc$rotation[, 1:npc]
  var_exp <- imp[2, 1:npc]
  var_exp_cum <- imp[3, 1:npc]
  attr(out, "method") <- "SVD"
  attr(out, "npc") <- npc
  attr(out, "inputDim") <- dim(x)
  attr(out, "center") <- scale
  attr(out, "scale") <- scale
  attr(out, "R2") <- var_exp
  attr(out, "R2cum") <- var_exp_cum
  attr(out, "loadings") <- loadings
  attr(out, "sdev") <- pc$sdev[1:npc]
  attr(out, "call") <- call
  out
}

################################################################################
## Bayesian PCA taken from pcaMethods pacakage (09/19/2021)
################################################################################
.bpca <- function(x, npc = 2, max_steps = 100, verbose=interactive(),
                  threshold = 1e-4) {

  ## R implementation of a Bayesion PCA missing value estimator.
  ## After the Matlab script of Shigeyuki OBA (2002  May. 5th)
  ## See also: http://hawaii.aist-nara.ac.jp/%7Eshige-o/tools/
  ## Great thanks to them!
  M <- .BPCA_initmodel(x, npc)
  tauold <- 1000

  for(step in 1:max_steps) {
    M <- .BPCA_dostep(M, x)
    if (step %% 10 == 0) {
      tau <- M$tau
      dtau <- abs(log10(tau) - log10(tauold))
      if ( verbose ) {
        cat("Step Number           : ", step, '\n')
        cat("Increase in precision : ", dtau, '\n')
        cat("----------", '\n')
      }
      if (dtau < threshold) {
        break
      }
      tauold <- tau
    }
  }
  
  R2cum <- rep(NA, npc)
  TSS <- sum(x^2, na.rm = TRUE)
  for (i in 1:npc) {
    difference <-
      x - (M$scores[, 1:i, drop = FALSE] %*% t(M$PA[, 1:i, drop = FALSE]) )
    R2cum[i] <- 1 - (sum(difference^2, na.rm = TRUE) / TSS)
  }

  result <- new("pcaRes")
  result@scores <- M$scores 
  result@loadings <- M$PA
  result@R2cum <- R2cum
  result@method <- "bpca"
  return(result)
}

##' The function contains the actual implementation of the BPCA
##' component  estimation. It performs one step of the BPCA EM
##' algorithm.  It is called 'maxStep' times from within the main loop
##' in BPCAestimate.
##'
##' This function is NOT intended to be run standalone.
##' @title Do BPCA estimation step
##' @param M Data structure containing all needed information. See the
##' source documentation of BPCA_initmodel for details
##' @param y Numeric original data matrix
##' @return Updated version of the data structure
##' @author Wolfram Stacklies
.BPCA_dostep <- function(M,y) {

  ## Empty matrix in which the scores are copied
  M$scores <- matrix(NA, M$rows, M$comps)

  ## Expectation step for data without missing values
  Rx <- diag(M$comps) + M$tau * t(M$PA) %*% M$PA + M$SigW
  Rxinv <- solve(Rx)
  idx <- M$row_nomiss

  if (length(idx) == 0) {
    trS <- 0
    T <- 0
  } else {
    dy <- y[idx,, drop=FALSE] - .repmat(M$mean, length(idx), 1)
    x <- M$tau * Rxinv %*% t(M$PA) %*% t(dy)
    T <- t(dy) %*% t(x)
    trS <- sum(sum(dy * dy))

    ## Assign the scores for complete rows
    xTranspose <- t(x)
    for (i in 1:length(idx)) {
      M$scores[idx[i],] <- xTranspose[i,]
    }
  }
  ## Expectation step for incomplete data
  if( length(M$row_miss) > 0) {
    for(n in 1:length(M$row_miss)) {
      i  <- M$row_miss[n]
      dyo <- y[ i, !M$nans[i,], drop=FALSE] - M$mean[ !M$nans[i,], drop=FALSE]
      Wm <- M$PA[ M$nans[i,],, drop=FALSE]
      Wo <- M$PA[ !M$nans[i,],, drop=FALSE]
      Rxinv <- solve( (Rx - M$tau * t(Wm) %*% Wm))
      ex  <- M$tau * t(Wo) %*% t(dyo)
      x <- Rxinv %*% ex
      dym <- Wm %*% x
      dy <- y[i,, drop=FALSE]
      dy[ !M$nans[i,] ] <- t(dyo)
      dy[ M$nans[i,] ] <- t(dym)
      M$yest[i,] <- dy + M$mean
      T <- T + t(dy) %*% t(x)
      T[ M$nans[i,], ] <- T[ M$nans[i,],, drop=FALSE] + Wm %*% Rxinv
      trS <- trS + dy %*% t(dy) + sum(M$nans[i,]) / M$tau + 
        sum( diag(Wm %*% Rxinv %*% t(Wm)) ) 
      trS <- trS[1,1]
      ## Assign the scores for rows containing missing values
      M$scores[M$row_miss[n],] <- t(x)
    }
  }
  T <- T / M$rows
  trS <- trS / M$rows

  ## Maximation step
  Rxinv <- solve(Rx)
  Dw <- Rxinv + M$tau * t(T) %*% M$PA %*% Rxinv + 
    diag(M$alpha, nrow = length(M$alpha)) / M$rows
  Dwinv <- solve(Dw)
  M$PA <- T %*% Dwinv ## The new estimate of the principal axes (loadings)
  M$tau <- (M$cols + 2 * M$gtau0 / M$rows) /
    (trS - sum(diag(t(T) %*% M$PA)) +
     (M$mean %*% t(M$mean) * M$gmu0 + 2 * M$gtau0 / M$btau0) / M$rows)
  M$tau <- M$tau[1,1] ## convert to scalar
  M$SigW <- Dwinv * (M$cols / M$rows)
  M$alpha <- (2 * M$galpha0 + M$cols) / (M$tau * diag(t(M$PA) %*% M$PA) + 
                                         diag(M$SigW) + 2 * M$galpha0 / M$balpha0)

  return(M)
}



##' Model initialization for Bayesian PCA. This function is NOT
##' inteded to be run separately!
##'
##' The function calculates the initial Eigenvectors by use of SVD
##' from the complete rows.  The data structure M is created and
##' initial values are  assigned.
##' @title Initialize BPCA model
##' @param y numeric matrix containing missing values. Missing values
##' are denoted as 'NA'
##' @param components Number of components used for estimation
##' @return List containing
##' \item{rows}{Row number of input matrix}
##' \item{cols}{Column number of input matrix}
##' \item{comps}{Number of components to use}
##' \item{yest}{(working variable) current estimate of complete data}
##' \item{row_miss}{(Array) Indizes of rows containing missing values}
##' \item{row_nomiss}{(Array) Indices of complete rows (such with no
##' missing values)}
##' \item{nans}{Matrix of same size as input data. TRUE if \code{input == NA},
##' false otherwise}
##' \item{mean}{Column wise data mean}
##' \item{PA}{ (d x k) Estimated principal axes (eigenvectors,
##' loadings) The matrix ROWS are the vectors}
##' \item{tau}{Estimated precision of the residual error}
##' \item{scores}{ Estimated scores}
##' Further elements are: galpha0, balpha0, alpha, gmu0, btau0, gtau0,
##' SigW. These are working variables or constants.
##' @author Wolfram Stacklies
.BPCA_initmodel <- function(y, components) {
  ## Initialization, write static parameters to the central
  M <- NULL 
  M$rows <- nrow(y)
  M$cols <- ncol(y) 
  M$comps <- components ## Column number
  M$yest <- y ## Original data, NAs are set to 0 later on

  ## Find rows with missing values, etc...
  M$nans <- is.na(y)
  temp <- apply(M$nans, 1, sum)
  M$row_nomiss <- which(temp == 0)
  M$row_miss <- which(temp != 0)
  M$yest[M$nans] <- 0
  M$scores <- NULL

  ## Get the SVD of the complete rows
  covy <- cov(M$yest)
  values <- svd(covy, components, components)
  U <- values[[2]]
  S <- diag( values[[1]][1:components], nrow = components, ncol = components)
  V <- values[[3]]

  ## M$mean: column wise mean of the original data
  M$mean <- matrix(0, 1, M$cols)
  for(j in 1:M$cols) {
    idx <- which(!is.na(y[,j]))
    M$mean[j] <- mean(y[idx,j])
  }

  M$PA <- U %*% sqrt(S)
  M$tau <- 1 / ( sum(diag(covy)) - sum(diag(S)) )
  
  ## Constants etc
  taumax <- 1e10
  taumin <- 1e-10
  M$tau <- max( min(M$tau, taumax), taumin )

  M$galpha0 <- 1e-10
  M$balpha0 <- 1
  M$alpha <- (2 * M$galpha0 + M$cols) / (M$tau * diag(t(M$PA) %*% M$PA) +
                                         2 * M$galpha0 / M$balpha0)

  M$gmu0 <- 0.001

  M$btau0 <- 1
  M$gtau0 <- 1e-10
  M$SigW <- diag(components)
  return(M)
}

##' Creates a large matrix B consisting of an M-by-N tiling of copies
##' of A
##' @title Replicate and tile an array.
##' @param mat numeric matrix
##' @param M number of copies in vertical direction
##' @param N number of copies in horizontal direction
##' @return Matrix consiting of M-by-N tiling copies of input matrix
##' @author Wolfram Stacklies
.repmat <- function(mat, M, N) {

  ## Check if all input parameters are correct
  if(!all(M > 0, N > 0)) {
    stop("M and N must be > 0")
  }    
  
  ## Convert array to matrix
  ma <- mat
  if(!is.matrix(mat)) {
    ma <- matrix(mat, nrow=1)
  }

  rows <- nrow(ma)
  cols <- ncol(ma)
  replicate <- matrix(0, rows * M, cols * N)

  for (i in 1:M) {
    for (j in 1:N) {
      start_row <- (i - 1) * rows + 1
      end_row <- i * rows
      start_col <- (j - 1) * cols + 1
      end_col <- j * cols
      replicate[start_row:end_row, start_col:end_col] <- ma
    }
  }

  return(replicate)
}
