### USER-DEFINED FUNCTIONS FOR MACK'S MODEL ###
library(plyr)
library(magrittr)

### Convert vector to triangle
convertVec2Tri <- function(.vec, .rows, .cols) {
  collection <- c()
  for (x in 1:.rows) {
    collection <- c(collection, .vec[1:(.cols - x + 1)], rep(NA, x - 1))
    .vec <- .vec[-(1:(.cols - x + 1))]
  }
  return(matrix(collection, .rows, .cols, byrow = TRUE))
}

### Triangularise vector
triangularise <- function(.vec) {
  rows <- cols <- length(.vec)
  result <- llply(1:rows, function(x) .vec[1:(cols - x + 1)]) %>%
    unlist %>% convertVec2Tri(rows, cols)
  return(result)
}

### Get weights triangle for Mack's model "w"
getWeights <- function(.tri) {
  rows <- dim(.tri)[1]
  cols <- dim(.tri)[2]
  result <- llply(1:rows, function(x) c(NA, .tri[x, 1:(cols - x)])) %>%
    unlist %>% convertVec2Tri(rows, cols)
  return(result)
}

### Calculate volume-weighted average of age-to-age factor "lambda"
getMackLambda <- function(.tri) {
  w <- getWeights(.tri)
  f <- .tri / w
  
  result <- colSums(f*w, na.rm = TRUE) / colSums(w, na.rm = TRUE)
  return(result)
}
getMackLambda(data.tri)

### Backward fit triangle with lambda (VWAs)
fitBackward <- function(.tri, .lambda) {
  rows <- dim(.tri)[1]
  cols <- dim(.tri)[2]
  
  diagData.tri <- llply(1:rows, function(x) rep(.tri[x, cols - x + 1], 10 - x + 1)) %>%
    unlist %>% convertVec2Tri(rows, cols)
  
  lambda <- .lambda[-1]
  n <- length(lambda)
  lambda.cumul <- llply(1:n, function(x) c(rev(cumprod(rev(lambda[1:(n - x + 1)]))), 1)) %>%
    unlist %>% c(1) %>% convertVec2Tri(rows, cols)
  
  result <- diagData.tri / lambda.cumul
  return(result)
}
fitBackward(data.tri, getMackLambda(data.tri))

### Calculate unscaled Pearson residuals
getResidUnscaled <- function(.tri) {
  w <- getWeights(.tri)
  f <- .tri / w
  lambda <- getMackLambda(.tri) %>% triangularise
  
  result <- sqrt(w) * (f - lambda)
  return(result)
}
getResidUnscaled(data.tri)

### Calculate square-root of scale parameters for each period "sigma"
getMackSigma <- function(.tri, .finalVariance = FALSE) {
  resid <- getResidUnscaled(.tri)
  result <- colSums(resid ^ 2, na.rm = TRUE)
  result <- llply(1:dim(.tri)[2],
    function(x) result[x] / (sum(is.finite(resid[,x]), na.rm = TRUE) - 1)
  ) %>% unlist
  
  if (!.finalVariance)
    result[length(result)] <- NA
  else
    result[length(result)] <- min(rev(result)[2:3])
  
  return(sqrt(result))
}
getMackSigma(data.tri, TRUE)

### Calculate scaled Pearson residuals, used in resampling
getResidScaled <- function(.tri, .finalVariance = FALSE) {
  sigma.tri <- triangularise(getMackSigma(.tri, .finalVariance))
  result <- getResidUnscaled(.tri) / sigma.tri
  return(result)
}
getResidScaled(data.tri)

##########################################################################

### Resample scaled residuals
resampleResid <- function(.tri, .finalVariance = TRUE) {
  resid.vec <- getResidScaled(.tri, .finalVariance) %>%
    as.vector %>%
    extract(is.finite(.))
  result <- sample(resid.vec, size = length(resid.vec) + 1, replace = TRUE) %>%
    convertVec2Tri(dim(.tri)[1] - 1, dim(.tri)[2] - 1) %>%
    rbind(rep(NA, dim(.tri)[2] - 1)) %>%
    cbind(rep(NA, dim(.tri)[1]), .)
  return(result)
}
resampleResid(data.tri)

### Recover (pseudo) data from scaled residuals
recoverData <- function(.tri, .resid, .finalVariance = TRUE) {
  w <- getWeights(.tri)
  sigma <- triangularise(getMackSigma(.tri, .finalVariance))
  lambda <- triangularise(getMackLambda(.tri))
  
  f <- .resid * sigma / sqrt(w) + lambda
  lambda.fit <- colSums(w * f, na.rm = TRUE) / colSums(w, na.rm = TRUE)
  result <- fitBackward(.tri, lambda.fit)
  return(result)
}
recoverData(data.tri, resampleResid(data.tri))
getMackLambda(data.tri)

### Forecast triangle with process error
forecastMack <- function(.tri) {
  rows <- dim(.tri)[1]
  cols <- dim(.tri)[2]
  bootResid <- resampleResid(.tri, TRUE)
  bootTri <- recoverData(.tri, bootResid)
  bootLambda <- getMackLambda(bootTri)
  sigma2 <- getMackSigma(.tri) ^ 2
  
  diag.tri <- llply(1:rows, function(x) c(rep(NA, cols - x), .tri[x, cols - x + 1])) %>%
    unlist %>% convertVec2Tri(rows, cols)
  
  for (i in 2:rows) {
    for (j in cols - i + 2) {
      theta <- sigma2[j] / lambda[j]
      alpha <- lambda[j]
      diag.tri[i, j] <- 
    }
  }
  return(diag.tri)
}
forecastMack(data.tri)
data.tri
# ### Calculate reserves for each origin period
# getReserve <- function(.tri, .lambda) {
#   rows <- dim(.tri)[1]
#   cols <- dim(.tri)[2]
#   lambda.cumul <- rev(cumprod(rev(c(.lambda[-1], 1))))
#   
#   diagData <- laply(1:rows, function(x) .tri[x, cols - x + 1]) %>% rev
#   projData <- diagData*lambda.cumul
#   result <- as.matrix(rev(projData - diagData))
#   
#   colnames(result) <- "Reserve"
#   rownames(result) <- 1:rows
#   return(result)
# }
# getReserve(data.tri, getMackLambda(data.tri))

##########################################################################

bootMack <- function(.tri, B = 5) {
  llply(1:B,
    function(x) {
      bootResid.tri <- resampleResid(.tri, .finalVariance = TRUE)
      bootData.tri <- recoverData(.tri, resid.tri, .finalVariance = TRUE)
      bootLambda <- getMackLambda(bootData.tri)
      bootReserve <- getReserve(bootData.tri,
      
    } 
  )
}
