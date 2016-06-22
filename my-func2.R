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

### Get weights triangle for Mack's model (w)
getWeights <- function(.tri) {
  rows <- dim(.tri)[1]
  cols <- dim(.tri)[2]
  result <- llply(1:rows, function(x) c(NA, .tri[x, 1:(cols - x)])) %>%
    unlist %>% convertVec2Tri(rows, cols)
  return(result)
}

### Calculate volume-weighted average of age-to-age factor (lambda)
getMackLambda <- function(.tri) {
  w <- getWeights(.tri)
  f <- .tri / w
  
  result <- colSums(f*w, na.rm = TRUE) / colSums(w, na.rm = TRUE)
  return(result)
}
getMackLambda(data.tri)

### Calculate unscaled Pearson residuals
getResidUnscaled <- function(.tri) {
  w <- getWeights(.tri)
  f <- .tri / w
  lambda <- getMackLambda(.tri) %>% triangularise
  
  result <- sqrt(w) * (f - lambda)
  return(result)
}

### Calculate square-root of scale parameters for each period (sigma)
getMackSigma <- function(.tri, .finalVariance = FALSE) {
  resid <- getResidUnscaled(.tri)
  result <- colSums(resid ^ 2, na.rm = TRUE)
  result <- llply(1:dim(.tri)[2],
    function(x) result[x] / (sum(is.finite(resid[,x]), na.rm = TRUE) - 1)
  ) %>% unlist
  return(sqrt(result))
}

### Calculate scaled Pearson residuals, used in resampling
getResidualScaled <- function(.tri, .finalVariance = FALSE) {
  sigma.tri <- triangularise(getMackSigma(.tri, .finalVariance = .finalVariance))
  result <- getResidUnscaled(.tri) / sigma.tri
  return(result)
}
