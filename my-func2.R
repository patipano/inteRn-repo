### USER-DEFINED FUNCTIONS FOR MACK'S MODEL ###
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(ggplot2)
library(scales)
library(ggthemes)

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
getMackSigma <- function(.tri, .finalVariance = TRUE) {
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
    for (j in (cols - i + 2):cols) {
      theta <- sigma2[j] / bootLambda[j]
      alpha <- (bootLambda[j] ^ 2) *  diag.tri[i, j - 1] / (sigma2[j])
      diag.tri[i, j] <- rgamma(1, shape = alpha, scale = theta)
    }
  }
  return(diag.tri)
}
forecastMack(data.tri)
getMackSigma(data.tri)
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
  rows <- dim(.tri)[1]
  cols <- dim(.tri)[2]
  
  result1 <- laply(1:B,
    function(x) {
      forecast.tri <- forecastMack(.tri)
      diagData <- laply(1:rows, function(y) forecast.tri[y, cols - y + 1]) %>% as.matrix
      lastData <- forecast.tri[, cols] %>% as.matrix
      lastData - diagData
    }
  )
  
  result2 <- apply(result1, MARGIN = 2, FUN = mean) 
  return(list(reserve = result1, mean = result2))
}

##############################################################
data.tri

boo <- bootMack(data.tri, B = 2500)
boo$mean
head(boo$reserve, 20)
boom <- melt(boo$reserve)[-1]
names(boom) <- c("origin", "reserve")
head(boom)
gg <- ggplot(data = filter(boom, origin != 1), aes(x = reserve)) +
  geom_histogram(aes(y = ..density.., group = origin), col = "black") + 
  facet_wrap(~ origin, nrow = 4, scales = "free") +
  scale_x_continuous(labels = comma) + theme_few()
gg

boo2 <- BootChainLadder(data.tri, 5000, process.distr = "gamma")
boo2m <- melt(boo2$IBNR.ByOrigin[,1,])[, -2]
names(boo2m) <- c("origin", "reserve")
head(boo2m)
gg2 <- ggplot(data = filter(boo2m, reserve != 0), aes(x = reserve)) +
  geom_histogram(aes(group = origin), col = "black") + 
  facet_wrap(~ origin, nrow = 4, scales = "free") +
  scale_x_continuous(labels = comma) + theme_few()
gg2
summary(boo2)
boo$mean
filter(boo2m, reserve < 0)
