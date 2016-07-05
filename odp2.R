######################## OVER-DISPERSED POISSON MODEL ########################
library(ChainLadder)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
source("D:/MyProject/inteRn-repo/user-defined-functions.R")

### Import data and convert into triangle
data.tri <- read.csv(file = "D:/MyProject/inteRn-repo/dataset/taylor_ashe_incremental.txt", header = FALSE, sep = "\t") %>%
  as.matrix %>% as.triangle %>% incr2cum %>% set_colnames(1:ncol(.))

### Calculate unscaled Pearson residuals
getUnscaledResid <- function(.tri) {
  data.incr <- .tri %>% cum2incr
  data.df <- melt(data.incr, na.rm = TRUE)
  
  fitODP <- glm(value ~ as.factor(origin) + as.factor(dev), data = data.df, family = quasipoisson)
  data.fit <- fitted(fitODP) %>% convertVec2Tri(nrow(.tri), ncol(.tri)) %>% t
  result <- (data.incr - data.fit)/sqrt(data.fit)
  return(result)
}

### Calculate (non-constant) scale parameters
getScale <- function(.tri) {
  resid <- getUnscaledResid(.tri)
  n <- sum(!is.na(data.tri))
  p <- nrow(.tri) + ncol(.tri) - 1
  resid <- resid*sqrt(n/(n-p)) # Bias adjustment
  
  result <- colSums(resid^2, na.rm = TRUE) %>%
    divide_by(colSums(!is.na(resid)))
  result[ncol(.tri)] <- min(result[-ncol(.tri)])
  return(result)
}

### Calculate scaled Pearson residuals
getScaledResid <- function(.tri) {
  resid <- getUnscaledResid(.tri)
  n <- sum(!is.na(data.tri))
  p <- nrow(.tri) + ncol(.tri) - 1
  resid <- resid*sqrt(n/(n-p)) # Bias adjustment
  
  scale <- colSums(resid^2, na.rm = TRUE) %>%
    divide_by(colSums(!is.na(resid)))
  
  result <- laply(1:ncol(.tri), function(x) resid[, x]/sqrt(scale[x])) %>% t
  
  result[1, ncol(.tri)] <- 0L
  result[nrow(.tri), 1] <- 0L
  return(result)
}
getScaledResid(data.tri) %>% as.triangle() %>% plot

### Bootstrap
bootstrapODP <- function(.tri, B = 1) {
  n <- sum(!is.na(.tri))
  nPred <- nrow(.tri)*ncol(.tri) - n
  residList <- getScaledResid(.tri) %>% as.vector %>% extract(!is.na(.) & is_greater_than(abs(.), 0))
  scale <- getScale(.tri)
  scale2 <- llply(1:nrow(.tri), function(x) sc[(ncol(.tri) - x + 1):ncol(.tri)]) %>% unlist
  
  data.incr <- .tri %>% cum2incr
  data.df <- melt(data.incr, na.rm = TRUE)
  pseudoF.df <- melt(data.tri) %>% filter(is.na(value)) %>% select(1:2) %>% arrange(origin) # Blank data frame for prediction
  fitODP <- glm(value ~ as.factor(origin) + as.factor(dev), data = data.df, family = poisson)
  data.fit <- fitted(fitODP) %>% convertVec2Tri(nrow(.tri), ncol(.tri)) %>% t
  
  ### Bootstrap process
  result <- llply(1:B, function(x) {
    bootResid <- sample(residList, n, replace = TRUE) %>%
      convertVec2Tri(ncol(.tri), nrow(.tri))
    pseudo.df <- laply(1:ncol(.tri), function(x) {
      bootResid[, x]*sqrt(scale[x]*data.fit[, x]) + data.fit[, x]
    }) %>% t %>% as.triangle %>% melt(na.rm = TRUE)
    pseudo.fit <- glm(value ~ as.factor(origin) + as.factor(dev), data = pseudo.df, family = quasipoisson())
    pseudo.pred <-  exp(predict(pseudo.fit, pseudoF.df))
    ### Add process error (Gamma)
    complete.tri <- rgamma(n = nPred, shape = pseudo.pred/scale2, scale = scale2) %>%
      cbind(pseudoF.df, value = .) %>%
      rbind(filter(pseudo.df, origin != 1)) %>%
      acast(origin ~ dev) %>% as.matrix %>% incr2cum
    ### Calculate reserves
    laply(1:nrow(complete.tri), function(x) complete.tri[x, ncol(.tri)] - complete.tri[x, ncol(.tri) - x]) %>%
      matrix %>%
      set_colnames("ultimate") %>%
      set_rownames(2:nrow(.tri))
  }) %>% array(unlist(.), dim = c(nrow(.[[1]]), ncol(.[[1]]), B))
  return(result)
}

boot <- bootstrapODP(data.tri, B = 2)
boot %>% unlist %>% array(dim = c(9, 1, 2))

apply(boot, mean) # Mean
matrix(sc) %>% set_rownames(1:10) %>% set_colnames("ultimate")
melt(data.tri) %>% filter(is.na(value)) %>% select(1:2) %>% arrange(origin)
getScaledResid(data.tri)
