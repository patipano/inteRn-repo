####################### USER-DEFINED FUNCTIONS #######################
library(ChainLadder)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
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
triangularise(1:4) %>% as.vector


llply(1:10, function(x) sc[(10 - x + 1):10]) %>% unlist
### Diagnose residuals
diagnoseResid <- function(.tri, .resid) {
  df <- melt(.tri, na.rm = TRUE) %>% 
    join(melt(as.triangle(.resid), na.rm = TRUE, value.name = "resid")) %>%
    melt(id.vars = c("value", "resid"), value.name = "year")
  # return(df)
  gg <- ggplot(data = df, aes(x = year, y = resid)) +
    geom_point(aes(colour = variable)) +
    facet_wrap(~ variable)
  print(gg)
}
diagnoseResid(data.tri, getUnscaledResid(data.tri))
getScaledResid(data.tri) %>% as.triangle %>% melt(na.rm = TRUE)
getUnscaledResid(data.tri)

### Triangle representation for data frame
triRepresentation <- function(.df, .val) {
  result <- acast(.df, origin ~ dev, value.var = .val)
  names(dimnames(result)) <- c("origin", "dev")
  return(result)
}


############################################################################
### User-defined link function to handle negative values
quasipoisson <- function (link = "log")
  ## Amended by David Firth, 2003.01.16, at points labelled ###
  ## to cope with negative y values
  ##
  ## Computes Pearson X^2 rather than Poisson deviance
  ##
  ## Starting values are all equal to the global mean
{
  linktemp <- substitute(link)
  if (!is.character(linktemp)) {
    linktemp <- deparse(linktemp)
    if (linktemp == "link")
      linktemp <- eval(link)
  }
  if (any(linktemp == c("log", "identity", "sqrt")))
    stats <- make.link(linktemp)
  else stop(paste(linktemp, "link not available for poisson",
    "family; available links are", "\"identity\", \"log\" and 
    \"sqrt\""))
  variance <- function(mu) mu
  validmu <- function(mu) all(mu > 0)
  dev.resids <- function(y, mu, wt) wt*(y-mu)^2/mu   ###
  aic <- function(y, n, mu, wt, dev) NA
  initialize <- expression({
    n <- rep(1, nobs)
    mustart <- rep(mean(y), length(y))             ###
  })
  structure(list(family = "quasipoisson", link = linktemp,
    linkfun = stats$linkfun, linkinv = stats$linkinv, variance = 
      variance,
    dev.resids = dev.resids, aic = aic, mu.eta = stats$mu.eta,
    initialize = initialize, validmu = validmu, valideta = 
      stats$valideta),
    class = "family")
}
