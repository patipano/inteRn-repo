### NEW ODP ###
library(ChainLadder)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(data.table)
library(boot)
install.packages("ChainLadder")

data.tri <- read.csv(file = "D:/MyProject/inteRn-repo/dataset/taylor_ashe_incremental.txt", header = FALSE, sep = "\t") %>%
  as.matrix %>% as.triangle %>% incr2cum %>% set_colnames(1:ncol(.))

data.dt <- data.tri %>% cum2incr %>% melt %>% as.data.table %>% setkey(origin, dev)

glm(value ~ as.factor(origin) + as.factor(dev) + 0, data = data.dt, family = quasipoisson, na.action = na.exclude) %>%
  predict %>% exp %>%
  set(data.dt, j = "fitted", value = .)

data.dt[, unscaled := (value - fitted)/sqrt(fitted)]

data.dt[, s.param := sum(unscaled^2, na.rm = TRUE)/(sum(!is.na(unscaled), na.rm = TRUE) - 1), by = dev]

data.dt[!is.finite(s.param), s.param := min(data.dt[is.finite(s.param), s.param])]

data.dt[, scaled := unscaled/sqrt(s.param)]

data.dt[!is.na(value)]
data.dt
is.zero <- function(x) abs(x) < 0.000000001
# BOOTSTRAP ---------------------------------------------------------------

### .dt must contain no NA in "scaled" column
statistic <- function(.dt, i) {
  resid <- .dt[i, scaled]
  n.zero <- sum(is.zero(resid))
  zero.replace <- sample(data.dt[!is.zero(scaled), scaled], n.zero, replace = TRUE)
  resid[is.zero(resid)] <- zero.replace
  boot.dt <- data.dt[!is.na(value)][1:length(resid), .(origin, dev, value, fitted, s.param, scaled = resid)]
  boot.dt[, value.boot := scaled*sqrt(s.param*fitted) + fitted]

  model.boot <- glm(value.boot ~ as.factor(origin) + as.factor(dev) + 0, data = boot.dt, family = "quasipoisson")
  next.dt <- data.dt[is.na(value), .(origin, dev, s.param)]
  pred <- predict(model.boot, next.dt) %>% exp
  next.dt[, value.fitted := pred]
  
}

data.dt
statistic(data.dt[!is.na(scaled)], c(1:55))
# boot(data.dt[!is.na(scaled)], statistic, 2) %>% summary
