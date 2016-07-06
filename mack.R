######################## MACK'S MODEL ########################
library(ChainLadder)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
source("D:/MyProject/inteRn-repo/user-defined-functions.R")

### Import data and convert into triangle
data.tri <- read.csv(file = "D:/MyProject/inteRn-repo/dataset/taylor_ashe_incremental.txt", header = FALSE, sep = "\t") %>%
  as.matrix %>% as.triangle %>% incr2cum %>% set_colnames(1:ncol(.))

data.lag <- data.tri %>% 
  cbind(rep(NA, nrow(data.tri)), .) %>%
  extract(, 1:ncol(data.tri)) %>%
  set_colnames(1:ncol(data.tri))
names(dimnames(data.lag)) <- c("origin", "dev")

### Prepare data frame for regression model
data.df <- melt(data.tri) %>%
  join(melt(data.lag, value.name = "value.lag")) %>%
  filter(!(is.na(value) & is.na(value.lag))) %>%
  mutate(f = value/value.lag)

### Fit Mack model with GLM
fitMack <- glm(f ~ 0 + as.factor(dev), data = data.df, weights = value.lag)
predict(fitMack, newdata = transform(select(data.df, dev), factor))
predict(fitMack, dev = factor(c(1,2,3)))
select(data.df, dev) %>% transform(as.factor)
ddply(select(data.df, dev), dev, as.factor)
transform(select(data.df, dev), as.factor)
