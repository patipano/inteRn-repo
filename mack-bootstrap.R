########################################################
###### Bootstrapping Mack Distribution-free Model ######
########################################################
library(ChainLadder)
library(reshape2)
library(boot)
library(ggplot2)
library(dplyr)

url <- "D:/MyProject/inteRn-repo/dataset/richardverrall.txt"
data <- read.table(file = url, header = FALSE, sep = "\t")
rownames(data) <- 2000:2009
colnames(data) <- 1:10
data.tri <- incr2cum(as.triangle(as.matrix(data)))
plot(data.tri)

### Reshape data into long format
data.df <- melt(data.tri, na.rm = TRUE, varnames = c("origin", "dev"), value.name = "claim")

### INCOMPLETE Plot
gg <- ggplot(data = data.df, aes(x = dev, y = claim, group = as.factor(origin), colour = as.factor(origin)))
gg <- gg + geom_line(size = 1) + geom_label(aes(label = origin), alpha = .5)
gg

modMack <- MackChainLadder(Triangle = data.tri)
summary(fitMack)
mack.sigma <- (fitMack$sigma)

### Calcualate age-to-age factors
ataFactor <- attr(ata(data.tri), "vwtd")
data.tri
