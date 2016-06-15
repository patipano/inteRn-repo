########################################################
###### Bootstrapping Mack Distribution-free Model ######
########################################################
library(ChainLadder)
library(reshape2)
library(boot)
library(ggplot2)

url <- "D:/MyProject/inteRn-repo/dataset/richardverrall.txt"
data <- read.table(file = url, header = FALSE, sep = "\t")
rownames(data) <- 2000:2009
colnames(data) <- 1:10
data.tri <- incr2cum(as.triangle(as.matrix(data)))
plot(data.tri)

### Reshape data into long format
data.df <- melt(data.tri, na.rm = TRUE, varnames = c("origin", "dev"), value.name = "claim")

gg <- ggplot(data = data.df, aes(x = dev, y = claim, group = as.factor(origin), colour = as.factor(origin)))
gg <- gg + geom_line()
gg
