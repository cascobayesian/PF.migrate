# ref <- read.table("Data/ref.tab", header=TRUE, sep="\t")
# non <- read.table("Data/non.tab", header=TRUE, sep="\t")
# var <- read.table("Data/var.tab", header=TRUE, sep="\t")
# 
# ref.cut <- ref[,-c(1,2)]
# non.cut <- non[,-c(1,2)]

samples.counts <- length(ref.cut[1,])
pos.counts <- length(ref.cut[,1])

coverage <- ref.cut + non.cut # generates coverage profile
mixture <- non.cut/coverage # generate mixture profile
# mixture[is.na(mixture)] <- -1 # tag each location of zero coverage


## PART 1: Confidence Intervals ##

mixture.total.CI <- quantile(mixture, probs = c(0.05, 0.5, 0.95), na.rm=TRUE)
coverage.total.CI <- quantile(coverage, probs = c(0.05, 0.5, 0.95), na.rm=TRUE)

mixture.CIs <- matrix(0, nrow = samples.counts, ncol = 3)
coverage.CIs <- matrix(0, nrow = samples.counts, ncol = 3)

for (k in 1:samples.counts)
{
  coverage.CIs[k,] <- quantile((sort(coverage[,k])), probs = c(0.05, 0.5, 0.95), na.rm=TRUE)
  mixture.CIs[k,] <- quantile((sort(mixture[,k])), probs = c(0.05, 0.5, 0.95), na.rm=TRUE)
}

## PART 2: Assessing the Clumpiness of Data ##
## Clumpiness Functions ##
how.concentrated <- function(vec)
{
  max.cov <- max(table(sort(vec))/pos.counts)
  return(max.cov*100)
}

Mode <- function(x) 
{
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Visualizing ##
conc.stats <- matrix(0, ncol = samples.counts, nrow = 2)
for (j in 1:samples.counts)
{
  conc.stats[1,j] <- how.concentrated(coverage[,j])
  conc.stats[2,j] <- Mode(coverage[,j])
}

hist(conc.stats[2,], breaks=500) # here's the distribution of coverage modes. the peak at 40 is concerning
hist(conc.stats[2,], breaks=500, xlim=c(0,200)) # more zoomed in version of above: Poisson(s)???
plot(coverage[,622]) # this is what those really high-coverage samples look like
