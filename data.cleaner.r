# ref <- read.table("Data/ref.tab", header=TRUE, sep="\t")
# non <- read.table("Data/non.tab", header=TRUE, sep="\t")
# var <- read.table("Data/var.tab", header=TRUE, sep="\t")
# 
# ref.cut <- ref[,-c(1,2)]
# non.cut <- non[,-c(1,2)]

samples.counts <- length(ref[1,])
pos.counts <- length(ref[,1])

coverage <- ref.cut + non.cut # generates coverage profile
mixture <- non.cut/coverage # generate mixture profile
# mixture[is.na(mixture)] <- -1 # tag each location of zero coverage

## Sample Specific 99% Confidence Intervals (of mixture data) ##

alpha = 0.01
data.vec <- rep(0,0)
confidence.interval <- matrix(0, ncol=2, nrow=samples.counts)

for(i in 1:3500)
{
  mu <- mean(mixture[,i], na.rm=TRUE)
  N <- pos.counts - sum(is.na(mixture[,i]))
  std.error <- sd(mixture[,i], na.rm=TRUE)/sqrt(N)
  z.lower <- qnorm((alpha/2), mean = mu, sd = std.error)
  z.upper <- qnorm(1 - (alpha/2), mean = mu, sd = std.error)
  confidence.interval[i,] <- c(z.lower, z.upper)
  data.vec <- c(data.vec, mixture[,i])
}

## Aggregate Confidence Interval ##
  mu.dataset <- mean(data.vec, na.rm=TRUE)
  N.dataset <- length(data.vec) - sum(is.na(data.vec))
  std.error.dataset <- sd(data.vec, na.rm=TRUE)/sqrt(N.dataset)
  z.lower <- qnorm((alpha/2), mean = mu.dataset, sd = std.error.dataset)
  z.upper <- qnorm(1-(alpha/2), mean = mu.dataset, sd = std.error.dataset)
  confidence.interval.total <- c(z.lower, z.upper)
