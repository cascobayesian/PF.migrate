ref <- read.table("Data/ref.tab", header=TRUE, sep="\t")
non <- read.table("Data/non.tab", header=TRUE, sep="\t")
var <- read.table("Data/var.tab", header=TRUE, sep="\t")

ref.cut <- ref[,-c(1,2)]
non.cut <- non[,-c(1,2)]

samples.counts <- length(ref[1,])
pos.counts <- length(ref[,1])

coverage <- ref.cut + non.cut # generates coverage profile

which(coverage == 0, arr.ind = TRUE) # this finds the indicies of zero coverage

# The following is too large to run, and is full of divide by zero errors
mixture <- matrix(0, ncol = samples.counts, nrow = pos.counts)
for(i in 1:samples.counts)
{
  mixture[,i] <- non.cut[,i]/coverage[,i]
} # generates mixture profile
