ref <- read.table("Data/ref.tab", header=TRUE, sep="\t")
non <- read.table("Data/non.tab", header=TRUE, sep="\t")
var <- read.table("Data/var.tab", header=TRUE, sep="\t")

ref.cut <- ref[,-c(1,2)]
non.cut <- non[,-c(1,2)]

samples.counts <- length(ref[1,])
pos.counts <- length(ref[,1])

coverage <- ref.cut + non.cut # generates coverage profile
mixture <- non.cut/coverage # generate mixture profile
mixture[is.na(mixture)] <- -1 # tag each location of zero coverage
