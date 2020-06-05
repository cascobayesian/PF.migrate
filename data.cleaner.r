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

M <- 300
pdf('coverages_raw.pdf',height=12,width=16)

par(mfrow=c(10,10),mar=rep(0.1,4))

for(j in 1:M)
{
  plot(coverage[,j],xlab="",ylab="",type='p',axes=FALSE)
  text(x = 15,y=20,label=j,cex=1.5,col="blue")
  box()
}

dev.off()


index <- which(conc.stats[1,] <10)

M <- 300
pdf('coverages_pruned.pdf',height=12,width=16)

par(mfrow=c(10,10),mar=rep(0.1,4))

for(j in index[1:300])
{
  plot(coverage[,j],xlab="",ylab="",type='p',axes=FALSE)
  text(x = 15,y=20,label=j,cex=1.5,col="blue")
  box()
}

dev.off()

## the following code is incomplete
psm <- matrix(0, nrow = pos.counts, ncol= samples.counts)
ssm <- matrix(0, nrow = pos.counts, ncol= samples.counts)

for (j in 1:pos.counts) # one for each positions
{
  denom <- as.numeric(quantile(coverage[j,], 0.5))
  for (i in 1:samples.counts)
  {
   psm[j,i] <- coverage[j,i]/denom # position-specific median
   ssm[j,i] <- coverage[j,i]/as.numeric(quantile(coverage[,i], 0.5)) # sample-specific median
  }
}

M <- 300 # pos.counts
pdf('Position_medians.pdf',height=12,width=16)

par(mfrow=c(10,10),mar=rep(0.1,4))

for(j in 1:pos.counts)
{

  hist(psm[j,],xlab="",ylab="",axes=FALSE)
  text(x = 15,y=20,label=j,cex=1.5,col="blue")
  box()
}

dev.off()

M <- 300 # pos.counts
pdf('Sample_medians.pdf',height=12,width=16)

par(mfrow=c(10,10),mar=rep(0.1,4))

for(j in 1:pos.counts)
{
  
  hist(ssm[j,],xlab="",ylab="",axes=FALSE)
  text(x = 15,y=20,label=j,cex=1.5,col="blue")
  box()
}

dev.off()
