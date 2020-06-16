ref <- read.table("ref.clean.csv", header=TRUE, sep=",")
non <- read.table("non.clean.csv", header=TRUE, sep=",")



M <- length(ref)
pdf('ref vs. non',height=12,width=16)

par(mfrow=c(10,10),mar=rep(0.1,4))

for(j in 1:M)
{
  plot(ref[,j], non[,j],xlab="",ylab="",type='p',axes=FALSE)
  text(x = 15,y=20,label=j,cex=1.5,col="blue")
  box()
}

dev.off()