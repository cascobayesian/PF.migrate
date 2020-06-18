# ref <- read.table("ref.clean.csv", header=TRUE, sep=",")
# non <- read.table("non.clean.csv", header=TRUE, sep=",")
# 
# samples.counts <- length(ref[1,])
# pos.counts <- length(ref[,1])

k = 2 # Hard set K

p.hat <- rep(0, pos.counts)
  for(j in 1:pos.counts)
  {
    p.hat[j]<- sum(non[j,])/sum(non[j,] + ref[j,]) # non-reference allele frequency for all SNPs j
  }
  
v = rexp(1,0.2) # 5 or 0.2???




SNP.llk <- function(n,r,q,v){
  prob <- choose((n+r), n)*beta( (n+(q*v)), (r+((1-q)*v)) )
  prob <- prob/beta((q*v),(v*(1-q)))
  return(prob)
}

lambda <- function(p,c,k){
  return((p^c)*(1-p)^((2^k)-c)) 
}

calc.llk <- function(m,n,r,q,v,p,c,k)
{
  prod(sum(lambda(p,c,k)*SNP.llk(n,r,q,v)))
  
  
  
  
}