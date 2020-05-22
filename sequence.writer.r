## Read in the data and references ##
## Set your working directory to Malaria if you're using GitHub Desktop ##
#library("seqinr")
#reference.genome <- read.fasta("Data/LR605956.fasta", as.string=TRUE, seqonly=TRUE)
#ref <- read.table("Data/ref.tab", header=TRUE, sep="\t")
#non <- read.table("Data/non.tab", header=TRUE, sep="\t")
#var <- read.table("Data/var.tab", header=TRUE, sep="\t")

#g.c <- unlist(strsplit(unlist(reference.genome),"")) # converts to vector


## Sequence Writer Script ##

sequences <- matrix(g.c, nrow = length(g.c), ncol = length(ref[1,])) # make as many copies of the reference genome as we have samples



test.places <- c(1,87,166,363) 
#,562,694,1685,2035,3041,4256,4348, 4413, 4862,5002, 5030, 5123,
                 #5474, 5738, 5755, 5769, 6019,6150, 6516, 6558, 6583, 6822, 6892, 6926, 7028)
# Mauritania, Gambia, Guinea, Gambia, Kenya, Thailand, Tanzania, Ghana, Cambodia, Indonesia,
# Burkina Faso, Mali, Papua New Guinea, Peru, Bangladesh, Malawi, Vietnam, Colombia, Uganda, 
# Myanmar, Laos, Congo DR, Nigeria, Madagascar, Camaroon, Ivory Coast, Ethiopia, Benin, Senegal
# 3039-3041 has weird entries?? Errors??

sequences.test <- matrix(g.c, nrow = length(g.c), ncol = length(test.places))


## This Code ONLY does SNPs ##

for(i in 1:length(test.places)) # cycle thru our test places
{
  for (j in 1:length(ref[,1])) # cycle through all positions on genome
  {
    if((nchar(as.character(var[j,3])) == 1) && (nchar(as.character(var[j,4])) == 1)) # restricts us to just SNPs
    {
      pos <- ref[j,2] # which nucleotide does the position start on?
      if(ref[j,test.places[i]+2] <= non[j,test.places[i]+2]) # does the alternative outweigh the reference?
      {
        sequences.test[pos,i] <- as.character(var[j,4]) # change the "pos" position of the ith genome to the alt nucleotide (truncated)
      } # else keep the reference
    }
  }
}

sample.names <- names(ref)
print(sample.names[test.places[2]+2])

i <- 1
this.sample <- sample.names[test.places[i]+2]
out.string <- paste('>',this.sample,sep="")
#out.seq <- sequences.test[,i]
file.name <- paste(this.sample,".fasta",sep="")
out.seq <- paste(sequences.test[,i], sep="", collapse="")
write.table(file=file.name,x=out.string,col.names=FALSE,row.names=FALSE,sep="",quote=FALSE)
write.table(file=file.name,x=out.seq,col.names=FALSE,row.names=FALSE,sep="",quote=FALSE,append=TRUE)




