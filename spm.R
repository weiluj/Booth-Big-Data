words<-read.csv("WordsFinal.csv",header=F)

words<-words[,1]

head(words)


# Read the word-day pairings

doc_word<-read.table("WordFreqFinal.csv",header=F)

# Create a sparse matrix


library(gamlr)

spm<-sparseMatrix(
  i=doc_word[,1],
  j=doc_word[,2],
  x=doc_word[,3],
  dimnames=list(id=1:ndays,words=words))
spm <- as.matrix(spm)
dim(spm)