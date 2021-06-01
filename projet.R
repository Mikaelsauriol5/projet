library("tm")
BiocManager::install("Biobase")
install.packages('NMF')
library(NMF)
library(lsa)


textes <- Corpus(DirSource("Textes", encoding = "UTF-8"), readerControl = list(language = "eng"))

textes <- tm_map(textes, stripWhitespace)
textes <- tm_map(textes, tolower)
textes <- tm_map(textes, removePunctuation)
textes <- tm_map(textes, removeNumbers)
textes <- tm_map(textes, removeWords, stopwords("english"))
textes <- tm_map(textes, stemDocument, language = "english")

length(stopwords("english"))
stopwords("english")
tdm <- TermDocumentMatrix(textes)
nbDoc <- length(textes)

X <- tdm

#LSA
R <- lsa(X,2)
U <- R$tk
V <- R$dk
S <- diag(R$sk)

plot(V[,1], V[,2], main= "Graph of documents", pch=20, col=1)
text(V[,1], V[,2], seq(nbDoc), pos=2, col="red")

VS <- U%*%S%*%t(V)

E <- sum((VS - X)^2)

matrice <- as.matrix(rbind(tdm))
#NMF
P <- nmf(matrice, 2)
W <- P@fit@W
H <- P@fit@H

plot(H[1,], H[2,], main= "Graph of documents", pch=20, col=1)
text(H[1,], H[2,], seq(nbDoc), pos=2, col="red")

WH <- W%*%H
Ensa <- sum((WH - X)^2)
