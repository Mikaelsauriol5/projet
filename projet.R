library("tm")
BiocManager::install("Biobase")
install.packages('NMF')
library(NMF)


textes <- Corpus(DirSource("Textes", encoding = "UTF-8"), readerControl = list(language = "eng"))

textes <- tm_map(textes, stripWhitespace)
textes <- tm_map(textes, tolower)
textes <- tm_map(textes, removePunctuation)
textes <- tm_map(textes, removeNumbers)
textes <- tm_map(textes, removeWords, stopwords("english"))

length(stopwords("english"))
stopwords("english")
tdm <- TermDocumentMatrix(textes)
nbDoc <- length(textes)

X <- tdm

#LSA
R <- svd(X)
U <- R$u
V <- R$v
S <- diag(R$d)

plot(V[,1], V[,2], main= "Graph of documents", pch=20, col=1)
text(V[,1], V[,2], seq(nbDoc), pos=2, col="red")

VS <- V%*%S

matrice <- as.matrix(rbind(tdm))
#NMF
P <- nmf(matrice, 2, method='brunet', nrun=10, seed=123456)

profplot(P)
