source("http://bioconductor.org/biocLite.R")
biocLite("yeastCC")
library('yeastCC')
yeastCC
# create a matrix of genes by samples
gene.matrix <- exprs('yeastCC')
gene.matrix = exprs('yeastCC')
gene.matrix = exprs(yeastCC)
head(gene.matrix)
colnames(gene.matrix)
gene.column <- colnames(gene.matrix)
grep(aplha$gene.column)
grep(aplha$gene.column)$gene.matrix
alpha = gene.matrix[,grep("alpha",colnames(gene.matrix))]
mean(alpha[,1])
> contains.NAs = alpha[!complete.cases(alpha),]
> image(is.na(contains.NAs))
contains.NAs = alpha[!complete.cases(alpha),]
image(is.na(contains.NAs))
contains.NAs
allNAs = apply(is.na(alpha), MARGIN = 1, FUN = all)
alpha = alpha[!allNAs,]
sum(allNAs)
vars = apply(alpha, MARGIN = 2, FUN = var, na.rm = T)
plot(seq(0,119,7),vars,type="b",col="red") #"b" combines points with lines
# cluster genes based on their expression profile over time
# remove genes that do not show significant variation
# require that the variance of each gene over all time points be at least 0.4
?var
var(alpha[3,],na.rm=T)
vars.rows = apply(alpha, MARGIN = 1, FUN = var, na.rm = T) #Margin=1 use rows; margin=2 use columns
vars.rows = apply(alpha, MARGIN = 1, FUN = function(x) var(x, na.rm = T)) #Margin=1 use rows; margin=2 use columns
sum(vars.rows >= 0.4)
alpha.hi = alpha[vars.rows > 0.4,]
dim(alpha.hi)
heatmap(alpha.hi ,Colv = NA)
alpha.dist = dist(alpha.hi)
alpha.hclust = hclust(alpha.dist)
plot(alpha.hclust)
# cut the dendrogram into 5 clusters, use: cutree()
alpha.clusters = cutree(alpha.hclust, k = 5)
table(alpha.clusters
alpha.clusters = cutree(alpha.hclust, k = 5)
table(alpha.clusters)
cluster4 = names(alpha.clusters)[alpha.clusters == 4]
dist(alpha.hi[1:10,])
dist(alpha.hi[cluster4[1:10],])
load("hexamerCounts.RData")
hexamerCounts[1:10,1:10]
