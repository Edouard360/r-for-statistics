#aggregate(listToAggregate, referenceList, FUN)

#sweep(data, 1(line) or 2(column), STATS(colMeans(df)))

#aggregate(x, by, FUN, ..., simplify = TRUE, drop = TRUE)

#aggregate(x = testDF, by = list(by1), FUN = "mean")
#aggregate(x = testDF, by = list(by1,by2), FUN = "mean")
#But duplicates the first column of testDF
#aggregate( testDF, by = list(testDF[,1]), FUN = "mean")

#l[l==6]=6
#l[l>6]

Expert <- read.table("http://factominer.free.fr/docs/Expert_wine.csv",header = TRUE, sep = ";", row.names = 1)
res.pca <- PCA(Expert, scale = T, quanti.sup = 29:30, quali.sup = 1)
summary(res.pca)
barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = 1:nrow(res.pca$eig))
plot.PCA(res.pca, habillage = 1)
res.pca$ind$coord; res.pca$ind$cos2; res.pca$ind$contrib
plot.PCA(res.pca, axes = c(3, 4), habillage = 1)
dimdesc(res.pca)
plotellipses(res.pca, 1)
write.infile(res.pca, file = "my_FactoMineR_results.csv")

#############################

library(mvtnorm)
Z <- rmvnorm(n = 200, rep(0, 3), sigma = diag(3))
X1 <- Z[, 1]
X2 <- X1 + 0.001*Z[, 2]
X3 <- 10*Z[, 3]

##########--1--###########
#B - 1
#C - 3
#A - 4
#D - 2

library(FactoMineR)
data(decathlon)

numDec <- decathlon[,1:12]
moyD <- colMeans(numDec)
sdD <- apply(decathlon[,1:12],2,sd)

numDec <- sweep(numDec,2,moyD,FUN="-")
numDec <- sweep(numDec,2,sdD,FUN="/")

decathlon[,1:12] <- numDec

##########--2--###########

# - Illustrative 13
# - is.factor()

res.pca <- PCA(decathlon, quanti.sup = 11:12 , quali.sup = 13) #illustrative = supplémentaire
# 10 variables => sum eigen values = 10
summary(res.pca, nbelements = 5, ncp = 4) 
# ncp : nombre de composants principaux to display 
# nbelements : arbitrairement nombre des premières variables
barplot(res.pca$eig[,1], main = "Eigenvalues",names.arg = paste("Dim",1:nrow(res.pca$eig), sep=""))
plot(res.pca, choix = "ind", axes = c(3, 4))
plot(res.pca, choix = "var", axes = c(3, 4)) # ctr stands for contribution

# Would like a high percentage to show correlation and interprete data

##########--3--###########

#label=c("Long.jump","Points"),
#lim.cos2.var=0.5,
#select = "contrib 10"

