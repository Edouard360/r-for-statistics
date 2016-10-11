#setwd("")
vins_u <-read.csv('./vins_u.csv',sep=';')
nom_vins <-read.csv('./nom_vins.csv',sep=';')
nom_juges <-read.csv('./nom_juges.csv',sep=';')

vins <- merge(vins_u, nom_juges, by.x="numero_juge",by.y="Juge")
vins <- merge(vins, nom_vins, by.x="rang",by.y="num_vins")

by(vins[23],vins['nom_vins'],colMeans)
list(vins['nom_vins'])

GApp <- aggregate(vins[,'G.Appreciation'],list(vins[,'nom_vins']),mean)
OApp <- aggregate(vins[,'O.Appreciation'],list(vins[,'nom_vins']),mean)
Total = OApp
Total["x"] = Total["x"] + GApp["x"]
toString(Total[which.max(Total$x),]$Group.1)

library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

ggplot(vins)+aes(x=Sucree,y=G.Appreciation+O.Appreciation)+geom_point(size=0.5)+stat_smooth(method="lm") 

ggplot(vins)+aes(x=Sucree, y = G.Appreciation+O.Appreciation, col=Sexe)+geom_point()+stat_smooth()

ggplot(vins)+aes(x=Astringent,y=..density.., col=Sexe)+geom_density()

ggplot(vins)+aes(x=G.Appreciation, y =..density.., col=as.factor(seance))+geom_density()

descriptors <-colnames(vins)[7:23] 
VAgg <- aggregate(vins[,descriptors],list(vins[,'nom_vins']),mean,na.rm = T)
DT = data.frame(row.names=VAgg$Group.1,VAgg[,0:-1])

VAgg <- aggregate(vins[,descriptors],list(vins[,'cepage']),mean, na.rm=TRUE)
print(VAgg)

DTmean <-apply(DT, 2, mean)
DT <-sweep(DT, 2, STATS= DTmean, FUN = "-")
DTsd <-apply(DT, 2, sd)
DT <-sweep(DT, 2, STATS= DTsd, FUN = "/")
correlation <- cor(DT) 
plot(correlation[,"G.Appreciation"])

#setwd("")
trauma <-read.csv('./trauma_HWK1.csv',sep=',')

NACount <-apply(trauma,2,function(x) sum(is.na(x)))
IMPCount<-apply(trauma,2,function(x) sum(length(which(x=="IMP"))))
NRCount <-apply(trauma,2,function(x) sum(length(which(x=="NR"))))
plot(NACount)
plot(IMPCount)
plot(NRCount)

trauma[trauma=="NR"]<-NA
trauma[trauma=="IMP"]<-NA

trauma_avg=trauma[,] # We duplicate the data
options(warn=-1)
for(i in 1:ncol(trauma)){
  trauma_avg[is.na(trauma[,i]), i] <- mean(trauma_avg[,i], na.rm = TRUE)
}
options(warn=0)
trauma_rows_deleted = trauma[complete.cases(trauma),]

trauma[,"BMI"] <- as.numeric(gsub(",", as.character(trauma[,"BMI"]), replacement = "."))
mecanismeCount <- as.data.frame(table(trauma$Mecanisme)) 
mecanismeCount[which.max(mecanismeCount[,2]),1] 
