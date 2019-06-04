library(ca)
library("FactoMineR")
library("factoextra")

class(Titanic)
# "table"
dim(Titanic)
#[1] 4 2 2 2

> Titanic
, , Age = Child, Survived = No

Sex
Class  Male Female
1st     0      0
2nd     0      0
3rd    35     17
Crew    0      0

, , Age = Adult, Survived = No

Sex
Class  Male Female
1st   118      4
2nd   154     13
3rd   387     89
Crew  670      3

, , Age = Child, Survived = Yes

Sex
Class  Male Female
1st     5      1
2nd    11     13
3rd    13     14
Crew    0      0

, , Age = Adult, Survived = Yes

Sex
Class  Male Female
1st    57    140
2nd    14     80
3rd    75     76
Crew  192     20

Titanic[1:4,1,2,]
Survived
Class   No Yes
1st  118  57
2nd  154  14
3rd  387  75
Crew 670 192

TitAdulSobrev<-as.data.frame(Titanic[1:4,1,2,])
TitAdulSobrev
Class Survived Freq
1   1st       No  118
2   2nd       No  154
3   3rd       No  387
4  Crew       No  670
5   1st      Yes   57
6   2nd      Yes   14
7   3rd      Yes   75
8  Crew      Yes  192

Tit<-as.data.frame(Titanic)

df<-data.frame("Survived.Y"=TitAdulSobrev$Freq[5:8],"Survived.N"=TitAdulSobrev$Freq[1:4],"NoData"=c(0,0,2,4))
row.names(df)=TitAdulSobrev$Class[1:4]
df

Survived.Y Survived.N NoData
1st          57        118      0
2nd          14        154      0
3rd          75        387      2
Crew        192        670      4

## Proporción total
N<-sum(df)
Prop<-df/N
Prop<-round(Prop,4)
Prop

## Totales Fila
TotFilas<-apply(df,1,sum)
dfFil<-cbind(df,TotFilas)
colnames(dfFil)[ncol(df)+1]<-"TotFil"
dfFil

### Perfiles filas

dfPerFil<-dfFil[,-ncol(dfFil)]/rbind(rep(dfFil[1,ncol(df)+1],ncol(df)),rep(dfFil[2,ncol(df)+1],ncol(df)),rep(dfFil[3,ncol(df)+1],ncol(df)),rep(dfFil[4,ncol(df)+1],ncol(df)))
dfPerFil<-cbind(dfPerFil,apply(dfPerFil,1,sum))
colnames(dfPerFil)[ncol(dfPerFil)]<-"SumaFil"
dfPerFil<-round(dfPerFil,3)
dfPerFil



## Totales Columna
TotColumnas<-apply(df,2,sum)
dfCol<-rbind(df,TotColumnas)
rownames(dfCol)[nrow(df)+1]<-"TotCol"
dfCol


### Perfiles columnas
dfPerCol<-dfCol[-nrow(dfCol),]/cbind(rep(dfCol[nrow(df)+1,1],nrow(df)),rep(dfCol[nrow(df)+1,2],nrow(df)),rep(dfCol[nrow(df)+1,3],nrow(df)))
dfPerCol<-rbind(dfPerCol,apply(dfPerCol,2,sum))
rownames(dfPerCol)[nrow(dfPerCol)]<-"SumaCol"
dfPerCol<-round(dfPerCol,3)
dfPerCol

###Totales filas y columnas
TotFilyCol<-apply(dfFil,2,sum)
dfFilCol<-rbind(dfFil,TotFilyCol)
rownames(dfFilCol)[nrow(dfFil)+1]<-"TotCol"
dfFilCol

####Proporciones filas y columnas
N<-sum(df)
PropTot<-dfFilCol/N
PropTot<-round(PropTot,3)
PropTot

### Perfil medio Filas
dfPerfilMedioFilas<-dfFilCol[,-ncol(dfFilCol)]/rbind(rep(dfFilCol[1,ncol(dfFilCol)],3),rep(dfFilCol[2,ncol(dfFilCol)],3),rep(dfFilCol[3,ncol(dfFilCol)],3),rep(dfFilCol[4,ncol(dfFilCol)],3),rep(dfFilCol[5,ncol(dfFilCol)],3))
dfPerfilMedioFilas<-round(dfPerfilMedioFilas,3)
rownames(dfPerfilMedioFilas)[5]<-"Perfil Medio"
dfPerfilMedioFilas

plot(1:3,dfPerfilMedioFilas[1,],ylim=c(0,2),type="b",pch=16,xlab="Sobrevivientes",ylab="Proporción",xaxt='n',lwd=2)
mtext("Perfiles Filas",line=0)
axis(1,at=1:3,labels=colnames(dfPerfilMedioFilas))
lines(1:3,dfPerfilMedioFilas[2,],pch=16,type="b",col=2,lwd=2)
lines(1:3,dfPerfilMedioFilas[3,],pch=16,type="b",col=3,lwd=2)
lines(1:3,dfPerfilMedioFilas[4,],pch=16,type="b",col=4,lwd=2)
lines(1:3,dfPerfilMedioFilas[5,],pch=16,type="b",col=5,lwd=2,lty=2)
legend(2.3,2,rownames(dfPerfilMedioFilas),box.lty=0,pch=16,col=1:5,lwd=2,lty=c(1,1,1,1,2),cex=0.85,title="Clase")

### Perfil medio Columnas
dfPerfilMedioColumnas<-dfFilCol[-nrow(dfFilCol),]/cbind(rep(dfFilCol[nrow(dfFilCol),1],4),rep(dfFilCol[nrow(dfFilCol),2],4),rep(dfFilCol[nrow(dfFilCol),3],4),rep(dfFilCol[nrow(dfFilCol),4],4))
dfPerfilMedioColumnas<-round(dfPerfilMedioColumnas,3)
colnames(dfPerfilMedioColumnas)[4]<-"Perfil Medio"
dfPerfilMedioColumnas

plot(1:4,dfPerfilMedioColumnas[,1],ylim=c(0,1.5),type="b",pch=16,xlab="Clase",ylab="Proporción",xaxt='n',lwd=2)
mtext("Perfiles Columnas",line=0)
axis(1,at=1:4,labels=rownames(dfPerfilMedioColumnas))
lines(1:4,dfPerfilMedioColumnas[,2],pch=16,type="b",col=2,lwd=2)
lines(1:4,dfPerfilMedioColumnas[,3],pch=16,type="b",col=3,lwd=2)
lines(1:4,dfPerfilMedioColumnas[,4],pch=16,type="b",col=5,lwd=2,lty=2)
legend(3,1.5,colnames(dfPerfilMedioColumnas),box.lty=0,pch=16,col=c(1:3,5),lwd=2,lty=c(1,1,1,2),cex=0.85,title="Sobrevivientes")

#####################
df.ca <- CA(df, graph = FALSE)
ro<-get_ca_row(df.ca) # vemos lo que se guarda de las filas
co<-get_ca_col(df.ca) # vemos lo que se guarda de las columnas

ro$coord
ro$contrib
ro$cos2
ro$inertia

co$coord
co$contrib
co$cos2
co$inertia


fviz_contrib(df.ca, choice = "row", axes = 1) # graficamos las categorías de las filas
fviz_contrib(df.ca, choice = "col", axes = 1) # graficamos las categorías de las columnas
fviz_ca_row(df.ca, repel = TRUE)
fviz_ca_col(df.ca)
fviz_ca_biplot(df.ca, repel = TRUE) 

summary(df.ca)



#######
#MCA


Tit<-as.data.frame(Titanic)

dfmultvars<-data.frame("Class"=NA,"Sex"=NA,"Age"=NA,"Survived"=NA)
dfmultvars<-dfmultvars[-1,]



RepeatVecByRows<-function(vec,n){
  new<-vector()
  if(n==0){
    new<-rep(NULL,length(vec))
  }
  if(n!=0){
    for (i in 1:n){
      new<-rbind(new,vec)
    }
  }
  rownames(new)<-NULL
  return(new)
}

#RepeatVecByRows(c("a","b","c"),4)

for (i in 1:nrow(Tit)){
  NEWdf<-RepeatVecByRows(Tit[i,1:4],Tit$Freq[i])
  dfmultvars<-rbind(dfmultvars,NEWdf)
}
dfmultvars
#sum(Tit$Freq)
#[1] 2201
# dim(dfmultvars)
#[1] 2201    4

unique(dfmultvars)
nrow(unique(dfmultvars))# Hay 24 casos distintos

tit.mca <- MCA(dfmultvars,quali.sup=1, graph = T)# las variables deben ser introducidas como factores
fviz_contrib(tit.mca, choice ="var", axes = 1)# contribuciones de las variables
fviz_contrib(tit.mca, choice ="ind", axes = 1, top = 5) # contribuciones de los individuos
fviz_mca_var(tit.mca, repel = TRUE)# construye el biplot simétrico
#fviz_mca_ind(tit.mca, col.ind = "blue", habillage = dfmultvars$Sex, addEllipses = TRUE, repel =
#               TRUE) + theme_minimal()# señalo sobre el biplot los grupos definidos por el género
#fviz_mca_ind(tit.mca, col.ind = "blue", habillage = dfmultvars$Survived, addEllipses = TRUE, repel
#             = TRUE) + theme_minimal()# señalo sobre el biplot los grupos definidos según si sobrevivieron o no

summary(tit.mca)

resInd<-get_mca_ind(tit.mca)
head(resInd$coord)
head(resInd$cos2)
head(resInd$contrib)

resVar<-get_mca_var(tit.mca)
resVar$coord
resVar$cos2
resVar$contrib


library("ade4")
library("ca")
library("FactoMineR")
library("factoextra")
library("corrplot")
library("graphics")
library("foreign")
#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("anacor")
library("anacor")

acm.disjonctif(dfmultvars)# matriz disyuntiva
burtTable(dfmultvars)# calcula la matriz de Burt
mca.tit <- dudi.acm(dfmultvars, scannf = FALSE)
summary(mca.tit)
round(mca.tit$c1,3) # coordenadas de representación en el biplot

