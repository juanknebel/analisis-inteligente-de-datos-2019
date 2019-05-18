### Ejemplo de análisis robusto de componentes principales 

library(ggplot2)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library (ggrepel)
library(gridExtra)

# Recordamos el ejemplo de la clase pasada (PCA no robusto)
data("USArrests") 
head(USArrests)

dim(USArrests)#[1] 50  4

PrinComp <- prcomp(USArrests, scale = TRUE)# es lo mismo que prcomp(USArrests,center=TRUE, scale = TRUE) 

biplot(x = PrinComp, scale = 0, cex = 0.8, col = c("blue4", "brown3"))
bip1<-ggbiplot(PrinComp, choices = 1:2,labels =1:50)+ ggtitle('USArrests - Biplot - comp: 1 y 2')
bip2<-ggbiplot(PrinComp, choices = 3:4,labels =1:50)+ ggtitle('USArrests - Biplot - comp: 3 y 4')

grid.arrange(arrangeGrob(bip1 , bip2 , nrow=1))

bip1bis<-ggbiplot( PrinComp , obs.scale=1, choices=1:2)+
  geom_point (colour="royalblue") +
  geom_text_repel(aes(label=1:50),size=2) +
  theme ( legend.position="none" ) +
  #xlab ( "PC1 (50.1% de v a r i a b i l i d a d expl i c ada ) " ) +
  #ylab ( "PC2 (13.7% de v a r i a b i l i d a d expl i c ada ) " ) +
  ggtitle("Biplot entre las componentes 1 y 2") +
  theme(axis.title=element_text(size=7),
        plot.title=element_text(color="#666666",face="bold",size=9,
                                hjust=0.5) )

bip2bis<-ggbiplot( PrinComp , obs.scale=1, choices=3:4)+
  geom_point (colour="royalblue") +
  geom_text_repel(aes(label=1:50),size=2) +
  theme ( legend.position="none" ) +
  #xlab ( "PC1 (50.1% de v a r i a b i l i d a d expl i c ada ) " ) +
  #ylab ( "PC2 (13.7% de v a r i a b i l i d a d expl i c ada ) " ) +
  ggtitle("Biplot entre las componentes 3 y 4") +
  theme(axis.title=element_text(size=7),
        plot.title=element_text(color="#666666",face="bold",size=9,
                                hjust=0.5) )

grid.arrange(arrangeGrob(bip1bis , bip2bis , nrow=1))

prop_varianza <- PrinComp$sdev^2/sum(PrinComp$sdev^2) 
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:4),aes(x = pc, y = prop_varianza)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  labs(x = "Componente principal", y = "Proporción de varianza explicada")

scree1<-ggplot(data = data.frame(prop_varianza, pc = 1:4),aes(x = pc, y = prop_varianza)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  labs(x = "Componente principal", y = "Proporción de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza) 
prop_varianza_acum

ggplot(data = data.frame(prop_varianza_acum, pc = 1:4), aes(x = pc, y = prop_varianza_acum, group = 1)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x = "Componente principal", y = "Proporción de varianza explicada acumulada")

#########################

### comparamos con la alternativa robusta de mcd, rob, mve:
#(y probamos otras variantes de comandos de ggplot para graficar el biplot y el screeplot)

pcaRob=princomp(USArrests,cor = TRUE, scores = TRUE,covmat =MASS::cov.mcd(USArrests))
summary(pcaRob)
pcaRob$loadings
screeRob1<-ggscreeplot(pcaRob, type = c('pev', 'cev'))+ ggtitle('USArrests - Gráfico de Sedimentación Robusto: mcd') 
bipRob1<-ggbiplot(pcaRob, choices = 1:2,labels =1:50)+ ggtitle('USArrests - Biplot robusto: mcd - comp: 1 y 2')

pcaRob2=princomp(USArrests,cor = TRUE, scores = TRUE,covmat =MASS::cov.rob(USArrests))
summary(pcaRob2)
pcaRob2$loadings
screeRob2<-ggscreeplot(pcaRob2, type = c('pev', 'cev'))+ ggtitle('USArrests - Gráfico de Sedimentación Robusto con MASS') 
bipRob2<-ggbiplot(pcaRob2, choices = 1:2,labels =1:50)+ ggtitle('USArrests - Biplot robusto MASS comp: 1 y 2')

pcaRob3=princomp(USArrests,cor = TRUE, scores = TRUE,covmat = MASS::cov.mve(USArrests)) 
summary(pcaRob3) 
pcaRob3$loadings
screeRob3<-ggscreeplot(pcaRob3, type = c('pev', 'cev'))+ggtitle('USArrests - Gráfico de Sedimentación Robusto: mve') 
bipRob3<-ggbiplot(pcaRob3, choices = 1:2,labels =1:50)+ ggtitle('USArrests - Biplot robusto: mve - comp: 1 y 2')

grid.arrange(arrangeGrob(bip1 , bipRob1,ncol=2))
grid.arrange(arrangeGrob(bipRob2 , bipRob3,ncol=2))

grid.arrange(arrangeGrob(scree1 , screeRob1,screeRob2 , screeRob3,nrow=2))


# Comentario Ejercicio 7 de Capítulo 2
library(readxl)
internet<-read_xlsx("D:/MaestriaDataMining-DeptoCompu/AID_2019/TP2/internet.xlsx")
class(internet)
dim(internet)#1500 10
colnames(internet)
# [1] "ID"           "Nacionalidad" "Edad"         "Sexo"         "Estatura"    
# [6] "Interés"      "Tiempo"       "Temperatura"  "Autos"        "Cigarrillos"

head(internet)
table(internet$Sexo)
# 0   1   2 
# 1 684 815 
table(internet$Nacionalidad)
# Argentina  Brasilera Canadiense   Uruguaya 
# 799        329        370          2
pairs(internet[c(3,5,7:10)])
inter<-internet[c(3,5,7:10)]

#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("DMwR")
library(MASS)
library(lattice)
library(grid)
library(DMwR)#Paquete con funciones para data mining
cov1=cov.rob(inter,method="mcd",nsamp="sample")# Calcula MCD
cov2=cov.rob(inter,method="mve",nsamp="best") # Calcula MVE
cov3=cov.rob(inter,method="classical",nsamp="best")# Calcula la matriz de covarianzas clásica
center1=apply(inter,2,mean) # Calcula el vector de medias
center2=apply(inter,2,median ) # Calcula el vector de medianas

# Calcula distancias de Mahalanobis utilizando las distintas estimaciones de la matriz de covarianzas
dcov1=0 ; dcov2=0 ; dcov3=0
for(i in 1:nrow(inter)){
  dcov1[i]=mahalanobis(inter[i,],cov1$center,cov1$cov,inverted=FALSE)
  dcov2[i]=mahalanobis(inter[i,],cov2$center,cov2$cov,inverted=FALSE)
  dcov3[i]=mahalanobis(inter[i,],cov3$center,cov3$cov,inverted=FALSE)
}

distMaha<-round(cbind(dcov1,dcov2,dcov3),2)
distMahaOrd_1<-data.frame("Obs"=order(distMaha[,1],decreasing = TRUE),distMaha[order(distMaha[,1],decreasing = TRUE),])

distMahaOrd_1[1:10,]
# Obs   dcov1   dcov2  dcov3
# 1  1490 5296.80 4381.36 415.72
# 2  1365 2959.82 2452.01 233.84
# 3   836 2116.53 2124.45 876.93
# 4  1149  446.35  353.78  33.04
# 5   417  430.63  345.62  37.44
# 6   120  394.33  320.12  37.06
# 7   319  338.77  340.41 244.08
# 8   156  336.70  271.99  24.44
# 9   206  332.69  267.76  23.16
# 10  949  279.01  223.44  20.62

distMahaOrd_2<-data.frame("Obs"=order(distMaha[,2],decreasing = TRUE),distMaha[order(distMaha[,2],decreasing = TRUE),])

distMahaOrd_2[1:10,]

# Obs   dcov1   dcov2  dcov3
# 1  1490 5296.80 4381.36 415.72
# 2  1365 2959.82 2452.01 233.84
# 3   836 2116.53 2124.45 876.93
# 4  1149  446.35  353.78  33.04
# 5   417  430.63  345.62  37.44
# 6   319  338.77  340.41 244.08
# 7   120  394.33  320.12  37.06
# 8   156  336.70  271.99  24.44
# 9   206  332.69  267.76  23.16
# 10  949  279.01  223.44  20.62

distMahaOrd_3<-data.frame("Obs"=order(distMaha[,3],decreasing = TRUE),distMaha[order(distMaha[,3],decreasing = TRUE),])

distMahaOrd_3[1:10,]

# Obs   dcov1   dcov2  dcov3
# 1   836 2116.53 2124.45 876.93
# 2  1490 5296.80 4381.36 415.72
# 3   319  338.77  340.41 244.08
# 4  1365 2959.82 2452.01 233.84
# 5  1112  174.15  173.61 132.53
# 6  1466  125.61  127.57  84.41
# 7  1157   88.73   86.95  64.90
# 8   417  430.63  345.62  37.44
# 9   120  394.33  320.12  37.06
# 10 1149  446.35  353.78  33.04

inter<-as.data.frame(inter)

par(mfrow=c(3,1))
plot(inter[,1],inter[,2],pch=16, type="p",xlab="Edad",ylab="Estatura")
points(inter[distMahaOrd_1[1:10,1],1],inter[distMahaOrd_1[1:10,1],2],pch=16,col=2)
plot(inter[,1],inter[,2],pch=16, type="p",xlab="Edad",ylab="Estatura")
points(inter[distMahaOrd_2[1:10,1],1],inter[distMahaOrd_2[1:10,1],2],pch=16,col=3)
plot(inter[,1],inter[,2],pch=16, type="p",xlab="Edad",ylab="Estatura")
points(inter[distMahaOrd_3[1:10,1],1],inter[distMahaOrd_3[1:10,1],2],pch=16,col=5)
par(mfrow=c(1,1))


par(mfrow=c(3,1))
plot(inter[,3],inter[,4],pch=16, type="p",xlab="Tiempo",ylab="Temperatura")
points(inter[distMahaOrd_1[1:10,1],3],inter[distMahaOrd_1[1:10,1],4],pch=16,col=2)
plot(inter[,3],inter[,4],pch=16, type="p",xlab="Tiempo",ylab="Temperatura")
points(inter[distMahaOrd_2[1:10,1],3],inter[distMahaOrd_2[1:10,1],4],pch=16,col=3)
plot(inter[,3],inter[,4],pch=16, type="p",xlab="Tiempo",ylab="Temperatura")
points(inter[distMahaOrd_3[1:10,1],3],inter[distMahaOrd_3[1:10,1],4],pch=16,col=5)
par(mfrow=c(1,1))

par(mfrow=c(3,1))
plot(inter[,5],inter[,6],pch=16, type="p",xlab="Autos",ylab="Cigarrillos")
points(inter[distMahaOrd_1[1:10,1],5],inter[distMahaOrd_1[1:10,1],6],pch=16,col=2)
plot(inter[,5],inter[,6],pch=16, type="p",xlab="Autos",ylab="Cigarrillos")
points(inter[distMahaOrd_2[1:10,1],5],inter[distMahaOrd_2[1:10,1],6],pch=16,col=3)
plot(inter[,5],inter[,6],pch=16, type="p",xlab="Autos",ylab="Cigarrillos")
points(inter[distMahaOrd_3[1:10,1],5],inter[distMahaOrd_3[1:10,1],6],pch=16,col=5)
par(mfrow=c(1,1))



par(mfrow=c(3,1))
plot(inter[,1],inter[,2],pch=16, type="p",xlab="Edad",ylab="Estatura")
points(inter[distMahaOrd_1[1:10,1],1],inter[distMahaOrd_1[1:10,1],2],pch=16,col=rainbow(10))
plot(inter[,3],inter[,4],pch=16, type="p",xlab="Tiempo",ylab="Temperatura")
points(inter[distMahaOrd_1[1:10,1],3],inter[distMahaOrd_1[1:10,1],4],pch=16,col=rainbow(10))
plot(inter[,5],inter[,6],pch=16, type="p",xlab="Autos",ylab="Cigarrillos")
points(inter[distMahaOrd_1[1:10,1],5],inter[distMahaOrd_1[1:10,1],6],pch=16,col=rainbow(10))
par(mfrow=c(1,1))