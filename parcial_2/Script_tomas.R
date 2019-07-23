######## Importar todas las librerias ######## 
library(ggplot2)
library(stats)
library(reshape2)
library(car)
library(nortest)
library(openintro) 
library(RVAideMemoire)
library(MASS)
library(pgirmess)
library(corpcor)
library(Hotelling)
library(gridExtra)
library(scatterplot3d)
library(reshape2) 
library(knitr) 
library(dplyr) 
library(mvnormtest)
library(biotools)
library(klaR)
#install.packages("factoextra")
#library(rrcov)
library(e1071)
library(readxl)
#library(factoextra)
library(devtools)
#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")
library(ggbiplot)
library(ggrepel)
library(gridExtra)
library(ca)
library(FactoMineR)


######## Como cargar un set de datos ########
#xls o xlsx
Calcio_df <-read_excel("C:/Users/tomas/Desktop/Calcio.xls")
Calcio_df <- as.data.frame(Calcio_df)
#csv
Reglas <- read.csv(file="C:/Users/tomas/Desktop/Calcio.csv", header=TRUE, sep=",")
######## Comparacion de medias - Supuestos ########
#Testear normalidad
shapiro.test(Arbequina) # Testea la normalidad de los datos
shapiro.test(Carolea) # Testea la normalidad de los datos
#Si los datos no estan en columnas separadas si que depende de los valores de otra columna
shapiro.test(births2[births2$smoke == "smoker","weight"])
shapiro.test(births2[births2$smoke == "nonsmoker","weight"])
#BoxPlots
Calcio_df$Lote <- as.factor(Calcio_df$Lote) #Convierte la variable de grupo a categorica
ggplot(Calcio_df) + geom_boxplot(aes(x = Lote, y = calcio, colour = Lote)) + theme_bw()
#Analiza igualdad de las varianzas
leveneTest(calcio ~ Lote, data = Calcio_df)
######## Caso univariado parametrico########
#T de Student para muestras independientes
t.test(Calcio_df$calcio[which(Calcio_df$Lote==1)], y = Calcio_df$calcio[which(Calcio_df$Lote==2)], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)
# Si queremos que el test sea unilateral cambiar argumento "alternative" a "greater" or "less
#T de Student para muestras apareadas
datos <- data.frame(corredor = c(1:10), antes = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3), despues = c(12.7, 13.6, 12, 15.2, 16.8, 20, 12, 15.9, 16, 11.1)) 
t.test(x = datos$antes, y = datos$despues, alternative = "two.sided", mu = 0, paired = TRUE, conf.level = 0.95)
# ANOVA de 1 factor
porcentaje<-c(rep(15,5),rep(20,5),rep(25,5),rep(30,5),rep(35,5))
resistencia<-c(7,7,15,11,9,12,17,12,18,18,14,18,18,19,19,19,25,22,19,23,7,10,11,15,11)
porcAlgodon <-data.frame(porcentaje,resistencia)
porcAlgodon$porcentaje <-as.factor(porcAlgodon$porcentaje)
boxplot(resistencia~porcentaje)
AOVporcAlgo<- aov(porcAlgodon$resistencia~porcAlgodon$porcentaje)
summary(AOVporcAlgo)
# Se analiza igualdad de varianzas
bartlett.test(porcAlgodon$resistencia~porcAlgodon$porcentaje)
leveneTest(porcAlgodon$resistencia~porcAlgodon$porcentaje)
#Se analiza normalidad
shapiro.test(residuals(AOVporcAlgo))
#Pruebas PostHoc de Tukey
TukeyHSD(AOVporcAlgo,conf.level=0.95)
#ANOVA de 1 factor utilizando transformaciones de Box-Cox
dosis<-c(rep("a",6),rep("b",8),rep("c",4))
alerta<-c(30,38,35,41,27,24,32,26,31,29,27,35,21,25,17,21,20,19)
data<-data.frame(dosis,alerta)
aov.data<-aov(alerta~dosis,data=data)
summary(aov.data)
shapiro.test(residuals(aov.data))
boxplot(split(data$alerta,data$dosis),ylab="Alerta",xlab="Dosis")
bartlett.test(alerta,dosis)
leveneTest(alerta~as.factor(dosis))
boxcox(alerta~dosis,data=data,plotit=TRUE)# el máximo lambda se alcanza en -1.
aov.data2=aov(alerta^(-1)~dosis,data=data)
summary(aov.data2)
######## Caso univariado no parametrico########
#Test de Mann-Whitney-Wilcoxon
wilcox.test (Arbequina ,Carolea ,alternative="two.sided")
#Test de Mann-Whitney-Wilcoxon
muestra1 <- c( 1.1, 3.4, 4.3, 2.1, 7.0 , 2.5 ) 
muestra2 <- c( 7.0, 8.0, 3.0, 5.0, 6.2 , 4.4 )
wilcox.test(x = muestra1, y = muestra2, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)
#Test de la mediana
mood.medtest(muestra2~muestra1)
#Test no paramétrico de Kruskal Wallis para k muestras independientes
datos <- data.frame(condicion = c(rep("condicion1", 18), rep("condicion2", 18), rep("condicion3", 18)), n_huevos = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 27, 28, 29, 30, 51, 52, 53, 342, 40, 41, 42, 43, 44, 45, 46, 47, 48, 67, 88, 89, 90, 91, 92, 93, 94, 293, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 25, 36, 37, 58, 59, 60, 71, 72)) 
ggplot(data = datos, mapping = aes(x = condicion, y = n_huevos, colour = condicion)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = datos, mapping = aes(x = n_huevos, colour = condicion)) + 
  geom_histogram() + theme_bw() + facet_grid(. ~ condicion) + 
  theme(legend.position = "none")# + stat_bin(binwidth=30)
leveneTest(n_huevos ~ condicion, data = datos)
kruskal.test(n_huevos ~ condicion, data = datos)
kruskalmc(datos$n_huevos ~ datos$condicion)

######## Caso multivariado parametrico ########
#Test de Hotteling
clase<-c(rep("a",10),rep("b",10))
comp1<-c(10.7  , 9.84  , 9.55  , 10.13  , 9.84 , 10.7  , 10.99  , 11.28  , 11.28  , 10.7  , 9.26  , 9.26  , 9.26 , 9.55 , 8.97 , 8.39 , 8.39 , 9.26 , 8.39, 8.1)
comp2<-c(10.54  ,  9.74  ,  9.58  ,  10.56 ,  10.38  ,  9.44 ,  9.47  ,  10.67,  10.4,9.05  ,  8.52,  7.47 ,  8.46 ,  8.75  ,  8.19 ,  8.53  ,  8.38 ,  7.88  ,  8.4,  8.11)
comp3<-c(10.83,  8.51, 8.51, 10.06,  9.28,  9.28,  7.74,  9.28,  9.28, 7.74,  9.28,  6.96, 9.28,  9.28,  9.28,  6.96,  7.74,8.51,8.51, 7.74)
prod<-data.frame(clase,comp1,comp2,comp3)
fitProd = hotelling.test(.~ clase, data =prod) 
fitProd
p1<- ggplot(data = prod, aes(x = comp1, fill = clase)) + 
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = prod, aes(x = comp2, fill = clase)) + 
  geom_histogram(position = "identity", alpha = 0.5) 
p3 <- ggplot(data = prod, aes(x = comp3, fill = clase)) + 
  geom_histogram(position = "identity", alpha = 0.5)
grid.arrange(p1, p2, p3)
pairs(x = prod[, c("comp1", "comp2", "comp3")], col = c("firebrick", "green3")[prod$clase], pch = 19)
scatterplot3d(prod$comp1, prod$comp2, prod$comp3, color = c("firebrick", "green3")[prod$clase], pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "Comp 1", ylab = "Comp 2", zlab = "Comp 3", angle = 65) 
legend("bottomright", bty = "n", cex = .9, title = "Clase", c("a", "b"), fill = c("firebrick", "green3"))
#Testea normalidad multivariada
mshapiro.test(t(prod[,-1]))
#Analizamos igualdad de matrices de varianzas y covarianzas
boxM(data = prod[, 2:4], grouping = prod[, 1])
######## Analisis discriminante lineal ########
#Ejemplo con 2 clases
#Aplicamos LDA
modelo_lda <- lda(formula = clase ~ comp1 + comp2 + comp3, data = prod)
modelo_lda
#clasificamos una nueva observacion
nuevas_observaciones <- data.frame(comp1 = 10, comp2 = 9, comp3 = 7) 
predict(object = modelo_lda, newdata = nuevas_observaciones)
#Estimamos la tasa de error ingenua
predicciones <- predict(object = modelo_lda, newdata = prod[, -1], method = "predictive")
table(prod$clase, predicciones$class, dnn = c("Clase real", "Clase predicha"))
trainig_error <- mean(prod$clase != predicciones$class) * 100 
trainig_error#0%
with(prod, { 
  s3d <- scatterplot3d(comp1, comp2, comp3, color = c("firebrick", "green3")[prod$clase], pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "Comp 1", ylab = "Comp 2", zlab = "Comp 3", angle = 65) 
  s3d.coords <- s3d$xyz.convert(comp1, comp2, comp3) # convierte coordenadas 3D en proyecciones 2D 
  text(s3d.coords$x, s3d.coords$y, labels = prod$clase, cex = .8, pos = 4) 
  legend("bottomright", bty = "n", cex = .9, title = "Clase", c("a", "b"), fill = c("firebrick", "green3")) 
})
#Ejemplo con 3 clases
data("iris") 
head(iris, n = 3)
#Plotea las distribuciones
plot1 <- ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
plot2 <- ggplot(data = iris, aes(x = Sepal.Width)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
plot3 <- ggplot(data = iris, aes(x = Petal.Length)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
plot4 <- ggplot(data = iris, aes(x = Petal.Width)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
grid.arrange(plot1, plot2, plot3, plot4)
pairs(x = iris[, -5], col = c("firebrick", "green3", "blue")[iris$Species], pch = 20)
par(mfrow=c(1,1))
datos_tidy <- melt(iris, value.name = "valor") 
kable(datos_tidy %>% group_by(Species, variable) %>% 
        summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value, 5)))
#Analizamos normalidad multivariada
mshapiro.test(t(prod[,-1]))
#Analizamos igualdad de matrices de varianzas y covarianzas
boxM(data = iris[, 1:4], grouping = iris[, 5])
#Aplicamos LDA
modelo_lda_iris <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris) 
modelo_lda_iris
#Estimamos la tasa de error ingenua
prediccionesIris <- predict(object = modelo_lda_iris, newdata = iris[, -5]) 
table(iris$Species, prediccionesIris$class, dnn = c("Clase real", "Clase predicha"))
trainig_error_iris <- mean(iris$Species != prediccionesIris$class) * 100
trainig_error_iris#2
#Graficamos la funcion
partimat(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris, method = "lda", prec = 200,image.colors = c("darkgoldenrod1", "snow2", "skyblue2"), col.mean = "firebrick")

######## Analisis discriminante cuadratico ########
#Simulamos carga de datos
set.seed(1234)
grupoA_x <- seq(from = -3, to = 4, length.out = 100)-+ rnorm(100, sd = 1)
grupoA_y <- 6 + 0.15 * grupoA_x - 0.3 * grupoA_x^2 + rnorm(100, sd = 1)
grupoA <- data.frame(variable_z = grupoA_x, variable_w = grupoA_y, grupo = "A")
grupoB_x <- rnorm(n = 100, mean = 0.5, sd = 0.8)
grupoB_y <- rnorm(n = 100, mean = 2, sd = 0.9)
grupoB <- data.frame(variable_z = grupoB_x, variable_w = grupoB_y, grupo = "B")
datos <- rbind(grupoA, grupoB)
# Ploteamos los datos
plot(datos[, 1:2], col = datos$grupo, pch = 19) # No son linealmente separables
# Representamos distribuciones de las variables en histogramas
p1 <- ggplot(data = datos, aes(x = variable_z, fill = grupo)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x = variable_w, fill = grupo)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom") #tira error
#Analizamos los supuestos del modelo
#Histograma de cada variable para cada grupo
par(mfcol = c(2, 2))
for (k in 1:2) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$grupo)[i]
    x <- datos[datos$grupo == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("grupo", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}
## QQ plot normal por variable para cada grupo
par(mfcol = c(2, 2))
for (k in 1:2) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$grupo)[i]
    x <- datos[datos$grupo == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1)
    qqline(x)
  }
}
# Contraste de normalidad Shapiro-Wilk para cada variable en cada grupo.
datos_tidy <- melt(datos, value.name = "valor")
datos_tidy %>%
  group_by(grupo, variable) %>% 
  summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5)) #La variable w no satisface normalidad univariada en el grupo A.
# Análisis de normalidad multivariada
mshapiro.test(t(datos[,-3]))
# Analizamos igualdad de matrices de varianzas y covarianzas
boxM(data = datos[, 1:2], grouping = datos[, 3])
#No se verifica la normalidad multivariada.
#Si bien QDA tiene cierta robustez frente a la falta de normalidad multivariante
#es importante tenerlo en cuenta en la conclusión del análisis.
#Tampoco se verifica la igualdad de matrices de varianzas y covarianzas

# Aplicamos analisis discriminante cuadratico
modelo_qda <- qda(grupo ~ variable_z + variable_w, data = datos,CV=TRUE)
# modelo_qda
table(modelo_qda$class,datos$grupo)
# Tasa de  clasificación ingenua del discriminante
mean(modelo_qda$class==datos$grupo) # tasa de buena clasificación ingenua del discr. cuadrático
# Tasa de clasificacion con entrenamiento y validacion
set.seed(102030)
train=sample(1:200,150)
df_train=data.frame(datos[train,])
df_test=data.frame(datos[-train,])
qda.train=qda(grupo~ variable_z + variable_w, data = df_train)
qda_predTest = predict(qda.train, df_test)
qda_class = predict(qda.train, df_test)$class
table(qda_class, df_test$grupo)
mean(qda_class== df_test$grupo) # tasa de clasificación

######## Analisis discriminante cuadratico robusto ######## 
#Aplicamos Analisis discriminante cuadratico robusto
rob.mcd=QdaCov(datos$grupo~., data = datos)
predict(rob.mcd)@classification
table(predict(rob.mcd)@classification,datos$grupo)
#Medimos tasa de clasificacion ingenua
mean(predict(rob.mcd)@classification==datos$grupo) # tasa de buena clasif. ingenua del discr. robusto

######## Analisis de Cluster Jeraquico ########
#Cargo los datos
data(USArrests) 
datos <- scale(USArrests) 
head(datos)
#Matriz de distancias euclideas
mat_dist <- dist(x = datos, method = "euclidean") 
# Dendogramas
hc_complete <- hclust(d = mat_dist, method = "complete") 
hc_average <- hclust(d = mat_dist, method = "average")
hc_single <- hclust(d = mat_dist, method = "single")
hc_ward <- hclust(d = mat_dist, method = "ward.D2")
cor(x = mat_dist, cophenetic(hc_complete))
cor(x = mat_dist, cophenetic(hc_average))
cor(x = mat_dist, cophenetic(hc_single))
cor(x = mat_dist, cophenetic(hc_ward))
plot(hc_ward )
rect.hclust(hc_ward, k=4, border="red")
# Muestra los componenetes de cada cluster
grupos<-cutree(hc_ward,k=4)
split(rownames(datos),grupos)
# Cambio el k a otro valor, por ejemplo "3"
grupos<-cutree(hc_ward,k=3)
split(rownames(datos),grupos)
# Creo dendograma con limite de clusters k=2
datos2 <- USArrests 
set.seed(101) 
hc_completo <- datos2 %>% scale() %>% dist(method = "euclidean") %>% 
  hclust(method = "complete") 
fviz_dend(x = hc_completo, k = 2, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed")
# Hago el cluster plot
fviz_cluster(object = list(data = datos2, cluster = cutree(hc_completo, k = 2)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()
######## Analisis de Cluster No Jeraquico ########
#K-means
#Cargo los datos
data(USArrests) 
datos <- scale(USArrests) 
head(datos)
# Calculo el numero optimo de clusters
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", 
             diss = dist(datos, method = "euclidean")) + 
  geom_vline(xintercept = 4, linetype = 2)
# Enumero los individuos que componen ese cluster
set.seed(123) 
km_clusters <- kmeans(x = datos, centers = 4, nstart = 25)
names(km_clusters)
split(rownames(datos),km_clusters$cluster)
# Grafico el cluster
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")

######## PCA ########
#Carga el dataset
data("USArrests") 
head(USArrests)
#Hace PCA
PrinComp <- prcomp(USArrests, scale = TRUE)
names(PrinComp)
PrinComp$center# media
PrinComp$scale# desvio
PrinComp$rotation# loadings (autovectores)
PrinComp$sdev# raíz cuadrada de los autovalores
head(PrinComp$x)# scores
#Biplot (basico)
biplot(x = PrinComp, scale = 0, cex = 0.8, col = c("blue4", "brown3"))
#Biplot (mas lindo)
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
# Graficos de sedimentacion
prop_varianza <- PrinComp$sdev^2/sum(PrinComp$sdev^2) 
prop_varianza
ggplot(data = data.frame(prop_varianza, pc = 1:4),aes(x = pc, y = prop_varianza)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  labs(x = "Componente principal", y = "Proporción de varianza explicada")
######## Perfiles fila y columna ######## 
#Cargo dataset
Titanic
TitAdulSobrev<-as.data.frame(Titanic[1:4,1,2,])
TitAdulSobrev
df<-data.frame("Survived.Y"=TitAdulSobrev$Freq[5:8],"Survived.N"=TitAdulSobrev$Freq[1:4],"NoData"=c(0,0,2,4))
row.names(df)=TitAdulSobrev$Class[1:4]
df
#Gráficos de Perfiles Fila y Columna
N<-sum(df)
Prop<-df/N
Prop<-round(Prop,4)
Prop
#Totales fila
TotFilas<-apply(df,1,sum)
dfFil<-cbind(df,TotFilas)
colnames(dfFil)[ncol(df)+1]<-"TotFil"
dfFil
#Perfiles fila
Perfiles filas 
dfPerFil<-dfFil[,-ncol(dfFil)]/rbind(rep(dfFil[1,ncol(df)+1],ncol(df)),rep(dfFil[2,ncol(df)+1],ncol(df)),rep(dfFil[3,ncol(df)+1],ncol(df)),rep(dfFil[4,ncol(df)+1],ncol(df)))
dfPerFil<-cbind(dfPerFil,apply(dfPerFil,1,sum))
colnames(dfPerFil)[ncol(dfPerFil)]<-"SumaFil"
dfPerFil<-round(dfPerFil,3)
dfPerFil
#Totales Columna 
TotColumnas<-apply(df,2,sum)
dfCol<-rbind(df,TotColumnas)
rownames(dfCol)[nrow(df)+1]<-"TotCol"
dfCol
#Perfiles columnas
dfPerCol<-dfCol[-nrow(dfCol),]/cbind(rep(dfCol[nrow(df)+1,1],nrow(df)),rep(dfCol[nrow(df)+1,2],nrow(df)),rep(dfCol[nrow(df)+1,3],nrow(df)))
dfPerCol<-rbind(dfPerCol,apply(dfPerCol,2,sum))
rownames(dfPerCol)[nrow(dfPerCol)]<-"SumaCol"
dfPerCol<-round(dfPerCol,3)
dfPerCol
#Totales filas y columnas
TotFilyCol<-apply(dfFil,2,sum)
dfFilCol<-rbind(dfFil,TotFilyCol)
rownames(dfFilCol)[nrow(dfFil)+1]<-"TotCol"
dfFilCol
#Proporciones filas y columnas 
N<-sum(df)
PropTot<-dfFilCol/N
PropTot<-round(PropTot,3)
PropTot
#Perfil medio Filas 
dfPerfilMedioFilas<-dfFilCol[,-ncol(dfFilCol)]/rbind(rep(dfFilCol[1,ncol(dfFilCol)],3),rep(dfFilCol[2,ncol(dfFilCol)],3),rep(dfFilCol[3,ncol(dfFilCol)],3),rep(dfFilCol[4,ncol(dfFilCol)],3),rep(dfFilCol[5,ncol(dfFilCol)],3))
dfPerfilMedioFilas<-round(dfPerfilMedioFilas,3)
rownames(dfPerfilMedioFilas)[5]<-"Perfil Medio"
dfPerfilMedioFilas
#Plot
plot(1:3,dfPerfilMedioFilas[1,],ylim=c(0,2),type="b",pch=16,xlab="Sobrevivientes",ylab="Proporción",xaxt='n',lwd=2)
mtext("Perfiles Filas",line=0)
axis(1,at=1:3,labels=colnames(dfPerfilMedioFilas))
lines(1:3,dfPerfilMedioFilas[2,],pch=16,type="b",col=2,lwd=2)
lines(1:3,dfPerfilMedioFilas[3,],pch=16,type="b",col=3,lwd=2)
lines(1:3,dfPerfilMedioFilas[4,],pch=16,type="b",col=4,lwd=2)
lines(1:3,dfPerfilMedioFilas[5,],pch=16,type="b",col=5,lwd=2,lty=2)
legend(2.3,2,rownames(dfPerfilMedioFilas),box.lty=0,pch=16,col=1:5,lwd=2,lty=c(1,1,1,1,2),cex=0.85,title="Clase")
#Perfil medio columnas
dfPerfilMedioColumnas<-dfFilCol[-nrow(dfFilCol),]/cbind(rep(dfFilCol[nrow(dfFilCol),1],4),rep(dfFilCol[nrow(dfFilCol),2],4),rep(dfFilCol[nrow(dfFilCol),3],4),rep(dfFilCol[nrow(dfFilCol),4],4))
dfPerfilMedioColumnas<-round(dfPerfilMedioColumnas,3)
colnames(dfPerfilMedioColumnas)[4]<-"Perfil Medio"
dfPerfilMedioColumnas
#Plot
plot(1:4,dfPerfilMedioColumnas[,1],ylim=c(0,1.5),type="b",pch=16,xlab="Clase",ylab="Proporción",xaxt='n',lwd=2)
mtext("Perfiles Columnas",line=0)
axis(1,at=1:4,labels=rownames(dfPerfilMedioColumnas))
lines(1:4,dfPerfilMedioColumnas[,2],pch=16,type="b",col=2,lwd=2)
lines(1:4,dfPerfilMedioColumnas[,3],pch=16,type="b",col=3,lwd=2)
lines(1:4,dfPerfilMedioColumnas[,4],pch=16,type="b",col=5,lwd=2,lty=2)
legend(3,1.5,colnames(dfPerfilMedioColumnas),box.lty=0,pch=16,col=c(1:3,5),lwd=2,lty=c(1,1,1,2),cex=0.85,title="Sobrevivientes")

######## Analisis de Correspondencias ######## 
#Analisis de correspondencia simple
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
# graficamos las categorías de las filas
fviz_contrib(df.ca, choice = "row", axes = 1) 
# graficamos las categorías de las columnas
fviz_contrib(df.ca, choice = "col", axes = 1) 
fviz_ca_row(df.ca, repel = TRUE)
fviz_ca_col(df.ca)
#Biplot
fviz_ca_biplot(df.ca, repel = TRUE)
#Resumen
summary(df.ca)
