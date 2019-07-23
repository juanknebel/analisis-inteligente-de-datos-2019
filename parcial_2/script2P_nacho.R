######## Importar todas las librerias ######## 
library(lubridate)
library(readxl)
library (nortest)
library(moments)
library(MASS)
library(stats)
library(heplots) # Para funcion MBox
library(dplyr)
library(RVAideMemoire)
library(factoextra)
library(devtools)
library(textshape)
library(mclust)
library(corpcor)
library(Hotelling)
library(ggplot2)
library(reshape2)
library(car)
library(openintro) 
library(gridExtra)
library(scatterplot3d)
library(knitr) 
library(mvnormtest)
#library(biotools)
library(klaR)
library(rrcov)
library(e1071)
library(ggbiplot)
library(ggrepel)
library(ca)
library(FactoMineR)
#install.packages("pgirmess") # FALLA kruskalmc
#library(pgirmess)
#BiocManager::install("mixOmics")

## EJERCICO 1 CALCIO ####
### TEST DE LA MEDIA UNIVARIADO para mas de 2 grupos ###

Calcio=read_excel('/home/ignacio/datos/Dropbox/Facultad/Maestria/C1_Analisis_Inteligente_de_Datos/clase_12/Teorica/Calcio.xls')
head(Calcio)

# 1) La hipotesis a testear es si difiere la media de calcio en los lotes.
# H0 = Medias iguales, H1= Alguno tiene media distinta 

# 2) Supuestos del modelo a Testear
# Supuestos: Distribuciones son normales y las muestras son independientes y homocedasticidad
# ANOVA compara las medias de varios grupos

# Si son mas de dos clases tengo que pasarlo a factor primero
# Le paso el LOTE que son las clases (1, 2, 3, 4, etc)
Calcio$Lote = factor(Calcio$Lote)
data.anova  = aov(calcio~Lote, data=Calcio)
summary(data.anova)
# Para P-valor chico se rechaza que sean iguales (Rechazo H0, algunas medias son diferentes), en este caso es 0.00363
# DF es igual a #Mediciones - 1
# Pero para validar esto, hay que diagnosticar el modelo.
# homocedasticidad que establece la igualdad de varianzas de los grupos
# Para chequear homocedasticidad, se  usa test de Barlett


## Estos test verifican los supuestos de ANOVA para ver si es valido o no
CalcioDf=data.frame(Calcio)
#Lote=factor(CalcioDf$Lote) # Transforma en factor
bartlett.test(CalcioDf$calcio, CalcioDf$Lote)  # Aplica el test de Bartlett
 
# P valores chicos del Test rechazan, en este caso p-value = 0.9998 rechazo que no sean diferentes las varianzas
# no hay evidencia estadística signicativa de que la varianza de alguno de los subgrupos difera de las otras


# Para chequear homocedasticidad, tambien se puede usar Levene
leveneTest(CalcioDf$calcio, CalcioDf$Lote) # PArece que es el mas usado
# Este test fue consistente con el anterior con un p-valor= 0.9978

## Test de normalidad
# Faltaría analizar el cumplimiento del supuesto de normalidad de la distribución de los residuos,
# que es equivalente a analizar el supuesto de normalidad de la distribución de la variable original.

shapiro.test(residuals(data.anova))
ad.test(residuals(data.anova))
#agostino.test(residuals(data.anova))
# Alto P-Valor (mayor a 0.05 ) no tengo evidencia para rechazar el suspuesto de normalidad.

# QUE HACEMOS CON si Shapiro-Wilk y Anderson-Darling dan diferentes??????


# Realiza las comparaciones múltiples aposteriori entre los valores medios
tukey.test = TukeyHSD(data.anova, conf.level  =0.95)
tukey.test
# Si el intervalo contiene al 0 implica que no hay diferencia
#$Lote
#    diff         lwr         upr     p adj
# 2-1  0.034 -0.09125152  0.15925152 0.9237686
# 3-1  0.066 -0.05925152  0.19125152 0.5280285
# 4-1 -0.078 -0.20325152  0.04725152 0.3675516
# 5-1 -0.094 -0.21925152  0.03125152 0.2038678
# 3-2  0.032 -0.09325152  0.15725152 0.9378237
# 4-2 -0.112 -0.23725152  0.01325152 0.0937535
# 5-2 -0.128 -0.25325152 -0.00274848 0.0436833 ## No tienen al 0 => Hay Diferencia
# 4-3 -0.144 -0.26925152 -0.01874848 0.0194205 ## No tienen al 0 => Hay Diferencia
# 5-3 -0.160 -0.28525152 -0.03474848 0.0083781 ## No tienen al 0 => Hay Diferencia
# 5-4 -0.016 -0.14125152  0.10925152 0.9950930

# Viendo que la diferencia entre 5 y 3 es la mayor, p-valor chico y no contiene al cero
# Separo al 3 como mayor contenido de calcio y el 5 como menos (mirando el boxPlot)

# GGPLOT para las ditribuciones
ggplot(CalcioDf) + geom_boxplot(aes(x = Lote, y = calcio, colour = Lote)) + theme_bw()


# Si falla la normalidad y tengo "DOS" muestras uso
# Whitney-Wilcoxon
Arbequina=c(34.5,20.1,21.8,18.2,19.5,20.2,22.5,23.9,22.1,24.2)
Carolea=c(16.4,14.8,17.8,12.3,11.9,15.5,13.4,16,15.8,16.2)
shapiro.test(Arbequina)#Testea la normalidad de los datos
shapiro.test(Carolea)#Testealanormalidad de los datos
wilcox.test(Arbequina,Carolea,alternative="two.sided")
#Realiza el test de Mann−Whitney−Wilcoxon bilateral


# Si falla la normalidad y tengo MAS de dos grupos uso el de la mediana que NO tiene supuestos, solo INDEP
# Es ULTIMO RECURSO
mood.medtest(calcio~Lote,data=Calcio)
#RealizaeltestdelamedianadeMood


## Sino se cumplio lo de la Mediana pruebo transformaciones de Box-Cox
boxcox(calcio~Lote, data=CalcioDf, plotit=TRUE)
#Investigaquétransformacióndejalosdatosmáspróximosalanormalidad
tcalcio.anova=aov (calcio^(-2)~Lote , data=CalcioDf)
summary(tcalcio.anova) # ACA EL P VALOR DA CHIQUITO PORQUE LA TRASNFORMACION NO AYUDA

#CalcioDf$Lote <- as.factor(CalcioDf$Lote) #Convierte la variable de grupo a categorica
# Si falla la normalidad de todo pero aun asi es homocedastica entonces se puede aplicar
# Test no parametrico de Kruskal Wallis para k muestras independientes
kruskal.test(calcio ~ Lote, data = Calcio)
kruskalmc(Calcio$calcio ~ data$Lote)  # Necesito una libreria que no tengo.


######## Ejercicio_2 #########
#### ANALISIS DE DISCRIMINANTE - LDA ####

cancer=read_excel('/home/ignacio/datos/Dropbox/Facultad/Maestria/C1_Analisis_Inteligente_de_Datos/scripts2P/cancer.xls')
head(cancer)

## a
# Le sacamos la columna de clase y la de ID, quedan solo los datos crudos.
cancerSinId= cancer[-1]
cancerSinClass = cancer[ -c(1,2) ]
cancerClass_0 = cancer %>% filter(cancer$Class==0) %>% select(-c(1,2))
cancerClass_1 = cancer %>% filter(cancer$Class==1) %>% select(-c(1,2))

# Hago el boxPlot de uno solo
#cancerDf=data.frame(cancer)
#ggplot(cancerDf) + geom_boxplot(aes(x = Class, y = Adhes, colour = Class)) + theme_bw()

colMeans(cancerSinClass)
colMeans(cancerClass_0)
colMeans(cancerClass_1)

nrow(cancerClass_0) #238
nrow(cancerClass_1) #443

# Calcula la matriz de varianza y covarianza para cada clase
S0 = var(cancerClass_0)
S1 = var(cancerClass_1)

# Calcula la matriz de varianza y covarianza comun
# S= ((#Clase1-1)*S1 + (#Clase2-1)*S2 ) / (ElementosTotales-2)
S=((nrow(cancerClass_0)-1)*S0 + (nrow(cancerClass_1)-1)*S1) / (nrow(cancerSinClass)-2)
round(S,4) # Redondea 4 decimales


# Hotteling: Da P-Value cero.... ???
hottling_test = hotelling.test(.~ Class, data =cancerSinId) 
hottling_test

## buscar p-valor por variable
colnames(cancer)
#"Adhes" "BNucl" "Chrom" "Epith" "Mitos" "NNucl" "Thick" "UShap" "USize"
cancerSinClass_Adhes = cancerSinClass$Adhes
cancerSinClass_BNucl = cancerSinClass$BNucl
cancerSinClass_Chrom = cancerSinClass$Chrom
cancerSinClass_Epith = cancerSinClass$Epith
cancerSinClass_Mitos = cancerSinClass$Mitos
cancerSinClass_NNucl = cancerSinClass$NNucl
cancerSinClass_Thick = cancerSinClass$Thick
cancerSinClass_UShap = cancerSinClass$UShap
cancerSinClass_USize = cancerSinClass$USize

# SI UNA NO ES NORMAL NO ES NORMAL, pero si TODAS son NORMALEs aun podria ser NO NOrmal.
shapiro.test(cancerSinClass_Adhes)
shapiro.test(cancerSinClass_BNucl)
shapiro.test(cancerSinClass_Chrom)
shapiro.test(cancerSinClass_Epith)
shapiro.test(cancerSinClass_Mitos)
shapiro.test(cancerSinClass_Thick)
shapiro.test(cancerSinClass_NNucl)
shapiro.test(cancerSinClass_UShap)
shapiro.test(cancerSinClass_USize)


## Test de T-Student
#en teoría solo deberías usar para el discriminante las que tienen un p-valor muy chico, que en este caso son todas
t.test(x = cancer$Adhes[cancer$Class == 0], y = cancer$Adhes[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$BNucl[cancer$Class == 0], y = cancer$BNucl[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$Chrom[cancer$Class == 0], y = cancer$Chrom[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$Epith[cancer$Class == 0], y = cancer$Epith[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$Mitos[cancer$Class == 0], y = cancer$Mitos[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$Thick[cancer$Class == 0], y = cancer$Thick[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$NNucl[cancer$Class == 0], y = cancer$NNucl[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$UShap[cancer$Class == 0], y = cancer$UShap[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = cancer$USize[cancer$Class == 0], y = cancer$USize[cancer$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value

# Todas tienen un P-valor chico, por lo cual uso TODAS para el analisis discriminante

# Test de normalidad
# Normalidad multivariada, p-valor chicos se rechaza la hipo nula, entonces no es normal
mshapiro.test(t(cancerSinClass))
#P-valor chico, entonces rechazo hipotesis nula. No tengo evidencia para decir que es Normal.
# Como no cumple normalidad sera QDA

# Test de igual matriz de varianza y covarianza (homocedasticidad)
# Hay que tenerlo en DATA FRAME
cancerSinId = as.data.frame(cancerSinId)
boxM(cancerSinId[,2:10],cancerSinId[,1])
#P-valor chico, entonces rechazo hipotesis nula de que tengan igual Varianza y Cov.
#Ojo que es diferente para la otra funcion. 


## Construir funcion de discriminante lineal LDA (cuadrativo es QDA)

# Class es la que define al resto
# Lineal
can.lda = lda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, cancer)
can.lda

# Si anduvo todo bien entonces genero nuevo dato
new_observation <- data.frame(Adhes=1,BNucl=9,Chrom=7,Epith=1,Mitos=4,
                              NNucl=1,Thick=1,UShap=2,USize=11) 
predict(object = can.lda, newdata = new_observation)



# Cuadratico: Como no cumple los otros uso Cuadratico
can.qda = qda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, cancer)
can.qda


# Test de Ingenuidad

predictions_qda <- predict(object = can.qda, newdata = cancerSinId[, -1], method = "predictive")
table(cancerSinId$Class, predictions_qda$class, dnn = c("Clase real", "Clase predicha"))
trainig_error <- mean(cancerSinId$Class != predictions_qda$class) * 100 
trainig_error


## clasificación con entrenamiento y validación
set.seed(2019)
#training = cancerSinId[sample(1:nrow(cancerSinId),nrow(cancerSinId)*0.7),]
ids_train = sample(1:nrow(cancerSinId),nrow(cancerSinId)*0.7)
training = cancerSinId[ids_train,]
validation = cancerSinId[-ids_train,]
nrow(training)
nrow(validation)

############## LDA ############## 
can.lda.training = lda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, training)
can.lda.training

prediccionesLda <- predict(object = can.lda.training, newdata = validation, method = "predictive") 
table(validation$Class, prediccionesLda$class, dnn = c("Origen real", "Origen predicho"))

error_test_lda<- mean(validation$Class != prediccionesLda$class) * 100
error_test_lda#4.878049%


############## QDA ############## 
can.qda.training = qda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, training)
can.qda.training

prediccionesQda <- predict(object = can.qda.training, newdata = validation, method = "predictive") 
table(validation$Class, prediccionesQda$class, dnn = c("Origen real", "Origen predicho"))

error_test_qda<- mean(validation$Class != prediccionesQda$class) * 100
error_test_qda#4.390244%


# GRAFICOS
# CON MAS DE DOS ATRIBUTOS NO SE PUEDE PROBAR BIEN

Origen=factor(cancerSinId$Class)
partimat(Origen ~ Adhes+BNucl, data = cancerSinId, method = "qda", 
         imageplot = TRUE,col.mean=1,image.colors = c("lightgrey","gold"))

partimat(Origen ~ Adhes+BNucl, data = cancerSinId, method = "lda", 
         imageplot = TRUE,col.mean=1,image.colors = c("lightgrey","gold"))


##### CLUSTER EJERCICIO 3 #####

city=read_excel('/home/ignacio/datos/Dropbox/Facultad/Maestria/C1_Analisis_Inteligente_de_Datos/scripts2P/city.xlsx')
head(city)
citySinChicago=read_excel('/home/ignacio/datos/Dropbox/Facultad/Maestria/C1_Analisis_Inteligente_de_Datos/scripts2P/citySinChicago.xlsx')
head(citySinChicago)

city = column_to_rownames(city, loc="city")
citySinChicago = column_to_rownames(citySinChicago, loc="city")


# Matriz de distancias euclídeas 
mat_dist_city <- dist(x = city, method = "euclidean")
mat_dist_mat_dist_city <- dist(x = citySinChicago , "euclidean")

# Dendrogramas  
#hc_complete <- hclust(d = mat_dist, method = "complete") 
hc_average <- hclust(d = mat_dist_city, method = "average")
hc_average_sinChicago <- hclust(d = mat_dist_mat_dist_city, method = "average")
#hc_single <- hclust(d = mat_dist, method = "single")
#hc_ward <- hclust(d = mat_dist, method = "ward.D2")
cor(x = mat_dist_city, cophenetic(hc_average))

plot(hc_average)
rect.hclust(hc_average, k=3, border="blue")

grupos<-cutree(hc_average,k=3)
split(rownames(city),grupos)


fviz_cluster(object = list(data = city, cluster = cutree(hc_average, k = 3)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

fviz_cluster(object = list(data = citySinChicago, cluster = cutree(hc_average_sinChicago, k = 3)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

#######################################

set.seed(2019) 
fviz_nbclust(x = city, FUNcluster = kmeans, method = "wss", 
             diss = dist(city, method = "euclidean")) + 
  geom_vline(xintercept = 3, linetype = 2)

km_clusters <- kmeans(x = city, centers = 3, nstart = 25)
names(km_clusters)

split(rownames(city),km_clusters$cluster)

fviz_cluster(object = km_clusters, data = city, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")

