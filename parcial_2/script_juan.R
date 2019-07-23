# ---------------------- Importar todas las librerias ---------------------- #
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
library(rrcov)
library(e1071)
library(readxl)
library(factoextra)
library(devtools)
library(ggbiplot)
library(ggrepel)
library(gridExtra)
library(ca)
library(FactoMineR)
library(moments)
library(textshape)

# Dos grupos
# Shapiro para ver si son normales
# Si falla la normalidad de alguno de los datos, hacer esto con los dos datasets
# Debemos tener en cuenta que en este caso , el test de Mann-Whitney-Wilcoxon
# no es un test para el parametro de posicion. Por lo tanto, si rechazamos la
# hipotesis nula, podemos concluir que las distribuciones
# difieren pero no sabemos de que modo difieren
wilcox.test(Arbequina, Carolea, alternative="two.sided")

# ---------------------- ANOVA ---------------------- #
# test de la media univariado para mas de 3 grupos
data=read_excel('/Users/jknebel/Dropbox/scripts2P/Calcio.xls')

# 1 la hipotesis a testear es si difiere la media de calcio en los lotes
# H0: para todo i,j ui = uj
# H1: existe algun i,j tq ui != uj

# 2 Los supuestos son que las muestras son independientes 
# distribuciones de los residuos son normales
# y homocedasticidad de los residuos
# Para hacer test de mas de 3 grupos hay que usar anova
# Para p-valores chicos se rechazan H0 con una significancia de 0.05. Pero antes que ver los supuestos
# del contraste, diagnosticando el modelo.

data$Lote = factor(data$Lote) # si son mas de 2 clases
data.anova = aov(calcio~Lote, data)
summary(data.anova)

# Para chequear homocedasticidad de los residuos (varianzas de grupos iguales) usar test de barlet y levene

data_df=data.frame(data)
column_categorory=factor(data_df$Lote) # Transforma en factor

bartlett.test(data_df$calcio,column_categorory)
leveneTest(data_df$calcio,column_categorory) # más usado

# p-valores chicos del test rechazan la hipotesis nula
# p-valores grandes no hay evidencia estadistica para rechazar varianzas diferentes
# si los dos rechazan entonces son los dos consistentes

#boxplot
data_df$Lote <- as.factor(data_df$Lote) #Convierte la variable de grupo a categorica
ggplot(data_df) + geom_boxplot(aes(x = Lote, y = calcio, colour = Lote)) + theme_bw()

# Faltaría analizar el cumplimiento del supuesto de normalidad, hacemos Shapiro, Anderson y Agostino (no usar)
# de la distribución de los residuos, que es equivalente a analizar el supuesto de normalidad de la 
# distribución de la variable original.
# p-valor alto no hay evidencias estadisticas para rechazar el supuesto de normalidad

data.anova = aov(calcio~Lote, data)
shapiro.test(residuals(data.anova))
ad.test(residuals(data.anova))
# agostino.test(residuals(data.anova))

# Si esto falla usar test de la mediana que no tiene supuestos
# p valor chico se rechaza la hipo nula de que las medianas son iguales
mood.medtest(calcio~Lote, data=data)

# independencia??
chisq.test(x=data$Lote,data$calcio)

# Turkey para los intervalos de confianza simultaneos para las diferentes medias
# Comparaciones post hoc
# Si el intervalo contiene al 0 entonces no hay diferencia
tukey.test = TukeyHSD(data.anova)
tukey.test

# viendo que la diferencia entre 5 y 3 no tiene al cero y tiene un p-valor chico
# entonces elijo esos dos como los de mas diferencia, viendo el boxplot veo que 3
# tiene más contenido de calcio y el 5 menos. Entonces elijo 3 para el mayor y 5 para el menor

# Si no es normal, se puede aplicar box & cox
boxcox(calcio~Lote,data=data,plotit=TRUE)
data.aov2=aov(calcio^(-2)~Lote,data=data)
summary(data.aov2)

# Y ahora verifcar que se cumplan los supuestos
qqnorm(resid(data.aov2))
qqline(resid(data.aov2))
shapiro.test(residuals(data.aov2))
ad.test(residuals(data.aov2))
leveneTest(calcio^(-2)~as.factor(Lote),data=data)

# Si falla la normalidad de todo pero aun asi es homocedastica entonces se puede aplicar
# Test no parametrico de Kruskal Wallis para k muestras independientes
kruskal.test(calcio ~ Lote, data = data)

kruskalmc(data$calcio ~ data$Lote)

# ---------------------- Analisis de discriminante ---------------------- #
data=read_excel('/Users/jknebel/Dropbox/scripts2P/cancer.xls')
data = data.frame(data)
data_without_cols = data[-c(1,2)]
data_without_id = data[-1]
data_without_cols_group_1 = data %>% filter(data$Class == 0) %>% select(-c(1,2))
data_without_cols_group_2 = data %>% filter(data$Class == 1) %>% select(-c(1,2))

mean_vector_total = colMeans(data_without_cols)
mean_vector_1 = colMeans(data_without_cols_group_1)
mean_vector_2 = colMeans(data_without_cols_group_2)

# Matriz de varianza y covarianza para cada clase
var_1 = var(data_without_cols_group_1)
var_2 = var(data_without_cols_group_2)

# Matriz varianza y covarianza comun
# S = ( (total(S1) - 1) * S1 + (tatal(S2) - 1)* S2 ) / (tatal(S1) + tatal(S2) - 2)
var_total = 
  ( (nrow(data_without_cols_group_1) - 1 ) * var_1 +
  (nrow(data_without_cols_group_2) - 1 ) * var_2 ) /
  (nrow(data_without_cols) - 2)

# Hotteling
hottling_test = hotelling.test(.~ Class, data =data_without_id) 
hottling_test

# Hacer shapiro para todas las variables, si una falla entonces niormales multivariadas no son
data_without_id$Class = as.factor(data_without_id$Class)
data_tidy = melt(data_without_id, value.name = "valor")
kable(data_tidy[1:5000,] %>% group_by(Class,variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))
kable(data_tidy %>% group_by(variable) %>% summarise(p_value_anderson_darling.test = ad.test(valor)$p.value))

# Tener en cuenta lo siguiente
# Si se encuentra que una variable asume valores medios muy diferentes en los 
# distintos grupos, es probable que resulte una buena variable para discriminar.
# 
# En segunda instancia, con las variables para las cuales se notó diferencia, 
# es conveniente testear la igualdad de vectores medios entre los grupos.
# 
# Es conveniente estudiar si las matrices de varianzas-covarianzas de los distintos grupos 
# y del grupo general son similares o no.
# 
# Sólo en el caso de hallar diferencias signi􏰀cativas entre los vectores medios de los grupos
# y siendo que las matrices de varianzas-covarianzas resultaron similares, tendrá sentido utilizar
# la función discriminante lineal.

## Test de T-Student
# en teoría solo deberías usar para el discriminante las que tienen un p-valor muy chico, 
# que en este caso son todas
t.test(x = data_without_id$Adhes[data_without_id$Class == 0], 
       y = data_without_id$Adhes[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$BNucl[data_without_id$Class == 0], 
       y = data_without_id$BNucl[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$Chrom[data_without_id$Class == 0], 
       y = data_without_id$Chrom[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$Epith[data_without_id$Class == 0], 
       y = data_without_id$Epith[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$Mitos[data_without_id$Class == 0], 
       y = data_without_id$Mitos[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$Thick[data_without_id$Class == 0], 
       y = data_without_id$Thick[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$NNucl[data_without_id$Class == 0], 
       y = data_without_id$NNucl[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$UShap[data_without_id$Class == 0], 
       y = data_without_id$UShap[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = data_without_id$USize[data_without_id$Class == 0], 
       y = data_without_id$USize[data_without_id$Class == 1], 
       alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value

# Normalidad multivariada, p-valor chicos se rechaza la hipo nula, entonces no es normal
data_without_id = data[-1]
data_without_id = data.frame(data_without_id)
mshapiro.test(t(data_without_id[,-1]))

# Analizamos igualdad de matrices de varianzas y covarianzas, p-valor chicos
# rechaza la hipo nula, entonces no hay igualdad de varianza
boxM(data = data_without_id[,2:10], grouping = data_without_id[,1])

# Graficos para ver si es linealmente separable
data_with_class = data_without_id
plot(data_without_id[, 2:10], col = data_without_id$Class, pch = 19)

data_with_class$Class = if_else(data_without_id$Class==1,"A","B")
p1 <- ggplot(data = data_with_class, aes(x = Adhes, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = data_with_class, aes(x = BNucl, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = data_with_class, aes(x = Chrom, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
p4 <- ggplot(data = data_with_class, aes(x = Epith, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
p5 <- ggplot(data = data_with_class, aes(x = NNucl, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
p6 <- ggplot(data = data_with_class, aes(x = Mitos, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
p7 <- ggplot(data = data_with_class, aes(x = UShap, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
p8 <- ggplot(data = data_with_class, aes(x = USize, fill = Class)) +
  geom_histogram(position = "identity", alpha = 0.5)
cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 4, common.legend = TRUE, legend = "bottom")

plot1 <- ggplot(data = data_with_class, aes(x = Adhes)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
plot2 <- ggplot(data = data_with_class, aes(x = BNucl)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
plot3 <- ggplot(data = data_with_class, aes(x = Chrom)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
plot4 <- ggplot(data = data_with_class, aes(x = Epith)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
plot5 <- ggplot(data = data_with_class, aes(x = NNucl)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
plot6 <- ggplot(data = data_with_class, aes(x = Mitos)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
plot7 <- ggplot(data = data_with_class, aes(x = UShap)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
plot8 <- ggplot(data = data_with_class, aes(x = USize)) + 
  geom_density(aes(colour = Class)) + theme_bw() 
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7,plot8)

# Tasa de error ingenua si es LDA
#Estimamos la tasa de error ingenua
data_model.lda=lda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, data_without_id)
predictions <- predict(object = data_model.lda, newdata = data_without_id[, -1], method = "predictive")
table(data_without_id$Class, predictions$class, dnn = c("Clase real", "Clase predicha"))
trainig_error <- mean(data_without_id$Class != predictions$class) * 100 

# Hay que hacer un discriminante cuadratico LDQ
data_model.qda = qda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, data_without_id)
predictions_qda <- predict(object = data_model.qda, newdata = data_without_id[, -1], method = "predictive")
table(data_without_id$Class, predictions_qda$class, dnn = c("Clase real", "Clase predicha"))
trainig_error_qda <- mean(data_without_id$Class != predictions_qda$class) * 100

# Si anduvo todo bien entonces genero nuevo dato
new_observation <- data.frame(Adhes=1,BNucl=9,Chrom=7,Epith=1,Mitos=4,
                              NNucl=1,Thick=1,UShap=2,USize=11) 
predict(object = data_model.lda, newdata = new_observation)

# Hacer un cross validacion
set.seed(2019)
percentage = 0.7
total_obs = nrow(data_without_id)
ids_train = sample(1:total_obs,total_obs*percentage)
train_data = data_without_id[ids_train,]
validation_data = data_without_id[-ids_train,]

data_model_train.qda = qda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, train_data)
predictions_qda_with_train <- predict(object = data_model_train.qda, newdata = validation_data, method = "predictive")
table(validation_data$Class, predictions_qda_with_train$class, dnn = c("Clase real", "Clase predicha"))
trainig_error_qda_with_validation <- mean(validation_data$Class != predictions_qda_with_train$class) * 100

data_model_train.lda = lda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, train_data)
predictions_qda_with_train <- predict(object = data_model_train.lda, newdata = validation_data, method = "predictive")
table(validation_data$Class, predictions_qda_with_train$class, dnn = c("Clase real", "Clase predicha"))
trainig_error_lda_with_validation <- mean(validation_data$Class != predictions_qda_with_train$class) * 100

# Grafico de particiones no se puede probar bien con mas de 2 atributos
Origen=factor(data_without_id$Class)
partimat(Origen ~ Adhes+BNucl, data = data_without_id, method = "qda", 
         imageplot = TRUE,col.mean=1,image.colors = c("lightgrey","gold"))


# ---------------------- Clusters ---------------------- #
data_cluster = read_excel('/Users/jknebel/Dropbox/scripts2P/city.xlsx')
# data_cluster = data_cluster[-11,] # Sin chicago
# Si los nombres de columnas estan en una columna hacer
data_cluster = column_to_rownames(data_cluster, loc="city")
distance_matrix <- dist(x = data_cluster, method = "euclidean") 

# Hacer la clusterizacion
#hc_complete <- hclust(d = distance_matrix, method = "complete") 
hc_average <- hclust(d = distance_matrix, method = "average")
#hc_single <- hclust(d = distance_matrix, method = "single")
#hc_ward <- hclust(d = distance_matrix, method = "ward.D2")
cor(x = distance_matrix, cophenetic(hc_average))

# Dendrogramas
plot(hc_average)
rect.hclust(hc_average, k=3, border="blue")

grupos<-cutree(hc_average,k=3)
split(rownames(data_cluster),grupos)

fviz_cluster(object = list(data = data_cluster, cluster = cutree(hc_average, k = 3)), 
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

# K means
set.seed(2019)
fviz_nbclust(x = data_cluster, FUNcluster = kmeans, method = "wss", 
             diss = dist(data_cluster, method = "euclidean")) + 
  geom_vline(xintercept = 3, linetype = 2)

km_clusters <- kmeans(x = data_cluster, centers = 3, nstart = 25)
names(km_clusters)

split(rownames(data_cluster),km_clusters$cluster)

fviz_cluster(object = km_clusters, data = data_cluster, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")

