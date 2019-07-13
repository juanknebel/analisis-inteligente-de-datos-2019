#Retomamos el último ejemplo de la clase pasada

# Se	 observaron	 dos	 grupos de salmones de Alaska y Canadá y se quiere	
# determinar el origen de los mismos en función de los datos obtenidos
library(readxl)
salmon <- read_excel("D:/MaestriaDataMining-DeptoCompu/AID/AIDproject/salmon.xlsx")
head(salmon)

set.seed(2019)
entrenamiento<-sample(1:100,70)
validación<-c(1:100)[-entrenamiento]
Origen=factor(salmon$origen)

### Regresión Logística

Alaska0_Canada1<-ifelse(Origen[entrenamiento]=="Alaska",0,1)
Alaska0_Canada1_test<-ifelse(Origen[validación]=="Alaska",0,1)
modelo_reg_logis<-glm(as.factor(Alaska0_Canada1) ~ mar+aguadulce, data = salmon[entrenamiento,],family="binomial")
prediccionesRegLog<-ifelse(modelo_reg_logis$fitted.values>0.5,1,0) 
table(salmon$origen[entrenamiento], prediccionesRegLog, dnn = c("Origen real", "Origen predicho"))
error_RegLog_ingenuo<- mean(Alaska0_Canada1 != prediccionesRegLog) * 100
error_RegLog_ingenuo#7.142857%

pred_test_RegLog<-predict(object=modelo_reg_logis,newdata=salmon[validación,-1],type="response")
pred_test_RegLog_0_1<-ifelse(pred_test_RegLog>0.5,1,0)
table(salmon$origen[validación], pred_test_RegLog_0_1, dnn = c("Origen real", "Origen predicho"))


error_RegLog<- mean(Alaska0_Canada1_test!= pred_test_RegLog_0_1) * 100
error_RegLog#6.666667%


### SVM

library(ggplot2)
library(e1071)
modelo_svm=svm(as.factor(Alaska0_Canada1)~mar+aguadulce,data=salmon[entrenamiento,],method="C-classification",kernel="radial",cost=10,gamma=.1)
pred_svm=predict(modelo_svm, salmon[validación,-1])
table(salmon$origen[validación], pred_svm, dnn = c("Origen real", "Origen predicho"))
error_svm<- mean(Alaska0_Canada1_test!= pred_svm) * 100
error_svm#10%

#plot(modelo_svm,salmon[entrenamiento,],symbolPalette=topo.colors(3),dataSymbol="o",color.palette=cm.colors)
plot(modelo_svm,salmon[entrenamiento,])


## Clustering

data(USArrests) 
datos <- scale(USArrests) 
head(datos)

# Métodos Jerárquicos:

# Matriz de distancias euclídeas 
mat_dist <- dist(x = datos, method = "euclidean") 

# Dendrogramas  
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
grupos<-cutree(hc_ward,k=3)
split(rownames(datos),grupos)

library(factoextra) 
library(dplyr)
datos2 <- USArrests 
set.seed(101) 
hc_completo <- datos2 %>% scale() %>% dist(method = "euclidean") %>% 
  hclust(method = "complete") 
fviz_dend(x = hc_completo, k = 2, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed")

fviz_cluster(object = list(data = datos2, cluster = cutree(hc_completo, k = 2)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

fviz_dend(x = hc_completo, k = 4, cex = 0.6) + 
  geom_hline(yintercept = 3.5, linetype = "dashed")

fviz_cluster(object = list(data = datos2, cluster = cutree(hc_completo, k = 4)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()



# K-means:

library(factoextra) 
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", 
             diss = dist(datos, method = "euclidean")) + 
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123) 
km_clusters <- kmeans(x = datos, centers = 4, nstart = 25)
names(km_clusters)
split(rownames(datos),km_clusters$cluster)

fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")


