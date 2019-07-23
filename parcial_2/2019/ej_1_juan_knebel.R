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

# ---------------------- Ejercicio 1 ---------------------- #

data_cluster_raw=read_excel('./leches.xlsx')
data_cluster = data_cluster_raw
data_cluster = column_to_rownames(data_cluster, loc="Mamífero")
distance_matrix <- dist(x = data_cluster, method = "euclidean") 

# Hacer la clusterizacion
hc_complete <- hclust(d = distance_matrix, method = "complete") 
hc_average <- hclust(d = distance_matrix, method = "average")
hc_single <- hclust(d = distance_matrix, method = "single")
hc_ward <- hclust(d = distance_matrix, method = "ward.D2")

cor(x = distance_matrix, cophenetic(hc_complete))
cor(x = distance_matrix, cophenetic(hc_average))
cor(x = distance_matrix, cophenetic(hc_single))
cor(x = distance_matrix, cophenetic(hc_ward))

# Dendrogramas
plot(hc_complete)
rect.hclust(hc_complete, k=4, border="blue")

plot(hc_average)
rect.hclust(hc_average, k=4, border="blue")

#plot(hc_single)
#rect.hclust(hc_single, k=3, border="blue")
#plot(hc_ward)
#rect.hclust(hc_ward, k=3, border="blue")

grupos_complete<-cutree(hc_complete,k=4)
grupos_average<-cutree(hc_average,k=4)
split(rownames(data_cluster),grupos_complete)
split(rownames(data_cluster),grupos_average)

fviz_cluster(object = list(data = data_cluster, cluster = cutree(hc_complete, k = 3)), 
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

fviz_cluster(object = list(data = data_cluster, cluster = cutree(hc_average, k = 3)), 
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

# K means
set.seed(2019)
fviz_nbclust(x = data_cluster, FUNcluster = kmeans, method = "wss", 
             diss = dist(data_cluster, method = "euclidean")) + 
  geom_vline(xintercept = 4, linetype = 2)

km_clusters <- kmeans(x = data_cluster, centers = 4, nstart = 25)
names(km_clusters)

split(rownames(data_cluster),km_clusters$cluster)

fviz_cluster(object = km_clusters, data = data_cluster, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")

cluster_1 = data_cluster_raw %>% filter(Mamífero=="FOCA" | Mamífero=="DELFÍN")
cluster_2 = data_cluster_raw %>% filter(Mamífero=="HAMSTER" | Mamífero=="OVEJA" 
                                        | Mamífero=="CERDO" | Mamífero=="BÚFALO" 
                                        | Mamífero=="ZORRO" | Mamífero=="GATO" | Mamífero=="PERRO")
cluster_3 = data_cluster_raw %>% filter(Mamífero=="RATA" | Mamífero=="RENO" 
                                        | Mamífero=="CONEJO" | Mamífero=="CIERVO")
cluster_4 = data_cluster_raw %>% filter(Mamífero=="CABALLO" | Mamífero=="BURRO" 
                                        | Mamífero=="CEBRA" | Mamífero=="MULA" 
                                        | Mamífero=="CAMELLO" | Mamífero=="LLAMA" | Mamífero=="BISONETE")

mean_vector_cluster_1 = colMeans(cluster_1[-1])
mean_vector_cluster_2 = colMeans(cluster_2[-1])
mean_vector_cluster_3 = colMeans(cluster_3[-1])
mean_vector_cluster_4 = colMeans(cluster_4[-1])

# Elijo testear el cluster_2 y cluster_4
cluster_2$Mamífero = 0
cluster_4$Mamífero = 1
new_data_frame = rbind(cluster_2,cluster_4)
hottling_test = hotelling.test(.~ Mamífero, data =new_data_frame) 
hottling_test

