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

# ---------------------- Ejercicio 3 ---------------------- #


# ---------------------- Analisis de discriminante ---------------------- #
data=read_excel('./vehiculos.xls')
data = data.frame(data)
data_without_cols = data %>% select(-c(1,5))
data_group_1 = data %>% filter(data$grave == "si") %>% select(-c(1,5))
data_group_2 = data %>% filter(data$grave == "no") %>% select(-c(1,5))

mean_vector_total = colMeans(data_without_cols)
mean_vector_1 = colMeans(data_group_1)
mean_vector_2 = colMeans(data_group_2)

# Matriz de varianza y covarianza para cada clase
var_1 = var(data_group_1)
var_2 = var(data_group_2)

# Matriz varianza y covarianza comun
# S = ( (total(S1) - 1) * S1 + (tatal(S2) - 1)* S2 ) / (tatal(S1) + tatal(S2) - 2)
var_total = 
  ( (nrow(data_group_1) - 1 ) * var_1 +
      (nrow(data_group_2) - 1 ) * var_2 ) /
  (nrow(data_without_cols) - 2)

# Hotteling
hottling_test = hotelling.test(.~ grave, data =data[,-c(1)]) 
hottling_test

# Hacer shapiro para todas las variables, si una falla entonces niormales multivariadas no son
data_tidy = melt(data[,-c(1)], id.vars = "grave", value.name = "valor")
kable(data_tidy %>% group_by(grave,variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))
kable(data_tidy %>% group_by(grave,variable) %>% summarise(p_value_anderson_darling.test = ad.test(valor)$p.value))

# Normalidad multivariada, p-valor chicos se rechaza la hipo nula, entonces no es normal
mshapiro.test(t(data[,-c(1,5)]))

data_antiguedad = data[-c(1,3,4,5)]
data_edad = data[-c(1,2,4,5)]
data_potencia = data[-c(1,2,3,5)]

shapiro.test(as.numeric(data_antiguedad$antigüedad))
shapiro.test(as.numeric(data_edad$edad.conductor))
shapiro.test(as.numeric(data_potencia$potencia))

boxcox(valor~grave,data=data_tidy,plotit=TRUE, lambda = seq(-2,10,0.5))


# Analizamos igualdad de matrices de varianzas y covarianzas, p-valor chicos
# rechaza la hipo nula, entonces no hay igualdad de varianza
boxM(data = data[,-c(1,5)], grouping = data[,5])





# hasta aca llegue!!!

# Graficos para ver si es linealmente separable
data_with_class = data_without_id
plot(data[,-c(1,5)], pch = 19)



p1 <- ggplot(data = data, aes(x = potencia, fill = grave)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = data, aes(x = edad.conductor, fill = grave)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = data, aes(x = antigüedad, fill = grave)) +
  geom_histogram(position = "identity", alpha = 0.5)
cowplot::plot_grid(p1,p2,p3, nrow = 4, common.legend = TRUE, legend = "bottom")

# Hay que hacer un discriminante cuadratico LDQ
data_model.qda = qda(grave~potencia+edad.conductor+antigüedad, data[,-c(1)])
predictions_qda <- predict(object = data_model.qda, newdata = data[,-c(1)], method = "predictive")
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