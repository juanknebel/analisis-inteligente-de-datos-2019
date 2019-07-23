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

# ---------------------- Ejercicio 2 ---------------------- #

data_beer = data.frame(
  marca = c(rep("A", 4), rep("B", 4), rep("C", 4), rep("D", 4), rep("E", 4)),
  valor_calorico = c(31.5,32,32.7,30.9,32.3,31.9,33,31.7,28.8,27.6,29.1,26.1,24.5,25.3,24.9,26.1,25.4,26.3,23.9,24.1))

# La hipotesis a testear es si difiere la media del valor calórico
# H0: para todo i,j ui = uj
# H1: existe algun i,j tq ui != uj
# Los supuestos son que las muestras son independientes 
# distribuciones de los residuos son normales
# y homocedasticidad de los residuos

# Anova
data_beer$marca = factor(data_beer$marca)
data_beer.anova = aov(valor_calorico~marca, data_beer)
summary(data_beer.anova)

# box plots
ggplot(data_beer) + geom_boxplot(aes(x = marca, y = valor_calorico, colour = marca)) + theme_bw()

# Homocedasticidad
bartlett.test(data_beer$valor_calorico,data_beer$marca)
leveneTest(data_beer$valor_calorico,data_beer$marca)

# Normalidad
shapiro.test(residuals(data_beer.anova))
ad.test(residuals(data_beer.anova))

# Tukey
tukey.test = TukeyHSD(data_beer.anova)
tukey.test
