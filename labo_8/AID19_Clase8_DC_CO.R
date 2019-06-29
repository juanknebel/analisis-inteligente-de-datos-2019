####################################
####################################
#Ejemplo 1: Muestras normales, independientes con misma varianza

#El dataset "births" del paquete openintro contiene información sobre 150 nacimientos junto con 
#información de las madres. Se quiere determinar si existen evidencias significativas de que el 
#peso (en libras) de los recién nacidos cuyas madres fuman difiere de aquellos cuyas madres no 
#fuman. Se considerarán sólo los casos con peso no inferior a 5 libras (aprox. 2.27 kg) para 
#analizar muestras normales.

library(ggplot2)
library(stats)
library(reshape2)
library(car)
# options(repos = c(CRAN = "http://cran.rstudio.com"))
library(nortest)
install.packages("openintro")
library(openintro)
?births
data(births) 
head(births,4)
births2<-births[which(births$weight>=5),]
dim(births2)

#H0: las medias de los pesos de ambos grupos son iguales
#H1: las medias de los pesos de ambos grupos son distintas

#Estadístico observado:
mean(births2[births2$smoke == "nonsmoker", "weight"]) - mean(births2[births2$smoke == "smoker", "weight"])

ggplot(births2, aes(x = weight)) + 
  geom_histogram(aes(y = ..density.., colour = smoke)) + 
  facet_grid(. ~ smoke) + theme_bw()

qqnorm(births2[births2$smoke == "smoker", "weight"], xlab = "", ylab = "", main = "Pesos de Bebés de Fumadoras") 
qqline(births2[births2$smoke == "smoker", "weight"])

shapiro.test(births2[births2$smoke == "smoker","weight"])

qqnorm(births2[births2$smoke == "nonsmoker", "weight"], xlab = "", ylab = "", main = "Pesos de Bebés de No Fumadoras") 
qqline(births2[births2$smoke == "nonsmoker", "weight"])

shapiro.test(births2[births2$smoke == "nonsmoker","weight"])

ggplot(births2) + geom_boxplot(aes(x = smoke, y = weight, colour = smoke)) + theme_bw()

leveneTest(weight ~ smoke, data = births2)
t.test(x = births2[births2$smoke == "smoker", "weight"], y = births2[births2$smoke == "nonsmoker", "weight"], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)

####################################
####################################
#Ejemplo 2: Muestras apareadas (dependientes)

#Un equipo de atletismo ha decidido contratar a un nuevo entrenador. Para decidir si al cabo de 
#un año mantienen su contrato se selecciona aleatoriamente a 10 miembros del equipo y se 
#cronometran sus tiempos en 100 metros lisos al inicio del año, al final del año se volverá 
#a cronometrar a esos mismos 10 corredores. En vista de los datos obtenidos 
#¿Hay diferencia significativa entre el rendimiento de los corredores tras un año de entrenar 
#con el nuevo instructor?
#Se trata de un caso de estudio en el que las mediciones se realizan sobre los mismos individuos 
#bajo dos condiciones distintas, se trata de datos pareados.

datos <- data.frame(corredor = c(1:10), antes = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3), despues = c(12.7, 13.6, 12, 15.2, 16.8, 20, 12, 15.9, 16, 11.1)) 
head(datos, 4)

diferencia <- datos$antes - datos$despues 
datos <- cbind(datos, diferencia) 
head(datos, 4)

t.test(x = datos$antes, y = datos$despues, alternative = "two.sided", mu = 0, paired = TRUE, conf.level = 0.95)

####################################
####################################
#Ejemplo 3: Tests no paramétricos de Mann Whitney Wilcoxon y de la Mediana para 2 muestras independientes

#Se dispone de dos muestras, de las que no se conoce el tipo de distribución de las poblaciones 
#de origen y cuyo tamaño es demasiado pequeño para determinar si siguen una distribución normal. 
#¿Existe una diferencia significativa entre poblaciones?

muestra1 <- c( 1.1, 3.4, 4.3, 2.1, 7.0 , 2.5 ) 
muestra2 <- c( 7.0, 8.0, 3.0, 5.0, 6.2 , 4.4 )
wilcox.test(x = muestra1, y = muestra2, alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)

#install.packages("RVAideMemoire")
library(RVAideMemoire)
mood.medtest(muestra2~muestra1)
####################################
####################################
#Ejemplo 4: ANOVA de 1 factor

#Analice los datos del experimento para estudiar el efecto del porcentaje de algodón 
#sobre la resistencia a la tensión de una fibra sintética.

library(stats)
library(reshape2)
library(car)
library(nortest)

porcentaje<-c(rep(15,5),rep(20,5),rep(25,5),rep(30,5),rep(35,5))
resistencia<-c(7,7,15,11,9,12,17,12,18,18,14,18,18,19,19,19,25,22,19,23,7,10,11,15,11)
porcAlgodon <-data.frame(porcentaje,resistencia)
attach(porcAlgodon)

porcentaje.f=factor(porcentaje)

plot(resistencia~porcentaje.f)# idem boxplot(resistencia~porcentaje.f)

AOVporcAlgo<- aov(resistencia~porcentaje.f)
summary(AOVporcAlgo )#idem usando anova
#anova(AOVporcAlgo )

###
Analysis of Variance Table

Response: resistencia
Df Sum Sq Mean Sq F value    Pr(>F)    
porcentaje.f  4 475.76  118.94  14.757 9.128e-06 ***
  Residuals    20 161.20    8.06                      
---
  Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
###

#Aplicando ANOVA el p-valor=9.13e-06 es <0.05, por lo tanto, si se verifican 
#los supuestos de normalidad y homogeneidad de la varianza será válido decir 
#que se rechaza que las medias de cada grupo son todas iguales entre sí, es 
#decir, hay al menos un par que difieren, por lo tanto los porcentajes de 
#algodon influyen sobre la resistencia.

Veamos si se cumplen los supuestos necesarios para aplicar ANOVA.
bartlett.test(resistencia,porcentaje.f)

####
Bartlett test of homogeneity of variances

data:  resistencia and porcentaje.f
Bartlett's K-squared = 0.93309, df = 4, p-value = 0.9198
####


leveneTest(resistencia~porcentaje.f)

#Levene's Test for Homogeneity of Variance (center = median)
#Df F value Pr(>F)
#group  4  0.3179 0.8626
#20  

shapiro.test(residuals(AOVporcAlgo ))

Shapiro-Wilk normality test

data:  residuals(AOVporcAlgo)
W = 0.94387, p-value = 0.1818

ad.test(residuals(AOVporcAlgo))

#Anderson-Darling normality test

#data:  residuals(AOVporcAlgo)
#A = 0.51857, p-value = 0.1699

install.packages("moments")
library(moments)
agostino.test(residuals(AOVporcAlgo))

# D'Agostino skewness test

#data:  residuals(AOVporcAlgo)
#skew = 0.10554, z = 0.25734, p-value = 0.7969
#alternative hypothesis: data have a skewness


qqPlot(residuals(AOVporcAlgo),ylab = "residuos", col = "coral",pch = 19, col.lines = "cadetblue")


#Los intervalos de confianza simultáneos para las diferencias de medias 
#de Tukey resultan:

TukeyHSD(AOVporcAlgo,conf.level=0.95)

Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = resistencia ~ porcentaje.f)

$porcentaje.f
diff         lwr        upr     p adj
20-15   5.6   0.2270417 10.9729583 0.0385024
25-15   7.8   2.4270417 13.1729583 0.0025948
30-15  11.8   6.4270417 17.1729583 0.0000190
35-15   1.0  -4.3729583  6.3729583 0.9797709
25-20   2.2  -3.1729583  7.5729583 0.7372438
30-20   6.2   0.8270417 11.5729583 0.0188936
35-20  -4.6  -9.9729583  0.7729583 0.1162970
30-25   4.0  -1.3729583  9.3729583 0.2101089
35-25  -6.8 -12.1729583 -1.4270417 0.0090646
35-30 -10.8 -16.1729583 -5.4270417 0.0000624
####################################
####################################

