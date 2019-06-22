library(ggplot2)
library(stats)
library(reshape2)
library(car)
library(nortest)


###Ejemplo: ANOVA de 1 factor utilizando transformaciones de Box-Cox

#Con la intención de evaluar la eficacia de un medicamento en el nivel de 
#alerta de unos pacientes, tres dosis (a, b, c) de un determinado fármaco
#se administraron a 18 sujetos. Se pide analizar la eficacia del medicamento.

#Tener en cuenta que el Test no paramétrico de Kruskal Wallis no requiere 
#normalidad de los datos, pero sí homogeneidad de la varianza (homocedasticidad).

dosis<-c(rep("a",6),rep("b",8),rep("c",4))
alerta<-c(30,38,35,41,27,24,32,26,31,29,27,35,21,25,17,21,20,19)

data<-data.frame(dosis,alerta)
head(data,8)

#Para analizar la eficacia del medicamento, veamos si existen diferencias 
#entre las medias de las tres dosis. Aplicamos ANOVA.

aov.data<-aov(alerta~dosis,data=data)
summary(aov.data)

#Se puede ver que existen diferencias estadísticamente significativas 
#entre los niveles del fármaco (p=0.00298 <0.05). Para asegurar la validez
# de esta afirmación realizamos las siguientes pruebas diagnósticas.

#La siguiente tabla muestra las medias total y por nivel de los 3 
#medicamentos:

print(model.tables(aov.data,"means"))

#Analizamos normalidad:

qqnorm(resid(aov.data))
qqline(resid(aov.data))

shapiro.test(residuals(aov.data))

Shapiro-Wilk normality test

data:  residuals(aov.data)
W = 0.98604, p-value = 0.991

ad.test(residuals(aov.data))

Anderson-Darling normality test

data:  residuals(aov.data)
A = 0.11805, p-value = 0.987

#Analizamos igualdad de varianzas (homoscedasticidad):

boxplot(split(data$alerta,data$dosis),ylab="Alerta",xlab="Dosis")

#Las varianzas estimadas en cada grupo son:
tapply(data$alerta,data$dosis,var,na.rm=TRUE)

bartlett.test(alerta,dosis)

Bartlett test of homogeneity of variances

data:  alerta and dosis
Bartlett's K-squared = 4.4546, df = 2, p-value = 0.1078

leveneTest(alerta~as.factor(dosis))

Levene's Test for Homogeneity of Variance (center = median)
Df F value  Pr(>F)  
group  2  4.1667 0.03638 *
  15                  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#OJO: Hay que tener en cuenta el tamaño muestral, cuando el tamaño 
#de la muestra es pequeño, incluso grandes desviaciones de la normal 
#no se detectan, y cuando el tamaño de la muestra es grande, incluso 
#la más mínima desviación de la normalidad logra rechazar la hipótesis
#nula. En este caso, al ser la muestra pequeña y el test de Bartlett 
#sensible a las desviaciones de la normalidad, este test no detecta 
#diferencia de varianzas (heterocedasticidad) en los niveles del factor
#(dosis). Por eso, es conveniente utilizar el test de Levene, el cual 
#rechaza la homoscedasticidad, lo que indica que NO se cumple uno de los
#supuestos del ANOVA.

#Para resolver este problema, puede ser útil alguna transformación de 
#Box-Cox:

library(MASS)
boxcox(alerta~dosis,data=data,plotit=TRUE)# el máximo lambda se alcanza 
#en -1.

#Se repite el procedimiento para la variable transformada, y se revisa 
#el cumplimiento de supuestos para aplicar ANOVA.

aov.data2=aov(alerta^(-1)~dosis,data=data)
summary(aov.data2)

Df    Sum Sq   Mean Sq F value   Pr(>F)    
dosis        2 0.0010623 0.0005311   14.64 0.000298 ***
  Residuals   15 0.0005442 0.0000363                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Se obtiene, como con la variable original, diferencias estadísticamente 
#significativas entre los niveles del factor dosis.

#Revisión de supuestos necesarios para aplicar ANOVA

qqnorm(resid(aov.data2))
qqline(resid(aov.data2))

shapiro.test(residuals(aov.data2))

Shapiro-Wilk normality test

data:  residuals(aov.data2)
W = 0.95026, p-value = 0.4291

ad.test(residuals(aov.data))

Anderson-Darling normality test

data:  residuals(aov.data)
A = 0.11805, p-value = 0.987

leveneTest(alerta^(-1)~as.factor(dosis),data=data)

#Levene's Test for Homogeneity of Variance (center = median)
Df F value Pr(>F)
group  2  0.4496 0.6462
15

#Con la transformación de Box-Cox realizada se verifican los supuestos
#necesarios y por lo tanto el resultado del ANOVA aplicado es válido.

#Los intervalos de confianza simultáneos para las diferencias de medias 
#de Tukey resultan:

TukeyHSD(aov.data2,conf.level=0.95)

Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = alerta^(-1) ~ dosis, data = data)

$dosis
diff          lwr        upr     p adj
b-a 0.004324235 -0.004125103 0.01277357 0.4013805
c-a 0.020382789  0.010283900 0.03048168 0.0002760
c-b 0.016058555  0.006477907 0.02563920 0.0015462

####################################
####################################
#Ejemplo: Test no paramétrico de Kruskal Wallis para k muestras independientes

#Un estudio compara el número de huevos que pone un determinado insecto bajo 3 condiciones 
#distintas. ¿Existen diferencias significativas dependiendo de las condiciones?

datos <- data.frame(condicion = c(rep("condicion1", 18), rep("condicion2", 18), rep("condicion3", 18)), n_huevos = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 27, 28, 29, 30, 51, 52, 53, 342, 40, 41, 42, 43, 44, 45, 46, 47, 48, 67, 88, 89, 90, 91, 92, 93, 94, 293, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 25, 36, 37, 58, 59, 60, 71, 72)) 
head(datos)

ggplot(data = datos, mapping = aes(x = condicion, y = n_huevos, colour = condicion)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = datos, mapping = aes(x = n_huevos, colour = condicion)) + 
  geom_histogram() + theme_bw() + facet_grid(. ~ condicion) + 
  theme(legend.position = "none")# + stat_bin(binwidth=30)

leveneTest(n_huevos ~ condicion, data = datos)

kruskal.test(n_huevos ~ condicion, data = datos)

install.packages("pgirmess")
library(pgirmess)
kruskalmc(datos$n_huevos ~ datos$condicion)
####################################
####################################

