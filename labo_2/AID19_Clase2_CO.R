#--------------------------------------------------------
# data.frames 
#--------------------------------------------------------
options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages(Rcpp)

Nombre = c("Ana","Luis","Pedro","Juan","Eva","Jorge") # crea un vector con los nombres
Edad = c(23,24,22,24,25,27) # crea un vector con las edades correspondientes 
Sexo = as.factor(c("F",rep("M",3),"F","M")) # crea un vector como factor con el sexo correspondiente 
levels(Sexo) # devuelve los grupos del vector dado como factor
datos=data.frame(Nombre,Edad,Sexo) # arma un entorno de datos
datos 
mean(datos$Edad[datos$Sexo=="F"]) # devuelve el promedio de la edad de las mujeres

table(datos[[3]]) # devuelve una tabla de frecuencias del factor Sexo

dfr<-data.frame("Dia"=c(1:5),"Mes"=rep("Abril",5))
dfr[,1]
dfr$Mes
class(dfr)#devuelve la clase, es decir el tipo de objeto


#--------------------------------------------------------
# listas
#--------------------------------------------------------
u=c(3,8,2,7,3,2,1)
u[u==3]<-4 # en esas posiciones asigna un 4   
u[which(u>=4)]<-0 # asigna 0 a esas posiciones
u

data=1:10
mat1=matrix(data,nrow=2,ncol=5) # asigna valores a una matriz
colnames(mat1)<-c("A","B","C","D","E") # asigna nombres a las columnas de la matriz
rownames(mat1)<-c("2015","2016") # asigna nombres a las filas de la matriz
mat2<-matrix(seq(10,1),nrow=2,byrow=T)
mat3=mat1%*%t(mat2)
mat3

vec1=seq(2,5)

milista<-list(c(T,F),dfr,u,"curso R",mat3) # genera una lista de objetos
milista # devuelve la lista creada
names(milista)<-c("valores de verdad","df","vector u","título","matriz 3") # asigna nombres a los elementos de la lista
milista # devuelve la lista con nombres 
milista2=list("a"=3,"b"=mat3,"c"=vec1) # genera otra lista
milista2 # devuelve la otra lista
class(milista2)#devuelve la clase, es decir el tipo de objeto

names(milista)
milista$título # devuelve el elemento guardado según el nombre
milista[[2]] # devuelve el elemento guardado según la posición
length(milista) # devuelve la cantidad de elementos de la lista
milista$a<-7 # agrega una componente al final de la lista
milista # devuelve la lista modificada
milista[[2]]<-mat1 # reasigna un valor de una componente según la posición
milista # devuelve la lista modificada

split(datos,Sexo) # particiona un entorno de datos a partir del factor Sexo
datos2=data.frame(datos,"Nación"=as.factor(c(rep("arg",3),rep("per",3)))) # agrega información al entorno de datos
datosmujeres=split(datos2,datos2$Sexo)[[1]] # almacena los datos correspondientes a la partición por mujeres
split(datosmujeres,datosmujeres$Nación) # particiona los nuevos datos por el factor Nación; es decir, se ha particionado un data frame por dos factores


#---------   generación de muestras de distribuciones conocidas------------

muestra.unif1=runif(100) # genera una muestra uniforme en [0,1] de 100 datos
muestra.unif1 # devuelve la muestra generada
muestra.unif2=runif(200,min=2,max=5) # genera una muestra uniforme en [2,5] de 200 datos
muestra.unif2 # devuelve la muestra generada
muestra.norm.est=rnorm(30) # genera una muestra normal estándar de 30 datos
muestra.norm.est # devuelve la muestra generada
muestra.norm=rnorm(50,mean=10,sd=3) # genera una muestra normal(13,3) de 50 datos
muestra.norm # devuelve la muestra generada
muestra.gamma=rgamma(40,rate=2, shape=3) # genera una muestra gamma(2,3) de 40 datos
muestra.gamma # devuelve la muestra generada
muestra.f=rf(80,df1=5,df2=6) # genera una muestra F de Snedekor(5,6) de 80 datos
muestra.f # devuelve la muestra generada
muestra.exp=rexp(90,2) # genera una muestra exponencial(2) de 90 datos
muestra.exp # devuelve la muestra generada
muestra.chi=rchisq(70,df=4) # genera una muestra chi cuadrado con 4 grados de libertad de 70 datos
muestra.chi # devuelve la muestra generada


#--------------------------------------------------------
#-------------------------- Ejemplo IMCinfantil -------------------------
#--------------------------------------------------------

# install.packages("readxl")# instala la librería
library(readxl)# llama a la librería
# read.csv2("C:/Users/ceci/AID/Datos/IMCinfantil.csv")# importa el archivo con la base de datos
# IMCinfantil<-read.csv2("C:/Users/ceci/AID/Datos/IMCinfantil.csv") # guarda la base de datos bajo un nombre

# Cargo la base recortada:
PACIENTE<-1:30
EDAD<-c(7, 7, 8, 7, 7, 10,7 ,7, 7 ,9, 9, 11, 7, 9,  9, 11, 12, 7, 11,  6,  8,  8,  7, 10,  7,  8, 10,  7,  9, 10)
SEXO<-c("M", "M", "M", "F", "M", "M" ,"M", "M", "M", "M", "M", "F", "M" ,"M" ,"F" ,"M", "M" ,"M" ,"M" ,"F" ,"F" ,"F", "F","F", "M" ,"M" ,"F", "F" ,"F" ,"M")  
PESO<-c(24.4, 23.6, 47.0, 24.0, 23.9, 41.0, 32.9, 22.4, 28.7, 31.4, 28.9, 51.2, 26.2, 58.5, 23.7, 25.5, 49.7, 39.6,42.5, 21.6, 38.0, 26.6, 20.4, 23.7, 21.4, 45.7, 51.3, 28.0, 26.9, 43.9)
TALLA<-c(1.2, 1.2, 1.4, 1.2, 1.2, 1.4, 1.3, 1.2, 1.3, 1.3, 1.3, 1.6, 1.3, 1.5, 1.3, 1.3, 1.7, 1.3, 1.5, 1.2, 1.3, 1.2, 1.2,1.3, 1.2, 1.4, 1.5, 1.3, 1.3, 1.5)
IMC<-c(16.94444, 16.38889, 23.97959, 16.66667, 16.59722, 20.91837, 19.46746, 15.55556, 16.98225, 18.57988,17.10059, 20.00000, 15.50296, 26.00000, 14.02367, 15.08876, 17.19723, 23.43195, 18.88889, 15.00000,22.48521, 18.47222, 14.16667, 14.02367, 14.86111, 23.31633, 22.80000, 16.56805, 15.91716, 19.51111)
PIMC<-c(7.97, 72.72, 97.08, 83.88, 45.85, 87.33, 96.57, 32.88, 80.77, 92.72, 55.54, 77.77, 70.70, 98.69,  3.25,2.07, 38.08, 98.75, 80.60, 39.97, 96.07, 71.06,  3.44,  2.02, 56.86, 98.99, 90.84, 57.50, 44.77, 84.89)
CC<-c(54, 52, 76, 63, 56, 78, 69, 52, 60, 69, 60, 75, 50, 88, 58, 73, 75, 76, 72, 52, 76, 54, 52, 56, 56, 78, 76, 57, 57, 76)
CatPeso<-c("N",  "N",  "OB", "N",  "N",  "SO", "OB", "N",  "N",  "SO", "N",  "N",  "N",  "OB", "D",  "D",  "N",  "OB","N" , "N",  "OB", "N",  "D",  "D",  "N",  "OB", "SO", "N",  "N",  "N" )
IMCin<-data.frame(PACIENTE,EDAD,SEXO,PESO,TALLA,IMC,PIMC,CC,CatPeso)
View(IMCin)

head(IMCin) # muestra las seis primeras filas de datos y los nombres de las columnas
tail(IMCin)# muestra las seis últimas filas de datos y los nombres de las columnas

dim(IMCin)#30 9

table(IMCin$SEXO) # devuelve las frecuencias absolutas de las categorías de la variable
100*table(IMCin$SEXO)/length(IMCin$SEXO) # calcula las frecuencias porcentuales
sal.sexo=rbind(table(IMCin$SEXO),100*table(IMCin$SEXO)/length(IMCin$SEXO)) # combina las dos frecuencias en una salida
rownames(sal.sexo)=c("frec.abs","frec.porc") # asigna nombre a las filas de la salida
colnames(sal.sexo)=c("Femenino", "Masculino") # asigna nombre a las columnas de la salida
sal.sexo # muestra la salida
round(sal.sexo,2) # redondea la salida a dos dígitos decimales

table(IMCin$CatPeso)
100*table(IMCin$CatPeso)/length(IMCin$CatPeso)
sal.catpeso=rbind(table(IMCin$CatPeso),100*table(IMCin$CatPeso)/length(IMCin$CatPeso))
rownames(sal.catpeso)=c("frec.abs","frec.porc")
levels(IMCin$CatPeso) # devuelve las categor?as ordenadas alfab?ticamente
colnames(sal.catpeso)=c("Deficiente","Normal","Obeso","Con sobrepeso")
sal.catpeso 
round(sal.catpeso,2)

dist.conj=table(IMCin$CatPeso, IMCin$SEXO) # devuelve la distribución conjunta 
total=apply(dist.conj,2,sum) # calculo los totales por sexo
dist.porc=round(100*cbind(dist.conj[,1]/total[1],dist.conj[,2]/total[2]),2) # calcula la distribución porcentual por sexo
colnames(dist.porc)<-c("F(%)","M(%)") # asigna nombre a las columnas de la distribución porcentual
sal.conj=cbind(dist.conj,dist.porc) # combina ambas distribuciones
Totales=apply(sal.conj,2,sum) # calcula el total de cada columna
sal.fin=rbind(sal.conj,Totales) # agrega una fila con los totales
sal.fin # muestra la salida final

install.packages("BiocManager")# necesario para usar el paquete modeest
library("BiocManager")
BiocManager::install("genefilter")# necesario para usar el paquete modeest
install.packages("modeest")
library(modeest) # llama a la librería

mfv(IMCin$EDAD) # calcula la moda de la edad
mfv(IMCin$TALLA) # calcula la moda de la talla
imc.base=cbind(IMCin [,c(2,4:6,8)])# arma una base seleccionando variables numéricas
Z= imc.base[,3]*100 # guarda los datos de la talla en centímetros
mean(Z) # calcula la media
mean(Z, trim=0.1) # calcula la media podada al 10%
mean(Z,trim=0.5) # Calcula la media podada al 50%
median(Z) # calcula la mediana 

c.var=function(x){ 100*sd(x)/mean(x)} # define la función de coeficiente de variación
c.var(Z) # aplica la función 

quantile(Z, 0.75) # calcula el cualtil 75
quantile(IMCin$EDAD, probs = seq(0, 1, 0.2)) # calcula los cuantiles 0, 20, 40, 60, 80 y 100
quantile(IMCin$TALLA, probs = seq(0, 1, 0.25)) # calcula los cuantiles 0, 25, 50, 75 y 100
DI.edad=quantile(IMCin$EDAD,0.75)-quantile(IMCin$EDAD,0.25)# calcula la desviación intercuartil
DI.edad
DI.Z=quantile(Z,0.75)-quantile(Z,0.25)
DI.Z

mad(Z, constant = 1) # calcula la mediana de los valores absolutos de los desvíos
mad(Z, constant = 1.4826) # multiplica lo anterior por la constante 1.4826
mad(Z) # toma por default la constante 1.4826

abs(mad(Z)-sd(Z)) # mide el apartamiento de normalidad
100*(abs(mad(Z)-sd(Z)))/sd(Z) # calcula el procentaje del apartamiento de normalidad
set.seed(1512)
muestra.norm=rnorm(1000) # genera datos con distribucion normal
sd(muestra.norm) # calcula el desvio estandar
mad(muestra.norm) # calcula el MAD

is.na(Z) # indica los valores que faltan
W<-Z 
W[1]<-NA # asigna un valor perdido en la primera componente del vector W
is.na(W)
mean(W) # devuelve NA
mean(W,na.rm=T) # no considera los valores no disponibles
mean(na.omit(W)) # equivalente al anterior
median(W,na.rm=T) # otra funcion que requiere excluir los valores no disponibles
sd(W,na.rm=T) # otra funcion que requiere excluir los valores no disponibles

base.split=split(IMCin[,c(2,4:6,8)],IMCin$SEXO) # arma una base numérica dividida según el sexo
mujeres=as.data.frame(base.split[[1]]) # guarda la primera subclase (F) como dataframe
varones=as.data.frame(base.split[[2]]) # guarda la segunda subclase (M) como dataframe
lapply(mujeres,"summary") # devuelve una lista aplicando el resumen de todas las variables de la base
unlist(lapply(mujeres,"summary")) # transforma la salida de lista a vector
matrix(unlist(lapply(mujeres,"summary")),nrow=5,ncol=6,byrow=T) # acomoda los elementos del vector en una matriz de modo tal que cada variable ocupe una fila
sumary.muj=matrix(unlist(lapply(mujeres,"summary")),nrow=5,ncol=6,byrow=T) # guarda los resúmenes estadísticos
sd.mujeres=round(unlist(lapply(mujeres,"sd")),2)  # calcula el desvío estándar de las variables de interés redondeado a dos dígitos decimales
salida.muj=cbind(sumary.muj[,4],sd.mujeres,sumary.muj[,c(3,2,5)]) # combina las columnas indicadas
colnames(salida.muj)<-c("MEDIA","SD","MEDIANA","Q1","Q3") # pone nombre a las columnas
salida.muj # devuelve la salida
write.table(salida.muj,"sal_muj.xls",col.names=NA, row.names=TRUE, sep="\t",quote=FALSE) # exporta la salida en un archivo excel separado por tabulaciones

