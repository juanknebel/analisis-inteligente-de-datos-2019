# Test de independencia

# supongamos que queremos saber si existe relación entre el estado civil de un grupo de personas, y
# su nivel de stress. Contamos con los datos de la siguiente tabla:

#H0: el estado civil es independiente del nivel de stress (para todo i y j P(X=xi,Y=yj)=P(X=xi).P(Y=yj))
#H1: las variables no son independientes (exite al menos algún par i y j tal que P(X=xi,Y=yj)!=P(X=xi).P(Y=yj))

#Contamos con los datos de la siguiente tabla:

CantStressBajo<-c(155,204,103)
CantStressAlto<-c(100,122,183)#c(100,200,283)#c(100,122,183)
Tab<-as.table(rbind(CantStressBajo,CantStressAlto))
dimnames(Tab)<-list(Stress=c("Bajo","Alto"),EstadoCivil=c("Soltero","Casado","Divorciado"))
Tab

# Gráficos útiles

#Gráfico de barras adyacentes
par(bg="lightcyan")
barplot(Tab,beside=TRUE,col= c("aquamarine3","tan1"),ylim=c(0,280),ylab="Cantidad") 
#title("Nivel de stress según estado civil",cex=0.75)
mtext("Nivel de stress según estado civil",cex=1,line=1)
legend("topright",cex=0.8,title="Stress",c("Bajo","Alto"), fill=c("aquamarine3","tan1"),horiz=F, box.lty = 0) 

#Gráfico de mosaicos
mosaicplot(t(Tab),col=c("aquamarine3","tan1"),main="Nivel de stress según estado civil",ylab="Nivel de stress",xlab="Estado civil",
           cex=0.8)

#Aplicación del test Chi-cuadrado:

chisq.test(Tab)


#Analizamos las subtablas de contingencia:
Tab1<-Tab[,-1]
chisq.test(Tab1)

Tab2<-Tab[,-2]
chisq.test(Tab2)

Tab3<-Tab[,-3]
chisq.test(Tab3)

#Veamos algunos outputs que devuelve el comando chisq.test:

names(chisq.test(Tab))

[1] "statistic" "parameter" "p.value"   "method"    "data.name" "observed"  "expected"  "residuals"
[9] "stdres"

chisq.test(Tab)$statistic

chisq.test(Tab)$expected

chisq.test(Tab)$residuals

chisq.test(Tab)$stdres

chisq.test(Tab1)$statistic

chisq.test(Tab1)$expected

chisq.test(Tab1)$residuals

chisq.test(Tab1)$stdres

chisq.test(Tab2)$statistic

chisq.test(Tab2)$expected

chisq.test(Tab2)$residuals

chisq.test(Tab2)$stdres

chisq.test(Tab3)$statistic

chisq.test(Tab3)$expected

chisq.test(Tab3)$residuals

chisq.test(Tab3)$stdres

#######################################

# Test de homogeneidad

# supongamos que queremos saber si existe diferencia entre el color de cabello de tres poblaciones
# pertenecientes a tres ciudades respectivamente. Contamos con los datos de la siguiente tabla:

#H0: el color de cabello es homogéneo en las tres poblaciones/ciudades (para todo i y j P(X=xi|Y=Pobj)=P(X=xi)
#H1: el color de cabello no es homogéneo en todas las poblaciones (exite al menos algún par i y j tal que P(X=xi|Y=yj)!=P(X=xi))

#Contamos con los datos de la siguiente tabla:
Pob1<-c(99,53)#Ciudad Norte
Pob2<-c(105,65)#Ciudad Centro
Pob3<-c(150,48)#Ciudad Sur

TabPob<-as.table(rbind(Pob1,Pob2,Pob3))
dimnames(TabPob)<-list(Ciudad=c("Norte","Centro","Sur"),ColorPelo=c("Rubios","Morochos"))
TabPob

# Gráficos útiles

#Gráfico de barras adyacentes
par(bg="lightcyan")
barplot(t(TabPob),beside=TRUE,col= c("gold","ivory4"),ylim=c(0,280),ylab="Cantidad",xlab="Ciudad") 
mtext("Color de pelo según ciudad",cex=1,line=1)
legend("topright",cex=0.8,c("Rubios","Morochos"), fill=c("gold","ivory4"),horiz=F, box.lty = 0)


#Gráfico de mosaicos
mosaicplot(TabPob,col=c("gold","ivory4"),main="Color de pelo según ciudad",ylab="Color de pelo",xlab="Ciudad",
           cex=0.8)


# Aplicación del test Chi-cuadrado:
chisq.test(TabPob)
chisq.test(TabPob)$expected


#Analizamos las subtablas de contingencia:

TabPob1<-TabPob[-1,]
chisq.test(TabPob1)
chisq.test(TabPob1)$expected

TabPob2<-TabPob[-2,]
chisq.test(TabPob2)
chisq.test(TabPob2)$expected

TabPob3<-TabPob[-3,]
chisq.test(TabPob3)
chisq.test(TabPob3)$expected
