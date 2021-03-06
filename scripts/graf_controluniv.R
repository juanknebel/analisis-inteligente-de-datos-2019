library(ggplot2) # Paquete para confeccionar dibujos
library(dplyr) # Paquete para manipular datos
library(readxl) # Permite leer archivos xlsx

datos=read_excel("C:/.../controlunivariado.xlsx")
# Importa la base con la cual se va a trabajar
attach(datos) # Se pone la base en la memoria

dat=datos %>% group_by(Obs, Clase) # Regrupa la base
exp_names <- c(`A`="Bajo control", `B`="Fuera de control",
               `C`="Fuera de control") # Cambia etiquetas

ggplot(dat, aes(x=Obs, y= Valor, group=Clase, colour=Clase)) +
  facet_wrap(~Experimento, labeller=as_labeller(exp_names)) +
  geom_point() +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_hline(yintercept= 3, linetype="dashed") +
  xlab("Observaciones") +
  ylab("") +
  theme(legend.position="none")+
  scale_color_manual(values=c("royalblue", "indianred3"))
# Produce un diagrama
  