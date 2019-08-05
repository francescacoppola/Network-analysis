install.packages("qdap")
install.packages("tidyr")
library(data.table)
library(rpart)
library(rpart.plot)
library (caret)
library(odbc)
library(DBI)
library(dplyr)
library(foreign)
library(plyr)
library(qdap)
library(tidyr)

print("se cargaron las librerias")

# Clear workspace and call the data
getwd()
setwd("C:/Users/fcoppola/Downloads/Microsoft.SkypeApp_kzf8qxf38zg5c!App/All")
JDE_DS <- readRDS("dsPrincipaltodo.rds")

head(JDE_DS, 40)
print("los 40 primeros")


#Duplicamos la data
tabla <- JDE_DS
print("se duplico la data")

#Filtro por el tipo de actividad shipconfirmation 
tabla <- filter(tabla, ActivityType %in% c("05. Ship Confirmation"))
###View(tabla2)

#Filtrar por transitionMinutes para ver cuantos datos perdemos
tabla2 <- filter (tabla, TransitionMinutes >0)
View(tabla2)

#Filtramos por Actividad para reducir mas el set de datos y quedarnos con un solo registro por actividad 565-shipconfirmation
tabla3 <- filter(tabla2, Activity == "565-Ship Confirmation")
#Obtenemos 733893 registros

#sin odenes sin envio
nueva <- tabla3 %>% drop_na(SDADDJ)

#--- selección de solo 11 variables
nueva1 <- data.frame(nueva)
nueva2 <- c("BusinessUnit","SDADDJ", "SLROUT","SDLITM", "SLTDAY", "SDUORG", "SDTRDJ")
nueva3 <- nueva[nueva2]

#vemos la tabla y sabemos las variables seleccionamos solo 100 de prueba
a <- nueva3[1:100,]
names(a)
colnames(nueva3) <- c("Origen", "FechaEnvio", "Destino", "Producto", "Horaenvio", "CantidadProducto", "Fechapedido")
names(nueva3)
table(nueva3$Origen)
table(nueva3$Destino)

nueva4 <- nueva3 %>% drop_na(Destino)
replace(nueva3, Origen=="BED001"|datos=="c2"|datos=="c3","c")

write.csv(nueva4, "DatosproyectoV1.csv")

table(nueva$SDUORG)
table(nueva$SLUNCS)
table(nueva$SDSHAN)
table(nueva$SLECST)
#creamos el archivo csv
write.csv(tabla3, "Datosredes.csv")

#me quedo solo con las columnas que me interesan 
#pido los nombres de las columnas
names(tabla3)
data<-tabla3[,c("BusinessUnit", "SLROUT")]
3

#CREAR un contador en una columna nueva para cada par de interacciones 
View(data)

#Contamos la frecuencia de la dupla de variables
data2 <- count(data)
View(data2)

data3 <- data2[complete.cases(data2), ]
write.csv(data2, "Datosredes2.csv")

