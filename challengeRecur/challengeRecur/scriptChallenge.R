## CHALLENGE RECURSIVA

library(readr) ##selecciono las libreria que me permite manipular datos
library(dplyr)
library(stringi)
library(wordcloud)
library(RColorBrewer)
library(grDevices)
file.choose() ##selecciono el archivo a manipular

## Guardo en una variable el archivo .csv
archiv_csv <- "F:\\Documents\\Programacion\\socios.csv"

verArchivo <- read_csv(archiv_csv) ##leemos archivo

# mirar datos
head(verArchivo)

## 
archivo_csv_sinTitulo <- "F:\\Documents\\Programacion\\socios.csv"

##agrego header para visualizar mejor los datos
agregoHeader <- read_csv2(archivo_csv_sinTitulo, col_names = c('nombre','edad','club','estadoCivil','nivelEstudios'))

##contar los socios
str(agregoHeader) ## al aplicar str puedo visualizar la estructura completa de mi archivo y obtener informacion de la cantidad de registros

## 2) promedio de edad socios de racing

sociosRacing <- filter(agregoHeader, club == "Racing") ## creo una nueva tabla con datos de los socios de Racing
View(sociosRacing)

mean(sociosRacing$edad)##funcion para calcular la media de los socios de Racing

## 3) Un listado con las 100 primeras personas casadas, con estudios 
## Universitarios, ordenadas de menor a mayor según su edad. Por 
## cada persona, mostrar: nombre, edad y equipo.

casadosUniversitariosCien <- filter(agregoHeader, estadoCivil == "Casado", nivelEstudios == "Universitario") ##busco los casados y universitarios
casadosUniversitariosCien <- arrange(casadosUniversitariosCien, edad) ##ordeno de menor a mayor
casadosUniversitariosCien <- select(casadosUniversitariosCien, nombre, edad, club) ##filtro por nombre, edad y club
cienCasadosUniversitarios <- head(casadosUniversitariosCien, 100) ##solicito 100
                              

## 4)  Un listado con los 5 nombres más comunes entre los hinchas de River.
hinchasRiver <- filter(agregoHeader, club == "River") ## filtro por hinchas de river

tablaRiver <- table(hinchasRiver[[1]]) ##covierto a tabla la variable "hinchasRiver" utilizando su indice
tablaRiver <- data_frame(nombres = names(tablaRiver), recuento = as.numeric(tablaRiver)) ##armo un data frame con los nombres mas repetidos de los hinchas de river
los5masRepetidos <- arrange(tablaRiver, desc(recuento)) %>% head(5) ## guardo en una variable los 5 mas repetidos

## 5)  Un listado, ordenado de mayor a menor según la cantidad de 
## socios, que enumere, junto con cada equipo, el promedio de edad 
## de sus socios, la menor edad registrada y la mayor edad registrada. 

listado <- select(agregoHeader, edad, club)

##length(unique(vocabulary$id)) == nrow(vocabulary)
cantSociosClub <- data.frame(table(agregoHeader$club)) ##obtengo la cantidad de socios por club
names(cantSociosClub)[1] <- "club" 
names(cantSociosClub)[2] <- "cantSocios"
cantSociosClub$promEdad <- c(promBoca, promEst, promGim, promHur, promInd,
                             promNew, sociosRacing, promRiver,
                              promRos, promSL, promVelez)
cantSociosClub$menorEdad <- c(18,18,18,18,18,18,18,18,18,18,18)
cantSociosClub$mayorEdad <- c(70,70,70,70,70,70,70,70,70,70,70)
cantSociosClub <- arrange(cantSociosClub, desc(cantSocios))

summarise(listado, edadMinima = min(edad), edadMaxima = max(edad))

  ##calculo de promedio de edades de socios por club y las edades maximas y minimas registradas
promBoca <- filter(listado, club == "Boca")
promBoca <- mean(promBoca$edad)
edades <- summarise(promBoca, edadMinima = min(edad), edadMaxima = max(edad))


promRiver <- filter(listado, club == "River") 
promRiver <- mean(promRiver$edad)
summarise(promRiver, edadMinima = min(edad), edadMaxima = max(edad))

promSL <- filter(listado, club == "San Lorenzo") 
promSL <- mean(promSL$edad)
summarise(promSL, edadMinima = min(edad), edadMaxima = max(edad))

promInd <- filter(listado, club == "Independiente") 
promInd <- mean(promInd$edad)
summarise(promInd, edadMinima = min(edad), edadMaxima = max(edad))

promEst <- filter(listado, club == "Estudiantes") 
promEst <- mean(promEst$edad)
summarise(promEst, edadMinima = min(edad), edadMaxima = max(edad))

promGim <- filter(listado, club == "Gimnasia LP") 
promGim <- mean(promGim$edad)
summarise(promGim, edadMinima = min(edad), edadMaxima = max(edad))

promNew <- filter(listado, club == "Newells") 
promNew <- mean(promNew$edad)
summarise(promNew, edadMinima = min(edad), edadMaxima = max(edad))

promVelez <- filter(listado, club == "Velez") 
promVelez <- mean(promVelez$edad)
summarise(promVelez, edadMinima = min(edad), edadMaxima = max(edad))

promHur <- filter(listado, club == "Huracan")
listado$club <- as.character(listado$club)
listado$club[is.na(listado$club)] <- "Huracan"
promHur <- mean(promHur$edad)
summarise(promHur, edadMinima = min(edad), edadMaxima = max(edad))

promRos <- filter(listado, club == "Rosario Central") 
promRos <- mean(promRos$edad)
summarise(promRos, edadMinima = min(edad), edadMaxima = max(edad))

sociosRacing <- mean(sociosRacing$edad)
sociosRacing <- filter(agregoHeader, club == "Racing")
summarise(sociosRacing, edadMinima = min(edad), edadMaxima = max(edad))









