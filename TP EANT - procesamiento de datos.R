library(tidyverse)
library(fastDummies)
library(lubridate)
setwd("~/Mis Cursos/Curso EANT/Proyecto Final - EANT - Data Science")
#Descargo el dataset 2018 y 2019####
df <-  read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv',header = T,sep = ',')
df_2018 <-df %>% 
  filter(is.na(df$lat)!= TRUE ) %>% #le saco los N/A's
  filter(franja_horaria %in% c(0:23))#le saco todo lo que no sea un numero

df2 <-  read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv',header = T,sep = ',')
df_2019 <-df2 %>% 
  filter(is.na(df2$lat)!= TRUE ) %>% #le saco los N/A's
  filter(franja_horaria %in% c(0:23))#le saco todo lo que no sea un numero
# Le sumo la variable de comisarias al dataset de 2018#####
comisarias <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv',
                       header = T,sep = ',')

euclidean <- function(a, b){ sqrt(sum((a - b)^2))}

a <- c()
for (i in c(1:dim(df_2018)[1])) {
  for (j in c(1:dim(comisarias)[1])) {
    lista = c()
    lista = c(lista,euclidean(c(df_2018$lat[i],df_2018$long[i]),
                              c(comisarias$lat[j],comisarias$long[j])))
  }
  a <-  c(a,min(lista))
}
df_2018$comisarias <- a

# Le sumo la variable de comisarias al dataset de 2019#####

a <- c()
for (i in c(1:dim(df_2019)[1])) {
  for (j in c(1:dim(comisarias)[1])) {
    lista = c()
    lista = c(lista,euclidean(c(df_2019$lat[i],df_2019$long[i]),
                              c(comisarias$lat[j],comisarias$long[j])))
  }
  a <-  c(a,min(lista))
}
df_2019$comisarias <- a

# Le sumo la variable de tren al dataset de 2018 #### 
tren <- read.csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-ferrocarril/estaciones-de-ferrocarril.csv',
                       header = T,sep = ',')

euclidean <- function(a, b){ sqrt(sum((a - b)^2))}

a <- c()
for (i in c(1:dim(df_2018)[1])) {
  for (j in c(1:dim(tren)[1])) {
    lista = c()
    lista = c(lista,euclidean(c(df_2018$lat[i],df_2018$long[i]),
                              c(tren$lat[j],tren$long[j])))
  }
  a <-  c(a,min(lista))
}
df_2018$tren <- a

write.csv(x = df_2018,"df_2018.csv",sep = ",",dec = ",")



# Le sumo la variable de tren al dataset de 2019#####

a <- c()
for (i in c(1:dim(df_2019)[1])) {
  for (j in c(1:dim(tren)[1])) {
    lista = c()
    lista = c(lista,euclidean(c(df_2019$lat[i],df_2019$long[i]),
                              c(tren$lat[j],tren$long[j])))
  }
  a <-  c(a,min(lista))
}
df_2019$tren <- a

write.csv(x = df_2018,"df_2019.csv",sep = ",",dec = ",")