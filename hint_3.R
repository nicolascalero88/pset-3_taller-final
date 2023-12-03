
## initil setup
rm(list=ls())
require(pacman)
p_load(tidyverse,rio,data.table)

## Punto 1.1
rutas <- list.files("pset-3/input" , recursive=T , full.names=T)

## Punto 1.2 

## Extraer las ruitas
rutas_resto <- str_subset(string = rutas , pattern = "Resto - Ca")

## Cargar en lista
lista_resto <- import_list(file = rutas_resto)

## Textear la cadena de caracteres 
rutas_resto[1]
str_sub(rutas_resto[35],start = 14 , 17)

## Agregar ruta
View(lista_resto[[1]])
lista_resto[[1]]$path <- rutas_resto[1]
  
## Aplicar loop  
for (i in 1:length(lista_resto)){
     lista_resto[[i]]$path <- rutas_resto[i]  
     lista_resto[[i]]$year <- str_sub(lista_resto[[i]]$path,start = 14 , 17)
}
View(lista_resto[[20]])

## Punto 1.3
lista_resto[[36]] <- NULL
df_resto <- rbindlist(l=lista_resto , use.names=T , fill=T)

## export
export(df_resto,"pset-3/output/db_full.rds")



