#Taller de R: Estadística y Programación
#                                 Taller        4
#                                   24/11/2023
#Integrante: Nicolas Calero-202122051


## limpiamos el area de trabajo
rm(list=ls())
#Cargamos la libreria
#En dado caso de no tenerla instalada la descargamos
#install.packages('pacman') 
require(pacman)
pacman::p_load(tidyverse,rio,data.table)

## Punto 1.1
#indicamos el directorio con el que vamos a trabajar o tenemos nuestras bases de datos
setwd('C:/Users/Julia/Downloads/Taller_Rstudio_problem3_Nicolas_Calero/problem-sets-main')
#fijamos la ruta con la cual vamos a llamar nuestras bases de datos
rutas <- list.files("pset-3/input" , recursive=T , full.names=T)

## Punto 1.2 

## Extraer las rutas
#Usamos el patron Resto- Ca para extraer las bases de datos que deseamos analizar
rutas_resto <- str_subset(string = rutas , pattern = "Resto - Ca")

## importamos la lista que pertenece a cada uno de los meses con el patron de la ruta que fijamos
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

## exportamos nuestra base de datos transformada
export(df_resto,"pset-3/output/db_full.rds")

#cargamos la libreria para generar la visualizacion de nuestras graficas
require(ggplot2)
#con la finalidad de identificar el nombre de las variables usamos el diccionario de GEIH-2021 con el siguiente link https://microdatos.dane.gov.co/index.php/catalog/701/data-dictionary/F2?file_name=%C3%81rea%20-%20Caracter%C3%ADsticas%20generales%20(Personas)
#P6040 == ¿cuántos años cumplidos tiene...? (si es menor de 1 año, escriba 00)
#P6081 == ¿El padre de ... reside en este hogar?

# Renombrar las columnas
names(df_resto)[names(df_resto) == "P6040"] <- "edad"
names(df_resto)[names(df_resto) == "P6081"] <- "vive_con_padre"
# Crear una nueva columna para la leyenda
df_resto$leyenda <- ifelse(df_resto$afiliado_cotizante_o_seguridadsocial == 1, "Sí",
                           ifelse(df_resto$afiliado_cotizante_o_seguridadsocial == 2, "No",
                                  ifelse(df_resto$afiliado_cotizante_o_seguridadsocial == 3, "Fallecido", "Otro")))

# Crear el gráfico utilizando ggplot
grafica1<- ggplot(df_resto, aes(x = edad, y = vive_con_padre)) +
  geom_point() +  # Puedes utilizar diferentes tipos de gráficos como geom_line(), geom_bar(), etc.
  labs(x = "Edad", y = "Vive con Padre") +  # Etiquetas de los ejes
  ggtitle("Gráfico de Edad vs Vive con Padre") + # Título del gráfico
  geom_text(data = data.frame(x = c(30, 30, 30), y = c(1, 2, 3), leyenda = c("Sí", "No", "Fallecido")),
            aes(x = x, y = y, label = leyenda), size = 6)  # Agregar etiquetas
#Exportamos la grafica generada, en la cual vemos que son pocas las personas que viven con ambos padres
ggsave("edad_vive-padre.png", plot = grafica1, width = 6, height = 4)

#P6090 == ¿... Está afiliado, es cotizante o es beneficiario de alguna entidad de seguridad social en salud?
# Renombrar las columnas
names(df_resto)[names(df_resto) == "P6040"] <- "edad"
names(df_resto)[names(df_resto) == "P6090"] <- "afiliado_cotizante_o_seguridadsocial"

# Crear una nueva columna para la leyenda
df_resto$leyenda <- ifelse(df_resto$afiliado_cotizante_o_seguridadsocial == 1, "Sí",
                        ifelse(df_resto$afiliado_cotizante_o_seguridadsocial == 2, "No",
                               ifelse(df_resto$afiliado_cotizante_o_seguridadsocial == 9, "No Sabe", "Otro")))


# Crear el gráfico utilizando ggplot
grafica2<- ggplot(df_resto, aes(x = edad, y = afiliado_cotizante_o_seguridadsocial)) +
  geom_point() +  # Puedes utilizar diferentes tipos de gráficos como geom_line(), geom_bar(), etc.
  labs(x = "Edad", y = "filiado, cotizante o tiene seguridad social") +  # Etiquetas de los ejes
  ggtitle("Gráfico de Edad vs afiliado, cotizante o tiene seguridad social") +  # Título del gráfico
  geom_text(data = data.frame(x = c(30, 30, 30), y = c(1, 2, 9), leyenda = c("Sí", "No", "No Sabe")),
            aes(x = x, y = y, label = leyenda), size = 6)  # Agregar etiquetas
#Exportamos la grafica generada, en la cual vemos que son pocas las personas que viven con ambos padres
ggsave("edad_afiliado-cotizante-seguridadsocial.png", plot = grafica2, width = 6, height = 4)










