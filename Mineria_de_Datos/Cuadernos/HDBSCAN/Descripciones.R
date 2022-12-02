#### Descripción de clusters ####

library(readr)
library(dplyr)
library(FactoMineR)
library(factoextra)


saber11a <- read_delim("./Data/saber11a.txt", 
                       delim = "|", escape_double = FALSE, trim_ws = TRUE)


saber11a = saber11a %>% filter(ESTU_DEPTO_RESIDE == "BOGOTÁ")

df = saber11a %>% select (-c("ESTU_TIPODOCUMENTO", "PERIODO","ESTU_CONSECUTIVO", "ESTU_NACIONALIDAD", # quito la variable nacionalidad. Dejo el departamento y el NSE de la institución
                             "ESTU_ESTUDIANTE","ESTU_PAIS_RESIDE",'ESTU_TIENEETNIA' ,'ESTU_COD_RESIDE_DEPTO', 'ESTU_MCPIO_RESIDE',
                             'ESTU_COD_RESIDE_MCPIO','COLE_CODIGO_ICFES',
                             'COLE_COD_DANE_ESTABLECIMIENTO', 'COLE_NOMBRE_ESTABLECIMIENTO','COLE_COD_DANE_SEDE', 'COLE_NOMBRE_SEDE','COLE_COD_MCPIO_UBICACION', 'COLE_MCPIO_UBICACION',
                             'COLE_COD_DEPTO_UBICACION', 'COLE_DEPTO_UBICACION',
                             'ESTU_PRIVADO_LIBERTAD', 'ESTU_COD_MCPIO_PRESENTACION', "COLE_BILINGUE", "COLE_CARACTER",
                             'ESTU_MCPIO_PRESENTACION',
                             'ESTU_COD_DEPTO_PRESENTACION','PERCENTIL_ESPECIAL_GLOBAL','ESTU_ESTADOINVESTIGACION',
                             'ESTU_TIPOREMUNERACION','ESTU_HORASSEMANATRABAJA','COLE_CALENDARIO',
                             'ESTU_DEPTO_PRESENTACION',  'PERCENTIL_LECTURA_CRITICA', 'DESEMP_LECTURA_CRITICA',
                             'PERCENTIL_MATEMATICAS', 'DESEMP_MATEMATICAS',
                             'PERCENTIL_C_NATURALES', 'DESEMP_C_NATURALES','PERCENTIL_SOCIALES_CIUDADANAS',
                             'DESEMP_SOCIALES_CIUDADANAS','PERCENTIL_INGLES',
                             'DESEMP_INGLES','PERCENTIL_GLOBAL',
                             'PERCENTIL_ESPECIAL_GLOBAL', 'PUNT_INGLES',
                             'ESTU_INSE_INDIVIDUAL', 'ESTU_NSE_INDIVIDUAL',
                             'ESTU_NSE_ESTABLECIMIENTO', 'ESTU_FECHANACIMIENTO'))


# Quitamos los registros incompletos
df = na.omit(df)


# Quitamos aquellos datos que no se pudo obtener su edad en Python


# Quito los datos de saber11 para despejar memoria

rm("saber11a")


# Leo las etiquetas
etiquetas11a =  read_csv("C:/Users/Manuel Vargas/Desktop/UNAL/7. Séptimo semestre/Minería de datos/Proyecto/etiquetasumapfinal.csv")
etiquetas11a = etiquetas11a[,2]
colnames(etiquetas11a) = "eti11"
df = cbind(df,etiquetas11a)

table(df$eti11)
df$eti11 = as.factor(df$eti11)
#df$eti11 = recode(df$eti11, "-1" = "sc")



# empezamos a describir las cosas

descripciones = catdes(df, 36)


### Grupo 0: tamaño= 3706

View(round(descripciones$category$`0`[,1:2],1))

# Carencia de tenencias.
# el 56% de ellos pertenece a generación e-gratuidad
# 60.9% son F
# # el 100% no tiene internet
# 80.2% son personas de estrato 1 y 2
# Puntajes por debajo de la media global

View(round(descripciones$quanti$`0`[,2:3],1))

# Media global:
# Ciudadanas: 51.6
# Matemáticas: 54.4
# Lectura crítica: 56.6
# Naturales 52.4
# Global: 262


## Grupo 1: 3714

View(round(descripciones$category$`1`[,1:2],1))
# Se caracterizan por tenencias. 
# 70.2% son mujeres
# 71.3 son colegios de jornada completa
# 68.4% son colegios femeninos
# El 100% son colegios no mixtos
# 64% automovil
# 71% personas de estrato 3-4

View(round(descripciones$quanti$`1`[,2:3],1))

# Puntajes por encima

# Grupo 2: 3714

View(round(descripciones$category$`2`[,1:2],1))


# el 99.9% tienen más de 100 libros en sus casas
# # 99.8% tienen internet
# colegios mixtos
# Tenencia de bienes
# COlegios no oficiales (70.4%
# 60.5% tienen automóvil
# 59.3 jornada completa


View(round(descripciones$quanti$`2`[,2:3],1))

# Puntajes mayores que la media


# Grupo 3: 5144

View(round(descripciones$category$`3`[,1:2],1))

# 99.1% tienen padre con dueño de negocios
# Tenencias

View(round(descripciones$quanti$`3`[,2:3],1))

# La cantitativa está extraña. Sólo sale puntaje de matemáticas x_x


# grupo 4: 3490

View(round(descripciones$category$`4`[,1:2],1))

# 99.4% son de estrato 4
# 87.6% vienen de colegios no oficiales
# 79.1% tienen automovil
# 78.9% jornada completa
# alto consumo 


View(round(descripciones$quanti$`4`[,2:3],1))

# Puntaje superior al promedio



## Grupo 5: 48500
View(round(descripciones$category$`5`[,1:2],1))
# Mixtos, internet, tv, computador, colegios oficiales

View(round(descripciones$quanti$`5`[,2:3],1))

# Puntajes muy iguales al promedio, bajo


# Grupo -1
View(round(descripciones$category$`-1`[,2:3],1))


# Media por encima del promedio...
df %>% filter(eti11 == "-1") %>% summarise(pg = mean(PUNT_GLOBAL))


     


### Consultas al dataframe



View(df %>% filter(eti11 == "-1" & (FAMI_EDUCACIONPADRE == "Postgrado" | FAMI_EDUCACIONMADRE == "Postgrado")))


# Qué hay de "raro" en los atípicos? Mencionemos algunas cosas interesantes

# 29: Un estudiante es de estrato 2, padres con postgrado, no tienen varias cosas, poco consumo de legumbres...
# Tienen internet pero no computadora
# El puntaje global fue de 249



# 28: Estrato 3, padre con postgrado, madre con educ profesional completa,
# no sabe en qué trabajan sus padres


# 2: Padre con posgrado, madre con técnico. No tienen internet, tienen computador
# pasa más de 3 horas en el internet. Es una familia con 9 o más personas



View(df %>% filter(eti11 == "-1" & PUNT_GLOBAL > 370))


# 6


View(df %>% filter(eti11 == "-1" & PUNT_GLOBAL > 340 & FAMI_ESTRATOVIVIENDA == "Estrato 1"))



View(df %>% filter(eti11 == "-1" & PUNT_GLOBAL > 340 & FAMI_ESTRATOVIVIENDA == "Estrato 2"))


View(df %>% filter(eti11 == "-1" & FAMI_ESTRATOVIVIENDA == "Estrato 6"))

     