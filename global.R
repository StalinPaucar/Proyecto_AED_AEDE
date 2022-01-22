#library(conflicted)#evitar errores en funciones con mismo nombre(librerias distintas)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(openair)
library(stringi)#remover acentos
#library(maptools)
library(modeest)
library(moments)
library(hms)
library(ggplot2)
library(ggseas)
library(viridis)
library(shadowtext)
library(plotly)
library(ggpubr)
library(Metrics)
library(nortest)
library(rgdal)
library(gstat)
library(rgeos)

#library(gridExtra)
#library(RColorBrewer)#para ver colores disponibles: display.brewer.all()
#library(leaflet)
#library(raster)
#library(dismo)
#library(broom)#no es
#library(funModeling)
#library(DT)
#conflict_prefer("filter", "dplyr")#evita error en funcion duplicada (filter)
#conflict_prefer("skewness", "moments")

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||   funciones personalizadas  ||||||||||||||||||||||||||||||||||
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#Estadisticos descriptivos personalizados
AD <- function(dat){
  Mean <- round(mean(dat, na.rm = TRUE),digits = 2)
  Median <- round(median(dat, na.rm = TRUE),2)
  Mode <- round(mfv1(dat,na_rm = TRUE),2) #necesario modeest
  Stdev <- round(sd(dat, na.rm = TRUE),2)
  Skew <- round(moments::skewness(dat),2)#necesario moments
  Kurt <- round(kurtosis(dat),2)#necesario moments
  Min <- round(min(dat),2)
  Max <- round(max(dat),2)
  Range <- round((Max - Min),2)
  Quant1 <- round(quantile(dat)[2],2)
  Quant3 <- round(quantile(dat)[4],2)
  result <- data.frame(Mean,Median,Mode,Stdev,Skew,Kurt,Min,Max,Range,Quant1,Quant3,row.names = variable.names(dat))
  return(result)
}

#aplicar una funcion a cada columna de un data frame
fun_df <- function(data,func){
  nam <- colnames(data)
  #frame <- as.tibble(NA)
  frame <- c()
  for (i in 1:length(data)) {
    col <- func(data[[i]])
    frame <- rbind(frame,col)
  }
  #row.names(frame) <- nam
  frame <- as.data.frame(frame)
  return(frame)
}
#intervalos acf ggplot
ic_alpha <- function(alpha, acf_res){
  return(qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n.used))
}

#personalizar grafico acf
ggplot_acf_pacf <-  function(res, lag, label, name, alpha= 0.05){
  df= with(res, data.frame(lag, acf))
  
  # IC alpha
  lim1= ic_alpha(alpha, res)
  lim0= -lim1
  
  ggplot(data = df, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y= label) +
    geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
    geom_hline(aes(yintercept = lim0), linetype = 2, color = 'blue') +
    theme_bw() + labs(title = paste0("Correlograma\n",name),x="Lag") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(text = element_text(size=12),
          title = element_text(face="bold",size=rel(1)),
          axis.title.x = element_text(face="bold",size=rel(1)),
          axis.title.y = element_text(face="bold",size=rel(1)),
          strip.text.x = element_text(size=rel(2.5)),
          strip.text.y = element_text(size=rel(2.5)))
  
}
plot_variogram <- function(v, m,vname="") {
  preds = variogramLine(m, maxdist = max(v$dist))
  ggplot() + 
    geom_point(data = v, aes(x = dist, y = gamma), size=3) +
    geom_line(data = preds, aes(x = dist, y = gamma))+
    labs(title = paste0("Variograma\n",vname),x="Distancia",y="Semi-Varianza")+
    theme_bw()+
    theme(text = element_text(size=16),
                       axis.text.x = element_text(size=rel(1.3)),
                       axis.text.y = element_text(size=rel(1.3)),
                       strip.text.x = element_text(size=rel(2)),
                       strip.text.y = element_text(size=rel(2)))
}

#|||||||||||||||||||||||||||||   LECTURA DE BASES DE DATOS |||||||||||||||||||||||||||||||||
var_names <- c("Temperatura Ambiental",
               "Humedad Relativa",
               "Radiacion Solar Difusa",
               "Radiacion Solar Global",
               "Temperatura de Suelo a nivel 1",
               "Direccion del Viento",
               "Velocidad del viento")
st_names <- c("Alao", "Atillo",
              "Cumanda", "Espoch",
              "Matus", "Multitud",
              "Quimiag","San Juan",
              "Tixan","Tunshi",
              "Urbina")
dat1 <- read.xlsx(xlsxFile = "data/X1_imputada.xlsx",
                  sheet = 1, detectDates = TRUE)
fecha1 <- as.POSIXct(paste(dat1[,2], dat1[,3]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
hora1 <- as.POSIXlt(fecha1)$hour
sdia1 <- as.POSIXlt(fecha1)$wday
mdia1 <- as.POSIXlt(fecha1)$mday
mes1 <- as.POSIXlt(fecha1)$mon + 1
ydia1 <- as.POSIXlt(fecha1)$yday + 1
year1 <- as.POSIXlt(fecha1)$year+1900
dat1 <- data.frame(fecha1,hora1,sdia1,mdia1,mes1,ydia1,year1,sapply(dat1[,4:length(dat1)], as.numeric))
dataX1 <- as.tibble(dat1)
colnames(dataX1) <- c("Fecha","Hora","Dia.Semanal","Dia.Mensual","Mes","Dia.Anual","Year",st_names)

dat2 <- read.xlsx(xlsxFile = "data/X2_imputada.xlsx",
                  sheet = 1, detectDates = TRUE)
fecha2 <- as.POSIXct(paste(dat2[,2], dat2[,3]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
hora2 <- as.POSIXlt(fecha2)$hour
sdia2 <- as.POSIXlt(fecha2)$wday
mdia2 <- as.POSIXlt(fecha2)$mday
mes2 <- as.POSIXlt(fecha2)$mon + 1
ydia2 <- as.POSIXlt(fecha2)$yday + 1
year2 <- as.POSIXlt(fecha2)$year+1900
dat2 <- data.frame(fecha2,hora2,sdia2,mdia2,mes2,ydia2,year2,sapply(dat2[,4:length(dat2)], as.numeric))
dataX2 <- as.tibble(dat2)
colnames(dataX2) <- c("Fecha","Hora","Dia.Semanal","Dia.Mensual","Mes","Dia.Anual","Year",st_names)

dat3 <- read.xlsx(xlsxFile = "data/X4_imputada.xlsx",
                  sheet = 1, detectDates = TRUE)
fecha3 <- as.POSIXct(paste(dat3[,2], dat3[,3]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
hora3 <- as.POSIXlt(fecha3)$hour
sdia3 <- as.POSIXlt(fecha3)$wday
mdia3 <- as.POSIXlt(fecha3)$mday
mes3 <- as.POSIXlt(fecha3)$mon + 1
ydia3 <- as.POSIXlt(fecha3)$yday + 1
year3 <- as.POSIXlt(fecha3)$year+1900
dat3 <- data.frame(fecha3,hora3,sdia3,mdia3,mes3,ydia3,year3,sapply(dat3[,4:length(dat3)], as.numeric))
dataX3 <- as.tibble(dat3)
colnames(dataX3) <- c("Fecha","Hora","Dia.Semanal","Dia.Mensual","Mes","Dia.Anual","Year",st_names)


dat4 <- read.xlsx(xlsxFile = "data/X5_imputada.xlsx",
                  sheet = 1, detectDates = TRUE)
fecha4 <- as.POSIXct(paste(dat4[,2], dat4[,3]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
hora4 <- as.POSIXlt(fecha4)$hour
sdia4 <- as.POSIXlt(fecha4)$wday
mdia4 <- as.POSIXlt(fecha4)$mday
mes4 <- as.POSIXlt(fecha4)$mon + 1
ydia4 <- as.POSIXlt(fecha4)$yday + 1
year4 <- as.POSIXlt(fecha4)$year+1900
dat4 <- data.frame(fecha4,hora4,sdia4,mdia4,mes4,ydia4,year4,sapply(dat4[,4:length(dat4)], as.numeric))
dataX4 <- as.tibble(dat4)
colnames(dataX4) <- c("Fecha","Hora","Dia.Semanal","Dia.Mensual","Mes","Dia.Anual","Year",st_names)


dat5 <- read.xlsx(xlsxFile = "data/X6_imputada.xlsx",
                  sheet = 1, detectDates = TRUE)
fecha5 <- as.POSIXct(paste(dat5[,2], dat5[,3]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
hora5 <- as.POSIXlt(fecha5)$hour
sdia5 <- as.POSIXlt(fecha5)$wday
mdia5 <- as.POSIXlt(fecha5)$mday
mes5 <- as.POSIXlt(fecha5)$mon + 1
ydia5 <- as.POSIXlt(fecha5)$yday + 1
year5 <- as.POSIXlt(fecha5)$year+1900
dat5 <- data.frame(fecha5,hora5,sdia5,mdia5,mes5,ydia5,year5,sapply(dat5[,4:length(dat5)], as.numeric))
dataX5 <- as.tibble(dat5)
colnames(dataX5) <- c("Fecha","Hora","Dia.Semanal","Dia.Mensual","Mes","Dia.Anual","Year",st_names)


dat6 <- read.xlsx(xlsxFile = "data/X15_imputada.xlsx",
                  sheet = 1, detectDates = TRUE)
fecha6 <- as.POSIXct(paste(dat6[,2], dat6[,3]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
hora6 <- as.POSIXlt(fecha6)$hour
sdia6 <- as.POSIXlt(fecha6)$wday
mdia6 <- as.POSIXlt(fecha6)$mday
mes6 <- as.POSIXlt(fecha6)$mon + 1
ydia6 <- as.POSIXlt(fecha6)$yday + 1
year6 <- as.POSIXlt(fecha6)$year+1900
dat6 <- data.frame(fecha6,hora6,sdia6,mdia6,mes6,ydia6,year6,sapply(dat6[,4:length(dat6)], as.numeric))
dataX6 <- as.tibble(dat6)
colnames(dataX6) <- c("Fecha","Hora","Dia.Semanal","Dia.Mensual","Mes","Dia.Anual","Year",st_names)



dat7 <- read.xlsx(xlsxFile = "data/X19_imputada.xlsx",
                  sheet = 1, detectDates = TRUE)
fecha7 <- as.POSIXct(paste(dat7[,2], dat7[,3]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
hora7 <- as.POSIXlt(fecha7)$hour
sdia7 <- as.POSIXlt(fecha7)$wday
mdia7 <- as.POSIXlt(fecha7)$mday
mes7 <- as.POSIXlt(fecha7)$mon + 1
ydia7 <- as.POSIXlt(fecha7)$yday + 1
year7 <- as.POSIXlt(fecha7)$year+1900
dat7 <- data.frame(fecha7,hora7,sdia7,mdia7,mes7,ydia7,year7,sapply(dat7[,4:length(dat7)], as.numeric))
dataX7 <- as.tibble(dat7)
colnames(dataX7) <- c("Fecha","Hora","Dia.Semanal","Dia.Mensual","Mes","Dia.Anual","Year",st_names)


#|||||||||||||||||||||||||||||| LECTURA DE COORDENADAS DE LAS ESTACIONES|||||||||||||||||||||||
coord <- read.xlsx(xlsxFile = "data/Coordenadas_Estaciones.xlsx",
                   sheet = 1, detectDates = TRUE)
#se ordenan la filas con arrage de menor a mayor por col 1
coord <- arrange(coord,coord[,1])

#sptdf <- coord
coordnew <- data.frame(lapply(coord,      
                              function(variables) {
                                if (is.character(variables)) {
                                  #toupper tranforma a mayusculas stri_tras remueve acentos y str_trim remueve espacios
                                  return(toupper(stri_trans_general(str_trim(variables, side = c("both")),"Latin-ASCII")))
                                } else {
                                  return(variables)
                                }
                              }),
                       stringsAsFactors = FALSE)
colnames(coordnew) <- c("ESTACION","PARROQUIA","CANTON","X","Y","LON","LAT")
pcd <- coordnew
pcd[pcd[,"PARROQUIA"]=="LIZARZABURO","PARROQUIA"] <-"RIOBAMBA" 
pcd[pcd[,"PARROQUIA"]=="MALDONADO","PARROQUIA"] <-"RIOBAMBA" 
pcd[pcd[,"PARROQUIA"]=="vELASCO","PARROQUIA"] <-"RIOBAMBA" 
pcd[pcd[,"PARROQUIA"]=="VELOZ","PARROQUIA"] <-"RIOBAMBA" 
pcd[pcd[,"PARROQUIA"]=="YARUQUI","PARROQUIA"] <-"RIOBAMBA"
sptdf <- pcd


#||||||Referenciar bases de datos y covertir a spatialPolygonDatframe||||||||||||||||||||
coordinates(sptdf) <- ~X+Y#asignar coordenadas espaciales
utm <- "+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
#Asignar sistema de referencia
proj4string(sptdf) <- utm
#sptdf$X <- pcd[,"X"]
#sptdf$Y <- pcd[,"Y"]

#||||||||||||||||||||||||||cargar mapas base de las provincia Ecuador|||||||||||||||||||||
#provincias<-readOGR("data/Mapas Provincias Ecuador SHP","nxprovincias")
#cantones<-readOGR("data/Mapas Provincias Ecuador SHP","nxcantones")
parroquias<-readOGR("data/Mapas Provincias Ecuador SHP","nxparroquias") 

# Filtrar dtos de los mapas para la provincia Chimborazo
#prov_chimb <- subset(provincias, DPA_DESPRO=='CHIMBORAZO')
#cant_chimb <- subset(cantones, DPA_DESPRO=='CHIMBORAZO')
parro_chimb <- subset(parroquias, DPA_DESPRO=='CHIMBORAZO')

#complemeto adicional para graficar en ggplot debido a que en el server shinyappsio
#no funciona las funciones tidy ni fortify por lo que es necesario tener el
#mapa de Chimborazo filtrado como dataframe en un documento aparte
#spdf_fortified <- read.xlsx(xlsxFile = "data/polygon_ch_fil.xlsx",
#                   sheet = 1, detectDates = TRUE)
#Actualizado para shiny server de Centos 7 no restringe la capacidad de memoria para calcular
#dataframes extensos ni almacenamiento limitado por lo que este archivo ya se calcula usando
#la funcion reactivo map_base en la seccion server.R