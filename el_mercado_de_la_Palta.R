##  el_mercado_de_la_Palta.R
##  
##  Héctor Garrido Henríquez
##  Analista Cuantitativo. Observatorio Laboral Ñuble
##  Docente. Facultad de Ciencias Empresariales
##  Universidad del Bío-Bío
##  Avenida Andrés Bello 720, Casilla 447, Chillán
##  Teléfono: +56-942353973
##  http://www.observatoriolaboralnuble.cl
##
##  Este programa se utiliza para el capítulo referente a las características del mercado
## de la palta en Chile


if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(readxl)) install.packages("readxl"); require(readxl)
if (!require(readr)) install.packages("readr"); require(readr)
if (!require(RColorBrewer)) install.packages("RColorBrewer"); require(RColorBrewer)
if (!require(stargazer)) install.packages("stargazer"); require(stargazer)
if (!require(haven)) install.packages("haven"); require(haven)
if (!require(survey)) install.packages("survey"); require(survey)
if (!require(stringi)) install.packages("stringi"); require(stringi)
if (!require(readxl)) install.packages("readxl"); require(readxl)
if (!require(lubridate)) install.packages("lubridate"); require(lubridate)


####
#### Precio de la palta en Chile

setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
path = "/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master"
source("weekly_palta.R")
#setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
#source("weekly_tomate.R")

par(family="serif", bty="l", bg="white", cex.lab=1) # opciones gráficas
ts.plot(exp(precio_mayorista), exp(precio_supermercado), lty=1:2,
        lwd=1, ylab="$/kilo", xlab="Tiempo")
legend("topleft", legend = c("Mayorista", "Supermercado"),lty=1:2, lwd=2, cex=0.8)

datos = cbind(exp(precio_mayorista), exp(precio_supermercado))

summary(datos)

stargazer(datos, summary = TRUE)


par(family="serif", bty="l", bg="white", cex.lab=1) # opciones gráficas

precios = left_join(price_may_palta, price_sup_palta, by = "start") %>%
  select(year.x,month.x,day.x,price.x, price.y) %>% 
  melt(id = c("year.x","month.x","day.x")) %>% mutate(precio = exp(value)) %>% 
  mutate(fecha = as.Date(paste0(year.x,"/",month.x,"/",day.x)))

ggplot(precios,aes(x=fecha,y=precio,colour=variable,group=variable)) + geom_line()+
  scale_colour_discrete(name  ="Precio",
                        breaks=c("price.x", "price.y"),
                        labels=c("Mayorista", "Supermercado"))+ylab("Precio ($/kilo)")



## ----include=FALSE-------------------------------------------------------
arribo_mayoristas <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/Volumen_Paltas.xls", range = "B6:O50") 

arribo_mayoristas = arribo_mayoristas %>% select(-Total) %>% 
  gather(mes, volumen,-Años) %>% arrange(Años) %>% filter(Años!=2018) %>% 
  mutate(fecha = seq(as.Date("1975/01/01"),as.Date("2017/12/01"),by="month")) %>% 
  select(-c(Años,mes)) 

summary(arribo_mayoristas$volumen/1000)

arribo_mayoristas[which.min(arribo_mayoristas$volumen),]
arribo_mayoristas[which.max(arribo_mayoristas$volumen),]

## ----fig.cap="Volúmenes de palta arribados en mercados mayoristas 1975-2017\\label{volumen}",out.width='4.5in', out.height='3.5in', fig.align='center', message=FALSE----
options(scipen = 10)
ggplot(arribo_mayoristas[1:516,], aes(fecha, volumen/1000000)) + geom_line() +
  xlab("") + ylab("Miles de toneladas")


## Gasto 
gasto <- read_dta("~/GoogleDrivePersonal/Master/Tesis/datos/BASE_GASTOS_VIIEPF.dta")
gasto = gasto  %>% mutate(PERSONA = 1) %>% arrange(FOLIO, PERSONA)


glosario_epf <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/glosario_epf.xlsx", 
                           range = "A2:D87")

path_clas = file.path("~/GoogleDrivePersonal/",
"Master/Tesis/datos/",
"clasificacion_de_consumo_individual_por_finalidades.xls")
clasificacion <- read_excel(path_clas, range = "A3:H1573")

legumbres_hortalizas = clasificacion %>% filter(D==0,X__1==1,G==1, C==7)

gasto = gasto %>% mutate(legumbres = grepl("^01.1.7",CCIF)) %>% 
  filter(legumbres == 1,P!=0) 

design = svydesign(~1, weights = ~FE, data = gasto)

consumo_promedio = svyby(~GASTO, by = ~GLOSA, design = design, svymean) %>%
  arrange(desc(GASTO))

rownames(consumo_promedio) = rownames(consumo_promedio) %>% as.numeric()

otras = consumo_promedio$GLOSA[12:50]

design$variables = design$variables %>% mutate(glosa = ifelse(GLOSA %in% otras,"Otras",GLOSA))

consumo_promedio = svyby(~GASTO, by = ~glosa, design = design, svymean) %>% arrange(desc(GASTO)) %>% 
  mutate(glosa = tolower(glosa))

consumo_latex = paste0(consumo_promedio[,1]," & ", 
                       round(consumo_promedio[,2],1), " \\")

consumo_quantiles = svyby(~GASTO, by = ~glosa, design = design,
                          svyquantile, c(.25,0.5,0.75), keep.var = FALSE) %>% 
  mutate(glosa = tolower(glosa))

tabla_resumen = cbind(consumo_promedio[,c(1,2)], consumo_quantiles[,c(2,3,4)])

colnames(tabla_resumen) = c("Producto", "Gasto promedio", "percentil 25", "percentil 50", "percentil 75")

xtable::xtable(tabla_resumen, type = "latex", caption = "Gasto promedio en legumbres y hortalizas en Chile",
               digits = 0)



#### Producción 

## ----echo=FALSE,fig.cap="Superficie plantada de Paltos\\label{Paltos}",out.width='4.5in', out.height='3.5in', fig.align='center', message=FALSE----
catastro <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/Datos-Catastro_may.xlsx")

catastro = catastro %>% filter(Especie=="Palto") %>% select(-Especie) %>% 
  rename(superficie = `Superficie comercial (hectáreas)`)

ggplot(catastro,aes(x=Año,y=superficie,colour=Región,group=Región)) + geom_line()+
  scale_colour_manual(values = brewer.pal(10,"Set3"))

## ------------------------------------------------------------------------
world_prod <- read_csv("~/GoogleDrivePersonal/Master/Tesis/datos/Producción mundial de palta.csv")

produccion = world_prod %>% filter(Element == "Production")

produccion_chile = world_prod %>% filter(Element == "Production", Area =="Chile")


### Comercio internacional 

chilean_exports <- read_csv("~/GoogleDrivePersonal/Master/Tesis/datos/chilean_exports.csv")

principal_destin = chilean_exports %>% filter(Year ==2017, Partner != "World") %>%
  arrange(desc(`Netweight (kg)`)) %>% select(Partner, `Netweight (kg)`, 
                                             `Trade Value (US$)`) %>% 
  mutate(peso = `Netweight (kg)`/sum(`Netweight (kg)`), 
         peso_acum = cumsum(peso))

chilean_exports <- read_csv("~/GoogleDrivePersonal/Master/Tesis/datos/chilean_exports.csv")

exportaciones_palta = read.csv("~/GoogleDrivePersonal/Master/Tesis/datos/exportaciones_palta.csv")

exportaciones_palta = exportaciones_palta %>%
  mutate(precio = Trade.Value..US../Netweight..kg., 
         Year = stri_sub(Period, 1,4), 
         Month = stri_sub(Period,5,6))

precio_ponderado = exportaciones_palta %>% filter(Year>=2012) %>% 
  group_by(Year, Month) %>% 
  summarise(promedio_ponderado = weighted.mean(precio, Netweight..kg.)) %>% 
  select(Year, Month, everything())

tipo_de_cambio <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/tipo_de_cambio.xls")

tipo_de_cambio = tipo_de_cambio %>% mutate(Year = year(Periodo), 
                                           Month = month(Periodo), 
                                           Year = as.character(Year), 
                                           Month = as.character(Month)) %>% 
  select(-Periodo)

precio_ponderado = inner_join(precio_ponderado,
                              tipo_de_cambio, by = c("Year", "Month"))

