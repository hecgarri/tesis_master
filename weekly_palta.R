rm(list=ls())
setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/datos/semanales/palta")
getwd()

palta_2008 = read.csv("2008_consumidor.csv", sep=",")
palta_2009 = read.csv("2009_consumidor.csv", sep=",")
palta_2010 = read.csv("2010_consumidor.csv", sep=",")
palta_2011 = read.csv("2011_consumidor.csv", sep=",")
palta_2012 = read.csv("2012_consumidor.csv", sep=",")
palta_2013 = read.csv("2013_consumidor.csv", sep=",")
palta_2014 = read.csv("2014_consumidor.csv", sep=",")
palta_2015 = read.csv("2015_consumidor.csv", sep=",")
palta_2016 = read.csv("2016_consumidor.csv", sep=",")


palta_cons = rbind(palta_2008, palta_2009, palta_2010, 
                    palta_2011, palta_2012, palta_2013, 
                    palta_2014, palta_2015, palta_2016)

#' Por el momento, utilizaremos solamente el tomate larga vida y la lechuga escarola para el análisis 


## ----Creación de las serie de precios al consumidor
library(magrittr)
library(dplyr)
library(tidyr)

p_super_palta = filter(palta_cons, Variedad=="Hass " & Calidad=="Primera " & 
                       Tipo.punto.monitoreo=="Supermercado ") %>% 
  `colnames<-` (c("week", "start", "end", "place","product",
                  "variety","quality","unity","min","max","price")) %>% 
  mutate(price=price/1.19,start =gsub("/","-",start))


#generaré una secuencia que tiene todas las semanas para luego comparar con mi secuencia incompleta

ts = seq.POSIXt(as.POSIXct("2008-03-17", '%y%m%d'), as.POSIXct("2016-12-26", '%y%m%d'), by="week")

df <- data.frame(start=ts)




p_super_palta = separate(data=p_super_palta,col = "start",
                       into = c("day", "month", "year"),sep = "\\-",extra="drop")
price_sup_palta = mutate(p_super_palta, day = as.numeric(day), 
                       month=as.numeric(month), year=as.numeric(year), 
                       start = paste(year, month, day, sep="-"), 
                       start = as.POSIXct(start, '%y%m%d'), 
                       price = log(price))

price_sup_palta <- full_join(df,price_sup_palta)

##########################################################
# Precios Mayoristas
###########################################################

lo_valledor = read.csv("LoValledor.csv")
vega_central = read.csv("VegaCentral.csv")

mayor = rbind(lo_valledor)

mayor = filter(mayor, Variedad=="Hass " & Calidad=="Primera ") %>% 
  mutate(Desde = gsub("/","-",Desde)) %>% 
  separate(col="Desde", into=c("day", "month", "year"), sep="\\-", extra="drop") %>% 
  mutate(day = as.numeric(day), mont = as.numeric(month), year = as.numeric(year), 
         start = paste(year, month, day, sep="-"), 
         start = as.POSIXct(start, '%y%m%d'))

mayor$Unidad.de.comercialización. = recode(as.character(mayor$Unidad.de.comercialización.),
        "$/bandeja 10 kilos "=10, 
       "$/bandeja 4 kilos embalada " = 4, "$/caja 12 kilos " = 12, 
       "$/kilo (en bins de 400 kilos) " = 400, "$/kilo (en bins de 450 kilos) " = 450,
       "$/kilo (en caja de 15 kilos) " = 15, "$/kilo (en caja de 17 kilos) " = 17)

mayor$precio_definitivo = ifelse(mayor$Unidad.de.comercialización.==10 | 
                                   mayor$Unidad.de.comercialización.==4 |
                                   mayor$Unidad.de.comercialización.== 12, 
                                 mayor$Precio.promedio./mayor$Unidad.de.comercialización., 
                                 mayor$Precio.promedio.)

mayor = mutate(mayor, Volumen = as.numeric(Volumen), Precio.promedio. = as.numeric(Precio.promedio.),
               volumen_definitivo = Volumen*Unidad.de.comercialización.) %>%   
  group_by(start) %>% summarise(price = weighted.mean(precio_definitivo, volumen_definitivo)) %>% 
  mutate(price = log(price))

price_may_palta = full_join(df, mayor)

price_may_palta = separate(price_may_palta, col="start", into=c("year", "month", "day"), sep="\\-", extra="drop") %>% 
  mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year), 
         start = paste(year, month, day, sep="-"), 
         start = as.POSIXct(start, '%y%m%d')) %>% 
  mutate(mes = as.factor(month))

#dummies = model.matrix(~price_may_palta$mes)[,-1]


#colnames(dummies) = c("feb", "mar", "abr", "may", "jun", "jul", 
#                      "aug", "sep", "oct", "nov", "dec")

precio_mayorista = ts(price_may_palta$price, start=c(2008,3), frequency=365/7)

precio_supermercado = ts(price_sup_palta$price, start=c(2008,3), frequency=365/7)
