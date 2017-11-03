rm(list=ls())
setwd("~/GoogleDrivePersonal/Master/Tesis/datos/semanales/palta")
getwd()

if (!require(data.table)) install.packages("data.table")
if (!require(tidyverse)) install.packages("tidyverse")

paltas = list.files(pattern="consumidor.csv$")

palta_cons = lapply(paltas, function(x) fread(x)) %>% rbindlist()


p_super_palta = filter(palta_cons, Variedad=="Hass" & Calidad=="Primera" & 
                       `Tipo punto monitoreo`=="Supermercado") %>% 
  `colnames<-` (c("week", "start", "end", "place","product",
                  "variety","quality","unity","min","max","price")) %>% 
  mutate(price=price/1.19,start =gsub("/","-",start))


#generaré una secuencia que tiene todas las semanas para luego comparar con mi secuencia incompleta

ts = seq.POSIXt(as.POSIXct("2008-03-17", '%y%m%d'), as.POSIXct("2017-10-09", '%y%m%d'), by="week")

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

mayor = rbind(lo_valledor)

mayor = mayor %>% 
  mutate(Desde = gsub("/","-",Desde)) %>% 
  separate(col="Desde", into=c("day", "month", "year"), sep="\\-", extra="drop") %>% 
  mutate(day = as.numeric(day), mont = as.numeric(month), year = as.numeric(year), 
         start = paste(year, month, day, sep="-"), 
         start = as.POSIXct(start, '%y%m%d'))

mayor$Unidad.de.comercialización. = recode(as.character(mayor$Unidad.de.comercialización.),
        "$/bandeja 10 kilos "=10, 
       "$/bandeja 4 kilos embalada " = 4,"$/bandeja 8 kilos "=8,
       "$/caja 12 kilos " = 12, 
       "$/kilo (en bins de 400 kilos) " = 400, "$/kilo (en bins de 450 kilos) " = 450,
       "$/kilo (en caja de 15 kilos) " = 15, "$/kilo (en caja de 17 kilos) " = 17, 
       "$/kilo (en caja de 20 kilos) " = 20, "$/kilo (en caja de 8 kilos ) " = 8)

mayor$precio_definitivo = ifelse(mayor$Unidad.de.comercialización.==10 | 
                                   mayor$Unidad.de.comercialización.==4 |
                                   mayor$Unidad.de.comercialización.==8 |
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

precio_mayorista = ts(price_may_palta$price, start=c(2008,3), frequency=365.25/7)

precio_supermercado = ts(price_sup_palta$price, start=c(2008,3), frequency=365.25/7)
