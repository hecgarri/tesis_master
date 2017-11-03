
setwd("~/GoogleDrivePersonal/Master/Tesis/datos/semanales/tomate")
getwd()

if (!require(data.table)) install.packages("data.table")
if (!require(tidyverse)) install.packages("tidyverse")

tomates = list.files(pattern="consumidor.csv$")

tomate_cons = lapply(tomates, function(x) fread(x)) %>% rbindlist(fill = TRUE)


p_super_tomate = filter(tomate_cons, Variedad=="Larga vida" & Calidad=="Primera") %>% 
  `colnames<-` (c("week", "start", "end", 
                  "variety","quality","unity","min","max","price")) %>% 
  mutate(price=price/1.19,start =gsub("/","-",start))


#generaré una secuencia que tiene todas las semanas para luego comparar con mi secuencia incompleta

ts = seq.POSIXt(as.POSIXct("2008-03-17", '%y%m%d'), as.POSIXct("2017-10-09", '%y%m%d'), by="week")

df <- data.frame(start=ts)

p_super_tomate = separate(data=p_super_tomate,col = "start",
                         into = c("day", "month", "year"),sep = "\\-",extra="drop")
price_sup_tomate = mutate(p_super_tomate, day = as.numeric(day), 
                         month=as.numeric(month), year=as.numeric(year), 
                         start = paste(year, month, day, sep="-"), 
                         start = as.POSIXct(start, '%y%m%d'), 
                         price = log(price))

price_sup_tomate <- full_join(df,price_sup_tomate)

##########################################################
# Precios Mayoristas
###########################################################

lo_valledor = read.csv("LoValledor.csv")

mayor = rbind(lo_valledor)

mayor = filter(mayor, Calidad=="Primera ") %>% 
  mutate(Desde = gsub("/","-",Desde)) %>% 
  separate(col="Desde", into=c("day", "month", "year"), sep="\\-", extra="drop") %>% 
  mutate(day = as.numeric(day), mont = as.numeric(month), year = as.numeric(year), 
         start = paste(year, month, day, sep="-"), 
         start = as.POSIXct(start, '%y%m%d'))

mayor$Unidad.de.comercialización. = recode(as.character(mayor$Unidad.de.comercialización.),
                                           "$/bandeja 15 kilos "=15, 
                                           "$/bandeja 18 kilos " = 18,
                                           "$/bandeja 20 kilos "=20,
                                           "$/caja 10 kilos " = 10, 
                                           "$/caja 12 kilos " = 12,
                                           "$/caja 15 kilos " = 15,
                                           "$/caja 16 kilos " = 16,
                                           "$/caja 18 kilos " = 18, 
                                           "$/caja 20 kilos " = 20)

mayor = mutate(mayor, precio_definitivo = Precio.promedio./Unidad.de.comercialización.)

mayor = mutate(mayor, Volumen = as.numeric(Volumen), Precio.promedio. = as.numeric(Precio.promedio.),
               volumen_definitivo = Volumen*Unidad.de.comercialización.) %>%   
  group_by(start) %>% summarise(price = weighted.mean(precio_definitivo, volumen_definitivo)) %>% 
  mutate(price = log(price))

price_may_tomate = full_join(df, mayor)

price_may_tomate = separate(price_may_tomate, col="start", into=c("year", "month", "day"), sep="\\-", extra="drop") %>% 
  mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year), 
         start = paste(year, month, day, sep="-"), 
         start = as.POSIXct(start, '%y%m%d')) %>% 
  mutate(mes = as.factor(month))

#dummies = model.matrix(~price_may_tomate$mes)[,-1]


#colnames(dummies) = c("feb", "mar", "abr", "may", "jun", "jul", 
#                      "aug", "sep", "oct", "nov", "dec")

precio_mayorista_tomate = ts(price_may_tomate$price, start=c(2008,3), frequency=365.25/7)

precio_supermercado_tomate = ts(price_sup_tomate$price, start=c(2008,3), frequency=365.25/7)

