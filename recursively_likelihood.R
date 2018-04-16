#The recursively calculated log likelihood


m = modelo_alternativo
p = m$lag
x = datos 
wind = 208
r = 1 # número de relaciones de cointegración 


fun = function(x)  ca.jo(x, type = "trace", ecdet = "const", K=9)@teststat
# Aplico la definición del modelo 
rolling = rollapplyr(datos, wind, fun, by.column = FALSE)

ts.plot(rolling)