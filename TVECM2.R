##  TVECM2.R
##  
##  Héctor Garrido Henríquez
##  Analista Cuantitativo. Observatorio Laboral Ñuble
##  Docente. Facultad de Ciencias Empresariales
##  Universidad del Bío-Bío
##  Avenida Andrés Bello 720, Casilla 447, Chillán
##  Teléfono: +56-942353973
##  http://www.observatoriolaboralnuble.cl
##
##  Este programa los utilizo para estimar el umbral del modelo
##  sujeto a la estimación del modelo anterior. 
##  Es un programa para hacer la búsqueda a través de minimos cuadrados condicionales

  p = 7
  T = nrow(datos)
  r = 1 # Número de relaciones de cointegración 
  beta = matrix(homogeneity@V[,1:r],ncol = r) # Vector cointegrante restringido
  DeltaY<-diff(y)[(p+1):(T-1),]
  DeltaX = homogeneity@Z1 # Matriz de variables independientes 
  ECTminus1 = homogeneity@ZK %*%beta # Relación cointegrante rezagada
  allgammas <- sort(unique(ECTminus1)) # Relación cointegrante ordenada 
  ng <- length(allgammas) # Número de elementos en el vector anterior 
  trim = 0.05 # porcentaje de observaciones a dejar fuera. 
  ngridG = 492 # Número de valores para realizar la búsqueda, en este caso el total
  gammas <- allgammas[round(seq(from = trim, to = 1 - trim, 
                                length.out = ngridG) * ng)] # Número de gammas para buscar

  store = matrix(NA,nrow = length(gammas),ncol=3) # Matriz de resultados 
  
  "%a%"<-function(matrix,dummy) matrix*dummy # Operador de multiplicación 
  for (i in 1:length(gammas)){
    ECT = ECTminus1
    zi<-cbind(ECT,DeltaX) #All variables: ECT and lag, of dim t x kp+1+1
    d1 = ifelse(ECT<gammas[i],1,0)
    n1 = mean(d1)
    props[i] = n1
    if(is.na(n1)==TRUE) n1<-0
    if (min(n1,1-n1)>trim) {
      zigamma<-c(d1)*zi
      zi<-zi%a%c(1-d1) #new operator for choice between set up of first matrix
      Z<-cbind(zigamma,zi)
      LS<-try(crossprod(c(Y-tcrossprod(Z,crossprod(Y,Z)%*%solve(crossprod(Z))))), silent=TRUE)
    }
    else LS = NA
    store[i,1] = LS
    store[i,2] = n1
    store[i,3] = gammas[i]
  }

set = store %>% data.frame() %>% rename(SSR = X1, prop = X2, gamma = X3) %>% arrange(gammas)

bestGamma = set$gamma[which.min(set$SSR)]
bestSSR = set$SSR[which.min(set$SSR)]
plot(set$gamma, set$SSR, type = "l", xlab = "Threshold parameter gamma", 
     ylab = "Residual Sum of Squares", main = "Grid Search")
points(x = bestGamma, y = bestSSR, 
       col = 2, cex = 2)

#dev.print(pdf,"fig_results/fig8.pdf")

set[which.min(set$SSR),]

# Ahora procederé a estimar los regímenes resultantes

dummy = ifelse(ECTminus1<set$gamma[which.min(set$SSR)],1,0) # Dummy que escoge el regimen
Z =   cbind(ECTminus1,DeltaX)
colnames(Z) = c("ect", colnames(DeltaX))
Zgamma = c(dummy)*Z
Zd = Z%a%c(1-dummy)
ZZ = cbind(Zgamma,Zd)
Y = DeltaY
modelo = lm(Y~ZZ-1)

sum_modelo = summary(modelo)

coef_modelo = round(rbind(cbind(sum_modelo$`Response mayorista`$coefficients[1:15,1],
                        sum_modelo$`Response mayorista`$coefficients[1:15,2],
                        sum_modelo$`Response supermercado`$coefficients[1:15,1],
                        sum_modelo$`Response supermercado`$coefficients[1:15,2]),
                        cbind(sum_modelo$`Response mayorista`$coefficients[16:30,1],
                              sum_modelo$`Response mayorista`$coefficients[16:30,2],
                              sum_modelo$`Response supermercado`$coefficients[16:30,1],
                              sum_modelo$`Response supermercado`$coefficients[16:30,2])),3)
rownames(coef_modelo) = gsub("^ZZ","",rownames(coef_modelo))
rownames(coef_modelo) = gsub(".dl","$_{t-",rownames(coef_modelo))
colnames(coef_modelo) = c("Mayoristas","","Supermercados","")

modelo_tex = paste0("\\Delta$",rownames(coef_modelo),"} & ",coef_modelo[,1]," (",coef_modelo[,2],") & ",
                    coef_modelo[,3]," (", coef_modelo[,4],") \\") %>% data.frame()


ts.plot(cbind(cumsum(fitted(modelo)[,1]),cbind(cumsum(fitted(modelo)[,2]))))


residuos = residuals(modelo)

boot_res = residuos[sample(seq_len(t), replace = TRUE),]