##  Calculos_EPF.R
##  
##  Héctor Garrido Henríquez
##  Analista Cuantitativo. Observatorio Laboral Ñuble
##  Docente. Facultad de Ciencias Empresariales
##  Universidad del Bío-Bío
##  Avenida Andrés Bello 720, Casilla 447, Chillán
##  Teléfono: +56-942353973
##  http://www.observatoriolaboralnuble.cl
##
##  Esta sección se utiliza en el capítulo "El mercado de la palta en Chile"
##  Utiliza la encuesta de presupuestos del INE para analizar el gasto en 
##  legumbres en Chile


if (!require(survey)) install.packages("survey"); require(survey)
if (!require(haven)) install.packages("haven"); require(haven)
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(readxl)) install.packages("readxl"); require(readxl)

