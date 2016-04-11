# Tarea3 Aprendizaje No Supervisado
library("scatterplot3d")
# Inicio a.csv

# Leer los csv
datos_a = read.csv(file = "a.csv",header = F)

#----- KMEDIAS ---#

table(datos_a$V3)

plot(datos_a$V1,datos_a$V2,xlab = "V1",ylab = "V2")
# Podemos observar que hay 3 conglomerados 
# por lo tanto vamos a utilizar 3 centers

kmedias_a = kmeans(x = datos_a[, c("V1", "V2")],
                   centers = 3)

# Datos originales con colores de cluster
plot(x = datos_a$V1,
     y = datos_a$V2,
     col = kmedias_a$cluster)

# Ahora graficamos los puntos
points(x = kmedias_a$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)
#Matriz de Confusion
matrizk=table(kmedias_a$cluster, datos_a$V3)

#Presicion kmedias
preskmedias = sum(diag(matrizk))/sum(matrizk)

#----- FKMEDIAS ---#

#----- CLUSTER JERÁRQUICOS-----#

# Primero buscamos la Matriz de Distancia 

# Copiamos el dataset en una variable nueva
entrada = datos_a

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada$V3 = NULL

# De DataFrame a Matrix
entrada = as.matrix(entrada)

# Matriz de distancia
distancia = dist(entrada)

# Llamando al método Hclust

# Primero usamos en metodo complete

metodo = "complete"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte = cutree(cluster, k=3)
head(corte)

plot(x = datos_a$V1,
     y = datos_a$V2,
     col = corte)

unique(corte)

matrizcom=table(datos_a$V3,corte)

#Presicion complete
prescom = sum(diag(matrizcom))/sum(matrizcom)

# Luego usamos en metodo single

metodo = "single"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte = cutree(cluster, k=3)
head(corte)

plot(x = datos_a$V1,
     y = datos_a$V2,
     col = corte)

unique(corte)

matrizsin=table(datos_a$V3,corte)

#Presicion single
pressin = sum(diag(matrizsin))/sum(matrizsin)

# Finalmente usamos en metodo average

metodo = "average"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte = cutree(cluster, k=3)
head(corte)

plot(x = datos_a$V1,
     y = datos_a$V2,
     col = corte)

unique(corte)

matrizavg=table(datos_a$V3,corte)

#Presicion average
presavg = sum(diag(matrizavg))/sum(matrizavg)

#----- FCLUSTER JERÁRQUICOS-----#
# Fin a.csv

# Inicio moon.csv

# Leer los csv
datos_moon = read.csv(file = "moon.csv",header = F)
#----- KMEDIAS ---#

table(datos_moon$V3)

plot(datos_moon$V1,datos_moon$V2,xlab = "V1",ylab = "V2")

# Podemos observar que hay 2 conglomerados 
# por lo tanto vamos a utilizar 2 centers

kmedias_moon = kmeans(x = datos_moon[, c("V1", "V2")],
                      centers = 2)

# Datos originales con colores de cluster
plot(datos_moon[, c("V1", "V2")],
     col = kmedias_moon$cluster)

# Ahora graficamos los puntos
points(x = kmedias_moon$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)
#Matriz de Confusion
matrizk=table(kmedias_moon $cluster, datos_moon$V3)

#Presicion kmedias
preskmedias = sum(diag(matrizk))/sum(matrizk)

#----- FKMEDIAS ---#
#----- CLUSTER JERÁRQUICOS-----#
# Primero buscamos la Matriz de Distancia 

# Copiamos el dataset en una variable nueva
entrada_moon = datos_moon

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada_moon$V3 = NULL

# De DataFrame a Matrix
entrada_moon = as.matrix(entrada_moon)

# Matriz de distancia
distancia_moon = dist(entrada_moon)

# Llamando al método Hclust

# Primero usamos en metodo complete

metodo = "complete"
cluster_moon = hclust(distancia_moon, method = metodo)
plot(cluster_moon)
corte_moon = cutree(cluster_moon , k=2)
head(corte_moon)

plot(x = datos_moon$V1,
     y = datos_moon$V2,
     col = corte_moon)

unique(corte_moon)

matrizcom=table(datos_moon$V3,corte_moon)

#Presicion complete
prescom = sum(diag(matrizcom))/sum(matrizcom)

# Luego usamos en metodo single

metodo = "single"
cluster_moon = hclust(distancia_moon, method = metodo)
plot(cluster_moon)
corte_moon = cutree(cluster_moon , k=2)
head(corte_moon)

plot(x = datos_moon$V1,
     y = datos_moon$V2,
     col = corte_moon)

unique(corte_moon)

matrizsin=table(datos_moon$V3,corte_moon)

#Presicion single
pressin = sum(diag(matrizsin))/sum(matrizsin)

# Finalmente usamos en metodo average

metodo = "average"
cluster_moon = hclust(distancia_moon, method = metodo)
plot(cluster_moon)
corte_moon = cutree(cluster_moon , k=2)
head(corte_moon)

plot(x = datos_moon$V1,
     y = datos_moon$V2,
     col = corte_moon)

unique(corte_moon)

matrizavg=table(datos_moon$V3,corte_moon)

#Presicion average
presavg = sum(diag(matrizavg))/sum(matrizavg)

#----- FCLUSTER JERÁRQUICOS-----#
# Fin moon.csv

# Inicio good_luck.csv

# Leer los csv
datos_goodluck = read.csv(file = "good_luck.csv",header = F)

#----- KMEDIAS ---#

table(datos_goodluck$V11)
# Podemos observar que hay 2 clases
# por lo tanto vamos a utilizar 2 centers
plot(datos_goodluck[,1:10], col = datos_goodluck$V11)

kmedias_goodluck = kmeans(x = datos_goodluck[, 1:10],
                          centers = 2)

#Matriz de Confusion
matrizk=table(kmedias_goodluck$cluster, datos_goodluck$V11)

#Presicion kmedias
preskmedias = sum(diag(matrizk))/sum(matrizk)

#----- FKMEDIAS ---#

#----- CLUSTER JERÁRQUICOS-----#

# Primero buscamos la Matriz de Distancia 

# Copiamos el dataset en una variable nueva
entrada_goodluck = datos_goodluck

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada_goodluck$V11 = NULL

# De DataFrame a Matrix
entrada_goodluck = as.matrix(entrada_goodluck)

# Matriz de distancia
distancia_goodluck = dist(entrada_goodluck)

# Llamando al método Hclust

# Primero usamos en metodo complete

metodo = "complete"
cluster_goodluck = hclust(distancia_goodluck, method = metodo)
plot(cluster_goodluck)
corte_goodluck = cutree(cluster_goodluck, k=2)
head(corte_goodluck)

plot(datos_goodluck[,1:10],
     col = corte_goodluck)

unique(corte_goodluck)

matrizcom=table(datos_goodluck$V11,corte_goodluck)

#Presicion complete
prescom = sum(diag(matrizcom))/sum(matrizcom)

# Luego usamos en metodo single

metodo = "single"
cluster_goodluck = hclust(distancia_goodluck, method = metodo)
plot(cluster_goodluck)
corte_goodluck = cutree(cluster_goodluck, k=2)
head(corte_goodluck)

plot(datos_goodluck[,1:10],
     col = corte_goodluck)

unique(corte_goodluck)

marizsin=table(datos_goodluck$V11,corte_goodluck)

#Presicion single
pressin = sum(diag(matrizsin))/sum(matrizsin)

# Finalmente usamos en metodo average

metodo = "average"
cluster_goodluck = hclust(distancia_goodluck, method = metodo)
plot(cluster_goodluck)
corte_goodluck = cutree(cluster_goodluck, k=2)
head(corte_goodluck)

plot(datos_goodluck[,1:10],
     col = corte_goodluck)

unique(corte_goodluck)

matrizavg=table(datos_goodluck$V11,corte_goodluck)

#Presicion average
presavg = sum(diag(matrizavg))/sum(matrizavg)

#----- FCLUSTER JERÁRQUICOS-----#
# Fin good_luck.csv

# Inicio h.csv
# Leer los csv
datos_h = read.csv(file = "h.csv",header = F)
table(datos_h$V4)
plot(datos_h[,1:3], col = datos_h$V4)
# Observamos que hay 7 colores distintos
# por lo tanto suponemos que son 7 clusters

scatterplot3d(datos_h$V1, datos_h$V2, datos_h$V3,color = datos_h$V4)
min(datos_h$V4)
max(datos_h$V4)
#----- FUNCION DE ASIGNAR CLASES ---#

clase_h = function(numero){
  if(numero < 5)
    return(1)
  else if(numero < 6)
    return(2)
  else if(numero < 7)
    return(3)
  else if(numero < 8)
    return(4)
  else if(numero < 9)
    return(5)
  else if(numero < 10)
    return(6)
  else
    return(7)
}
for (x in 1:nrow(datos_h)) {
  datos_h$V4[x] <- clase_h (datos_h$V4[x])
}

table(datos_h$V4)

#----- F FUNCION DE ASIGNAR CLASES ---#

#----- KMEDIAS ---#
kmedias_h = kmeans(x = datos_h[, 1:3],
                   centers = 7)

#Matriz de Confusion
matrizk=table(kmedias_h$cluster, datos_h$V4)

#Presicion kmedias
preskmedias = sum(diag(matrizk))/sum(matrizk)

#----- FKMEDIAS ---#

#----- CLUSTER JERÁRQUICOS-----#

# Primero buscamos la Matriz de Distancia 

# Copiamos el dataset en una variable nueva
entrada_h = datos_h

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada_h$V4 = NULL

# De DataFrame a Matrix
entrada_h = as.matrix(entrada_h)

# Matriz de distancia
distancia_h = dist(entrada_h)

# Llamando al método Hclust

# Primero usamos en metodo complete

metodo = "complete"
cluster_h = hclust(distancia_h, method = metodo)
plot(cluster_h)
corte_h = cutree(cluster_h, k=7)
head(corte_h)

plot(datos_h[,1:3],
     col = corte_h)

unique(corte_h)

matrizcom=table(datos_h$V4,corte_h)

#Presicion complete
prescom = sum(diag(matrizcom))/sum(matrizcom)

# Luego usamos en metodo single

metodo = "single"
cluster_h = hclust(distancia_h, method = metodo)
plot(cluster_h)
corte_h = cutree(cluster_h, k=7)
head(corte_h)

plot(datos_h[,1:3],
     col = corte_h)

unique(corte_h)

matrizsin=table(datos_h$V4,corte_h)

#Presicion single
pressin = sum(diag(matrizsin))/sum(matrizsin)

# Finalmente usamos en metodo average

metodo = "average"
cluster_h = hclust(distancia_h, method = metodo)
plot(cluster_h)
corte_h = cutree(cluster_h, k=7)
head(corte_h)

plot(datos_h[,1:3],
     col = corte_h)

unique(corte_h)

matrizavg=table(datos_h$V4,corte_h)
#Presicion average
presavg = sum(diag(matrizavg))/sum(matrizavg)
#----- FCLUSTER JERÁRQUICOS-----#
# Fin h.csv

# Inicio s.csv
# Leer los csv
datos_s = read.csv(file = "s.csv",header = F)
table(datos_s$V4)

#----- FUNCION DE ASIGNAR CLASES ---#

clase_s <- function(numero){
  if (numero < -2)
    return(1)
  else if (numero < 0)
    return(2)
  else if (numero < 2)
    return(3)
  else
    return(4)
}
for (x in 1:nrow(datos_s)) {
  datos_s$V4[x] = clase_s (datos_s$V4[x])
}

table(datos_s$V4)
plot(datos_s, col = datos_s$V4)
scatterplot3d(datos_s$V1, datos_s$V2, datos_s$V3,color = datos_s$V4)
#----- F FUNCION DE ASIGNAR CLASES ---#

#----- KMEDIAS ---#
kmedias_s = kmeans(x = datos_s[, 1:3],
                   centers = 4)

#Matriz de Confusion
matrizk=table(kmedias_s$cluster, datos_s$V4)

#Presicion kmedias
preskmedias = sum(diag(matrizk))/sum(matrizk)

#----- FKMEDIAS ---#

#----- CLUSTER JERÁRQUICOS-----#

# Primero buscamos la Matriz de Distancia 

# Copiamos el dataset en una variable nueva
entrada_s = datos_s

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada_s$V4 = NULL

# De DataFrame a Matrix
entrada_s = as.matrix(entrada_s)

# Matriz de distancia
distancia_s = dist(entrada_s)

# Llamando al método Hclust

# Primero usamos en metodo complete

metodo = "complete"
cluster_s = hclust(distancia_s, method = metodo)
plot(cluster_s)
corte_s = cutree(cluster_s, k=4)
head(corte_s)

plot(datos_s[,1:3],
     col = corte_s)

matricom=table(datos_s$V4,corte_s)

#Presicion complete
prescom = sum(diag(matrizcom))/sum(matrizcom)

# Luego usamos en metodo single

metodo = "single"
cluster_s = hclust(distancia_s, method = metodo)
plot(cluster_s)
corte_s = cutree(cluster_s, k=4)
head(corte_s)

plot(datos_s[,1:3],
     col = corte_s)

matrizsin=table(datos_s$V4,corte_s)
pressin = sum(diag(matrizsin))/sum(matrizsin)
# Finalmente usamos en metodo average

metodo = "average"
cluster_s = hclust(distancia_s, method = metodo)
plot(cluster_s)
corte_s = cutree(cluster_s, k=4)
head(corte_s)

plot(datos_s[,1:3],
     col = corte_s)

matrizavg=table(datos_s$V4,corte_s)
presavg = sum(diag(matrizavg))/sum(matrizavg)
#----- FCLUSTER JERÁRQUICOS-----#
# Fin s.csv

#Inicio help.csv
# Leer los csv
datos_help = read.csv(file = "help.csv",header = F)
table(datos_help$V4)

#----- FUNCION DE ASIGNAR CLASES ---#

clase_help <- function(numero){
  if (numero < -1)
    return(1)
  else if (numero < 2)
    return(2)
  else
    return(3)
}

for (x in 1:nrow(datos_help)) {
  datos_help$V4[x] = clase_help(datos_help$V4[x])
}

table(datos_help$V4)
plot(datos_help[,1:3], col = datos_help$V4)
scatterplot3d(datos_help$V1, datos_help$V2, datos_help$V3,color = datos_help$V4)
#----- F FUNCION DE ASIGNAR CLASES ---#

#----- KMEDIAS ---#
kmedias_help = kmeans(x = datos_help[, 1:3],
                      centers = 3)

#Matriz de Confusion
matrizk=table(kmedias_help$cluster, datos_help$V4)

#Presicion kmedias
preskmedias = sum(diag(matrizk))/sum(matrizk)

#----- FKMEDIAS ---#

#---- ANALISIS EXPLORATORIO ----#
#• Cuántos clústers ve en el dataset help ?
# 3 clusters
#• Qué pasa al aplicar la regla de asignación de clases en este dataset?
# La regla no asigna bien las clases ya que no divide los espirales bien
#• Qué solución daría para asignar de manera correcta los valores de las clases y pueda analizar el
#desempeño del algoritmo de clustering de manera correcta?
# Creamos una funcion que asigne correctamente las clases dependiendo de las instancias
# de las S del dataset

#---- F ANALISIS EXPLORATORIO ----#


#----- CLUSTER JERÁRQUICOS-----#

# Primero buscamos la Matriz de Distancia 

# Copiamos el dataset en una variable nueva
entrada_help = datos_help

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada_help$V4 = NULL

# De DataFrame a Matrix
entrada_help = as.matrix(entrada_help)

# Matriz de distancia
distancia_help = dist(entrada_help)

# Llamando al método Hclust

# Primero usamos en metodo complete

metodo = "complete"
cluster_help = hclust(distancia_help, method = metodo)
plot(cluster_help)
corte_help = cutree(cluster_help, k=3)
head(corte_help)

plot(datos_help[,1:3],
     col = corte_help)

matrizcom=table(datos_help$V4,corte_help)

#Presicion complete
prescom = sum(diag(matrizcom))/sum(matrizcom)

# Luego usamos en metodo single

metodo = "single"
cluster_help = hclust(distancia_help, method = metodo)
plot(cluster_help)
corte_help = cutree(cluster_help, k=3)
head(corte_help)

plot(datos_help[,1:3],
     col = corte_help)

matrizsin=table(datos_help$V4,corte_help)
pressin = sum(diag(matrizsin))/sum(matrizsin)
# Finalmente usamos en metodo average

metodo = "average"
cluster_help = hclust(distancia_help, method = metodo)
plot(cluster_help)
corte_help = cutree(cluster_help, k=3)
head(corte_help)

plot(datos_help[,1:3],
     col = corte_help)

table(datos_help$V4,corte_help)
presavg = sum(diag(matrizavg))/sum(matrizavg)
#----- FCLUSTER JERÁRQUICOS-----#
# Fin help.csv

# Inicio guess.csv
# Leer los csv
datos_guess = read.csv(file = "guess.csv",header = F)
plot(datos_guess)
#----CODO DE JAMBU----#
InerciaIC = rep(0, 30)
for (k in 1:30) {
  guess = kmeans(datos_guess, k)
  InerciaIC[k] = guess$tot.withinss
}
plot(InerciaIC, col = "blue", type = "b")

#Elijo k=3

#----F CODO DE JAMBU----#

#----- KMEDIAS ---#
kmedias_guess = kmeans(x = datos_guess[, 1:2],
                       centers = 3)

#----- FKMEDIAS ---#

#----- CLUSTER JERÁRQUICOS-----#

# Primero buscamos la Matriz de Distancia 

# Copiamos el dataset en una variable nueva
entrada_guess = datos_guess

# De DataFrame a Matrix
entrada_guess = as.matrix(entrada_guess)

# Matriz de distancia
distancia_guess = dist(entrada_guess)

# Llamando al método Hclust

# Primero usamos en metodo complete

metodo = "complete"
cluster_guess = hclust(distancia_guess, method = metodo)
plot(cluster_guess)
corte_guess = cutree(cluster_guess, k=3)
head(corte_guess)

plot(datos_guess[,1:2],
     col = corte_guess)

# Luego usamos en metodo single

metodo = "single"
cluster_guess = hclust(distancia_guess, method = metodo)
plot(cluster_guess)
corte_guess = cutree(cluster_guess, k=3)
head(corte_guess)

plot(datos_guess[,1:2],
     col = corte_guess)

# Finalmente usamos en metodo average

metodo = "average"
cluster_guess = hclust(distancia_guess, method = metodo)
plot(cluster_guess)
corte_guess = cutree(cluster_guess, k=3)
head(corte_guess)

plot(datos_guess[,1:2],
     col = corte_guess)

#----- FCLUSTER JERÁRQUICOS-----#
# Fin guess.csv

# Inicio a_big.csv
# Leer los csv
datos_abig = read.csv(file = "a_big.csv",header = F)
table(datos_abig$V3)

plot(datos_abig$V1,datos_abig$V2,xlab = "V1",ylab = "V2")


# Fin a_big.csv