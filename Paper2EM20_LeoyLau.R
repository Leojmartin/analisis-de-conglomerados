# PAPER 2 - CO5316 Analisis de Datos
# Leonardo Martin 14-10628 - Laura Villalba 11-11073
library(dplyr)
library(ggplot2)
library(corrplot)
library(cluster)
library(factoextra)
library(NbClust)

# leemos los datos
# 
datos <- read.table("Datos.txt", header = T)
datos <- na.omit(datos)


# tomamos el continente europeo
# 
datos <- filter(datos, Continente == "Europa")

# quitamos la columna que indica el continente
# 
datos <- select(datos, -Continente)

str(datos)

datos$Maternal_mortalityRatio = as.character(datos$Maternal_mortalityRatio)
datos$Maternal_mortalityRatio = as.numeric(datos$Maternal_mortalityRatio)

datos$Adolescent_birthRate = as.character(datos$Adolescent_birthRate)
datos$Adolescent_birthRate = as.numeric(datos$Adolescent_birthRate)

datos$parliament = as.character(datos$parliament)
datos$parliament = as.numeric(datos$parliament)

datos$population_female = as.character(datos$population_female)
datos$population_female = as.numeric(datos$population_female)

datos$labor.female = as.character(datos$labor.female)
datos$labor.female = as.numeric(datos$labor.female)

datos$labor_male = as.character(datos$labor_male)
datos$labor_male = as.numeric(datos$labor_male)

str(datos)

row.names(datos) <- datos$Country

#
# Analisis Exploratorio de los datos
# 
 
g <- ggplot(data = datos, aes(clase))
g + geom_bar(aes(fill = clase), width = 0.5) + 
  theme(axis.text.x = element_text(vjust = 0.6)) + 
  labs (title = "Histograma de Human Development", subtitle = "Europa")
 

# 
# Correlacion entre las variables
crr <- round(cor(datos[,2:10]), 2)
corrplot(crr)


# 
# # plot entre population.female y population.male
plot(datos$population_female, datos$population_male, main = "population.female vs population.male",
     xlim = c(0, 105))
text(datos$population_female, datos$population_male, datos$Country, pos = 4, cex = 0.75)

# plot entre labour.female y labour.male
plot(datos$labor.female, datos$labor_male, main = "labor.female vs labor.male", xlim = c(0, 75) )
text(datos$labor.female, datos$labor_male, datos$Country, pos = 4, cex = 0.75)

# plot entre maternal.mortality.ratio y adolescent.birth.rate
plot(datos$Maternal_mortalityRatio, datos$Adolescent_birthRate,
     main = " maternal.mortality.ratio vs adolescent.birth.rate", xlim = c(0,45))
text(datos$Maternal_mortalityRatio, datos$Adolescent_birthRate, datos$Country, pos = 4, cex = 0.75)

# estandarizamos los datos
datos[,2:10] <- scale(datos[,2:10])


#
# Haremos el calculo de la distancia de mahalanobis para crear la matriz
# de distancias y pasarlo a la funcion hclust.
# Para ello necesitamos la media por columnas y la matriz de covarianzas
# 
med <- colMeans(datos[,2:10])
matCovarianzas <- cov(datos[,2:10])
distM <- mahalanobis(datos[,2:10], med, matCovarianzas)

# 
# vemos la distancia de mahalanobis de cada pais
#
datos$Country[which(distM == min(distM))]
datos$Country[which(distM < mean(distM))]
datos$Country[which(distM > mean(distM))]
datos$Country[which(distM == max(distM))]

 # Calculamos la distancia euclidea de la distancia de mahalanobis
# 
distancia <- dist(distM)     

# metodo de Conglomerados:
# 
# --------------------------------  Conglomerados Jerarquicos
# 

# Vecinos mas lejanos
hcclink.euro = hclust(distancia)
#graficamos:
plot(hcclink.euro, labels = datos$Country, main = "Vecinos mas lejanos")

#
# Asociacion promedio
# hcalink.euro = hclust(distancia, method = "average")
# #graficamos:
# plot(hcclink.euro, labels = datos$Country, main = "Asociacion promedio")

#
# Centroide
hccenlink.euro = hclust(distancia, method = "centroid")
#graficamos:
plot(hcclink.euro, labels = datos$Country, main = "Centroide")


# agrupamos en k = 4 grupos, vecinos mas lejanos
# plot(hcclink.euro,  labels = datos$Country, main = "Vecinos mas lejanos")
# rect.hclust(hcclink.euro, k = 4, border = "red")

fviz_dend(x = hcclink.euro, k = 4, cex = 0.8) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Cluster Jerarquico",
       subtitle = "Vecinos mas lejanos, K=4")

#
# agrupamos en k = 4 grupos, centroide
# plot(hccenlink.euro,  labels = datos$Country, main = "Centroide")
# rect.hclust(hccenlink.euro, k = 4, border = "red")

fviz_dend(x = hccenlink.euro, k = 4, cex = 0.8) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Cluster Jerarquico",
       subtitle = "Centroide, K=4")



# ------------------------------- Conglomerados No Jerarquicos
#  
# utilizaremos el metodo no jerarquico k-means el cual utiliza el criterio de la incercia
# indicando el modelo optimo a partir de los grupos que tengan una distancia intra grupal minima
# y una distancia inter grupal maxima
# 
# 4 grupos
kmean.euro <- kmeans(datos[,2:10], 4)
#
# viendo los resultados tenemos:
# 4 clusters de 16, 8, 5, 15 datos para un total de 44 datos.
# luego tenemos 4 centroides dados en Cluster means
# luego tenemos la posicion de cada pais en su respectivo grupo
# Para Within cluster sum of squares by cluster: 
# 46.16455 36.45948 12.00912 57.39685
# tenemos la inercia interna de cada grupo, siendo el grupo 5 el que tiene mayor inercia interna
# para (between_SS / total_SS = 60.7 %) buscamos que este lo mas cerca de 100%


#---
# Vemos graficamente la distribucion de los paises tomando 4 conglomerados
# 
# con kmean
fviz_cluster(object = kmean.euro, data = datos[,2:10], repel = T, main = "K-means")

# con asociacion promedio
# fviz_cluster( main = "asociacio promedio", list(data = datos[,2:10], cluster = cutree(hcalink.euro, k = 5)))

# vecino mas lejano
# fviz_cluster( main = "vecino mas lejano", list(data = datos[,2:10], cluster = cutree(hcclink.euro, k = 5)))


#
# Haciendo la prueba para verificar cuantos conglomerados son recomendados a usar
ncong = NbClust(data = datos[,2:10], distance = "euclidean", min.nc=2, max.nc=10,
                method = "kmeans", index = "alllong")
# NOTA: utilizando cualquiera de las distancias obtenemos que 3 es el mejor numero de clusters

fviz_nbclust(datos[,2:10], kmeans, method = "silhouette") +
  geom_vline(xintercept = 3, linetype = 2) 

fviz_nbclust(ncong)

#
# haciendo los graficos utilizando 3 conglomerados
#fviz_cluster(list(data = datos[,2:10], cluster = cutree(hcalink.euro, k = 3)))
#


kmean.euro <- kmeans(datos[,2:10], 3)
fviz_cluster(object = kmean.euro, data = datos[,2:10], repel = T, main = "K-means")


fviz_dend(x = hcclink.euro, k = 3, cex = 0.8) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Cluster Jerarquico",
       subtitle = "Centroide, K=4")
fviz_cluster( main = "vecino mas lejano", repel = T, list(data = datos[,2:10], cluster = cutree(hcclink.euro, k = 3)))
fviz_cluster( main = "centroide", repel = T, list(data = datos[,2:10], cluster = cutree(hccenlink.euro, k = 3)))



# mapa de calor
mat <- as.matrix(datos[,2:10])
heatmap(mat, scale = "none", 
        distfun = function(x){dist(x, method="euclidean")}, 
        hclustfun = function(x){hclust(x,method="average")}, cexRow = 0.8, cexCol = 0.8)






