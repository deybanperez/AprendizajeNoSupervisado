?kmeans
?rnorm
?points

##############################################
#             EXAMPLE OF K-MEANS             #
##############################################
require(graphics)

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 3)) #if k changes change the col parameter on the points function
plot(x, col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8, cex = 2)

# sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)

## cluster centers "fitted" to each obs.:
fitted.x <- fitted(cl);  head(fitted.x)
resid.x <- x - fitted(cl)

## Equalities : ----------------------------------
cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
      c(ss(fitted.x), ss(resid.x),    ss(x)))
stopifnot(all.equal(cl$ totss,        ss(x)),
          all.equal(cl$ tot.withinss, ss(resid.x)),
          ## these three are the same:
          all.equal(cl$ betweenss,    ss(fitted.x)),
          all.equal(cl$ betweenss, cl$totss - cl$tot.withinss),
          ## and hence also
          all.equal(ss(x), ss(fitted.x) + ss(resid.x))
)

kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))

## random starts do help here with too many clusters
## (and are often recommended anyway!):
(cl <- kmeans(x, 5, nstart = 25))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)

##############################################
#                 KMEANS FUNCTION            #
##############################################

KMEANS <- function(file, k, iter){
  entrada <- read.csv(file, header = T, sep = ",", row.names=1)
  entrada1 = data.frame(entrada[1], entrada[2])
  # a 2-dimensional example
  colnames(entrada1) <- c("x", "y")
  (cl <- kmeans(entrada1, k, iter.max= iter)) #if k changes change the col parameter on the points function
  plot(entrada1, col = cl$cluster)
  points(cl$centers, col = 1:k, pch = 8, cex = 2)
}

##############################################
#                 FIRST EXAMPLE              #
##############################################

#KMEANS
entrada <- read.csv("entrada_1.csv", header = T, sep = ",", row.names=1)
entrada1 = data.frame(entrada$X, entrada$Y)
# a 2-dimensional example
colnames(entrada1) <- c("x", "y")
(cl <- kmeans(entrada1, 4)) #if k changes change the col parameter on the points function
plot(entrada1, col = cl$cluster)
points(cl$centers, col = 1:4, pch = 8, cex = 2)

KMEANS("entrada_1.csv", 4, 10)

##############################################
#                 SECOND EXAMPLE             #
##############################################
#KMEANS
entrada <- read.csv("entrada_2.csv", header = T, sep = ",", row.names=1)
entrada2 = data.frame(entrada$X, entrada$Y)
# a 2-dimensional example
colnames(entrada2) <- c("x", "y")
(cl <- kmeans(entrada2, 4)) #if k changes change the col parameter on the points function
plot(entrada2, col = cl$cluster)
points(cl$centers, col = 1:4, pch = 8, cex = 2)

KMEANS("entrada_2.csv", 4, 10)

##############################################
#                 THIRD EXAMPLE             #
##############################################
KMEANS("entrada_3.csv", 4, 10)

##############################################
#                 FOURTH EXAMPLE             #
##############################################
?matrix
c <- matrix(c(7, 9, 8, 7, -10 ,-8, -7, -7.5),nrow = 2, ncol = 2)

KMEANS("entrada_4.csv", 4, 5)


##############################################
#           Plantilla para knn grandes       #
##############################################
#si X es la matrix Xmxp donde m son filas y p columnas
# y si m>>1000

#n=100
#muestra = X[sample(1:nrow(X), size=n, replace=F)]
#presalida = kmeans(muestra,k)
#salida=kmeans(X, k=k), centers=presalida$centers, iter.max=y, algorithm= )


##############################################
#              Cluster Jerarquicos           #
##############################################
data(iris)
datos=iris
datos$Species = NULL
datos = as.matrix(datos)
distancia = dist(datos)
cluster = hclust(distancia)
plot(cluster)

?hclust

cluster = hclust(distancia, method = "single")
plot(cluster)

dendrograma = as.dendrogram(cluster)
plot(dendrograma)
corte = cut(dendrograma, h=3)$upper
plot(corte)

?cutree
corte_2 = cutree(cluster,k=4)
table(corte_2)


entrada <- read.csv("entrada_1.csv", header = T, sep = ",", row.names=1)
entrada1 = data.frame(entrada$X, entrada$Y)
datos = as.matrix(entrada1)
distancia = dist(datos)
cluster = hclust(distancia, method = "single")
corte_3 = cutree(cluster, k=4)
table(corte_3)

entrada <- read.csv("entrada_4.csv", header = T, sep = ",", row.names=1)
entrada1 = data.frame(entrada[1], entrada[2])
datos = as.matrix(entrada1)
distancia = dist(datos)
cluster = hclust(distancia, method = "average")
corte_3 = cutree(cluster, k=4)
table(corte_3, entrada$class)
