#Reading entrada_1.csv

#K-Means Example
#Reading.csv
df = read.csv("entrada_4.csv", row.names = 1)
df = rbind(df, c("-100","-100","2"))
centros = rbind(cbind(-5,10),
                cbind(6,-4),
                cbind(7,-8),
                cbind(-6,-0))
cl = kmeans(x = df[, 1:2], centers = 4)
plot(df[,1:2], col = cl$cluster)
points(cl$centers[,c("x", "y")],
       col = 1:4,
       pch = 19,
       cex = 2)
cl$centers
########################################################################3
#H CLusters
#Si df es un df

entrada.num = df
entrada.num$class = NULL
entrada.num = as.matrix(entrada.num)
distancia = dist(entrada.num)

cluster = hclust(distancia, method = "average")
dendrograma = as.dendrogram(cluster)
plot(cluster)
corte = cut(dendrograma, h = 1)$upper
plot(corte)
ct = cutree(cluster, k = 4)

plot(df$x, df$y,col=ct)
table(ct, df$class)

#Conseguir beunos centroides
#30 podria funcioanr bien
for(i in 1:n)
{
  sub.df = df[sample(1:nrow(df), size = 20, replace = F), c("x", "y")]
  kmedias.pre = kmeans(sub.df[, 1:2], 4)
  medias.vddero = kmeans(sub.df[, 1:2], centers = kmedias.pre$centers)
}

#Escojo la emdiana
#Escojo la media
#Elimino x valores y luego promedio

