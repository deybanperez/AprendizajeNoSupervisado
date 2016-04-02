#Function that install a required package if this is not installed yet
install = function(pkg)
{
  # If is is installed does not install packages
  if (!require(pkg, character.only = TRUE))
  {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE))
      stop(paste("load failure:", pkg))
  }
}

#Preprocessing function
pre_processing = function(df)
{
  df$V3 = as.numeric(df$V3)
  df$V3 = df$V3 - min(df$V3) + 1
  
  return(df)
}

#Preprocessing function for good_luck
pre_processing_especial = function(df)
{
  df$V11 = as.numeric(df$V11)
  df$V11 = df$V11 - min(df$V11) + 1
  
  return(df)
}

#Function to eval k-means model in 2D
eval_kmeans = function(df, cstart, cfinish, k, dataname)
{
  model_kmeans = kmeans(x = df[, cstart:cfinish], centers = k)
  plot(df[, cstart:cfinish], col = model_kmeans$cluster,
       main = paste(c("Data set", dataname), collapse = " "), sub = "K-means algorithm",
       xlab = "Feature 1", ylab = "Feature 2")
  points(model_kmeans$centers[,c("V1", "V2")],
         col = 6:8,
         pch = 19,
         cex = 2)
  
  return(model_kmeans)
}

#Function to eval k-means model especial for good_luck
eval_kmeans_especial = function(df, cstart, cfinish, k, dataname)
{
  model_kmeans = kmeans(x = df[, cstart:cfinish], centers = k)
  plot(df[,1:10], col = df$V11,
       main = paste(c("Matriz de Dispersion",dataname), collapse = " "),
       sub = "K-Means Algorithm")
  return(model_kmeans)
}

#Function to eval k-means on 3D data sets
eval_kmeans_3D = function(df, cstart, cfinish, k, dataname)
{
  model_kmeans = kmeans(x = df[, cstart:cfinish], centers = k)
  plot3d(x = df$V1, y = df$V2, z = df$V3, type = "s" ,col = model_kmeans$cluster,
         xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
         main = paste(c("Data Set",dataname), collapse = " "))
  return(model_kmeans)
}

#Function to eval H-Clust model in 2D
eval_hclust = function(distance, mode, centroids, input, dataname)
{
  model = hclust(distance, method = mode)
  model_cut= cutree(model, k = centroids)
  plot(x = input[,1], y = input[,2],
       main = paste(c("Data set", dataname), collapse = " "), sub = paste(c(mode,"H-Clust Algorithm"), collapse = " "),
       xlab = "Feature 1", ylab = "Feature 2",
       col = model_cut)
  return(model_cut)
}

#Function to evaluate H-Clustering especial
eval_hclust_especial = function(distance, mode, centroids, input, dataname,cfinish)
{
  model = hclust(distance, method = mode)
  model_cut= cutree(model, k = centroids)
  input = as.data.frame(input)
  plot(input[,1:cfinish],
       main = paste(c("Data set", dataname), collapse = " "), sub = paste(c(mode,"H-Clust Algorithm"), collapse = " "),
       col = model_cut)
  return(model_cut)
}


confusion_matrix = function(class, cluster)
{
  return(table(True = class, Predicted = cluster))
}

#Function that order the diagonal of a matrix
order_confusion_matrix = function(confusionMatrix)
{
  auxiliar = matrix(0, nrow = nrow(confusionMatrix), ncol = ncol(confusionMatrix))
  
  for (i in 1:ncol(confusionMatrix))
  {
    positions = which(confusionMatrix == max(confusionMatrix), arr.ind = T)
    auxiliar[, positions[1,1]] = confusionMatrix[,positions[1,2]]
    confusionMatrix[,positions[1,2]] = vector(mode = "numeric", length = nrow(confusionMatrix))
  }
  return(auxiliar)
}

#Function that return the sum of the Diagonal 
sum_diagonal = function(matrix)
{
  returnValue = 0;
  
  for (i in 1:nrow(matrix))
  {
    returnValue = returnValue + matrix[i,i]
  }
  
  return(returnValue)
}

#Function to calculate the hit rate, miss rate
confusion_matrix_evaluation = function(confusionMatrix, numberRows)
{
  hits = sum_diagonal(confusionMatrix)
  hits
  hitRate = hits / numberRows * 100
  missRate = 100 - hitRate
  write(sprintf("Tasa de acierto: %f", hitRate), stdout())
  write(sprintf("Tasa de fallo: %f", missRate), stdout())
}

get_colors = function(number, df)
{
  set.seed(22)
  index = sample(1:502,number, replace = F)
  colors = colors(distinct = T)[index]
  return(colors[as.integer(df$V5-min(df$V5)+1)])
}
