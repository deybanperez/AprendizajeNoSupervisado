#Function that install a required package if this is not installed yet
install = function(pkg)
{
  # If is is installed does not install packages
  if (!require(pkg, character.only = TRUE))
  {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE, respos = "http://cran.rstudio.com/"))
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
#Function to evaluate H-Clustering 3D
eval_hclust_3D = function(distance, mode, centroids, input, dataname)
{
  model = hclust(distance, method = mode)
  model_cut= cutree(model, k = centroids)
  plot3d(x = input[,1], y = input[,2], z = input[,3], type = "s",
         main = paste(c("Data set", dataname), collapse = " "), sub = paste(c(mode,"H-Clust Algorithm"), collapse = " "),
         xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
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
  auxiliar = matrix(0, nrow = nrow(confusionMatrix), ncol = nrow(confusionMatrix))
  
  for (i in 1:ncol(confusionMatrix))
  {
    positions = which(confusionMatrix == max(confusionMatrix), arr.ind = T)
    
    #If the column is empty
    if(zeros_colum(auxiliar[, positions[1,1]]))
    {
      auxiliar[, positions[1,1]] = confusionMatrix[,positions[1,2]]
      confusionMatrix[,positions[1,2]] = vector(mode = "numeric", length = nrow(confusionMatrix))
    }else
    {
      for (j in 1:ncol(auxiliar))
      {
        if(zeros_colum(auxiliar[,j]))
          break
      }
      
      if(auxiliar[positions[1,1], positions[1,1]] >= confusionMatrix[positions[1,1], positions[1,2]])
      {
        auxiliar[, j] = confusionMatrix[,positions[1,2]]
        confusionMatrix[,positions[1,2]] = vector(mode = "numeric", length = nrow(confusionMatrix))
      }
      else
      {
        auxiliar[, j] = auxiliar[, positions[1,1]]
        auxiliar[, positions[1,1]] = confusionMatrix[,positions[1,2]]
        confusionMatrix[,positions[1,2]] = vector(mode = "numeric", length = nrow(confusionMatrix))
      }
    }
  }
  rownames(auxiliar) <- rownames(auxiliar, do.NULL = FALSE, prefix = "TC")
  colnames(auxiliar) <- colnames(auxiliar, do.NULL = FALSE, prefix = "PC")

  return(auxiliar)
}


#Function that evalue is all the column is zero
zeros_colum = function(column)
{
  for (i in 1:length(column))
  {
    if(column[i] > 0)
      return(FALSE)
  }
  return(TRUE)
}

#Function that return the sum of the Diagonal 
sum_diagonal = function(matrix)
{
  returnValue = 0;
  
  for (i in 1:ncol(matrix))
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

confusion_matrix_evaluation_deyban = function(confusionMatrix, numberRows)
{
  hits = sum_diagonal(confusionMatrix)
  hits
  hitRate = hits / numberRows * 100
  return(hitRate)
}

get_colors = function(number, df)
{
  set.seed(22)
  index = sample(1:502,number, replace = F)
  colors = colors(distinct = T)[index]
  return(colors[as.integer(df$V5-min(df$V5)+1)])
}


euc.dist = function(x1, y1, x2, y2)
{
  return( sqrt( ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)) )
}

K_Means_Deyban = function(df, k, c = NULL)
{
  #Selecting random centroids
  centroids = matrix(0L, nrow = k, ncol = 2)
  
  # If You dont hace centroids, choose random centroids
  if(is.null(c))
  {
    for (i in 1:nrow(centroids))
    {
      centroids[i,1] = runif(1, min(df$V1), max(df$V1))
      centroids[i,2] = runif(1, min(df$V2), max(df$V2))
    }
  }else #Take centroids from parameters
    centroids = c
  
  #Initializing clusters vector
  clusters = vector(mode = "numeric", length = nrow(df))
  #Initializing Distance Matrix
  distance_matrix = matrix(0L, nrow = nrow(df), ncol = nrow(centroids))
  
  #Initializing the number of elementes by cluster
  c1 = 0
  c2 = 0
  c3 = 0
  
  # Beginning the algorithm
  for(n in 1:50)
  {
    #Calculing the distance between points and centroids
    for (i in 1:nrow(centroids))
      distance_matrix[,i] = euc.dist(centroids[i,1],
                                     centroids[i,2], df[,1], df[,2])
    #Assigning elements to a cluster
    for (i in 1:length(clusters))
      clusters[i] = which.min(distance_matrix[i,])
    
    #Finding new centroids
    for (i in 1:nrow(centroids))
    {
      centroids[i,1] = (1/length(clusters[clusters == i])) *
        sum(df[clusters[clusters == i],1])
      
      centroids[i,2] = (1/length(clusters[clusters == i])) *
        sum(df[clusters[clusters == i],2])
    }
    
    #Evaluating the change about clusters between iteration
    if(n > 1)
    {
      c1.temp = length(clusters[clusters == 1])
      c2.temp = length(clusters[clusters == 2])
      c3.temp = length(clusters[clusters == 3])  
      
      if((c1 == c1.temp) && (c2 == c2.temp) && (c3 == c3.temp)) # If the elements between iterations dont have changes
      {
        #return
        my_list = list(centroids, clusters)
        return(my_list)
      }
      else # Update values
      {
        c1 = c1.temp
        c2 = c2.temp
        c3 = c3.temp
      }
    }
    
  }
  #Return
  my_list = list(centroids, clusters)
  return(my_list)
}

