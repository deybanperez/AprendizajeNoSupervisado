K_Means_Deyban = function(df, k, class)
{
  #Selecting random centroids
  centroids = matrix(0L, nrow = k, ncol = 2)
  
  for (i in 1:nrow(centroids))
  {
    centroids[i,1] = runif(1, min(df$V1), max(df$V1))
    centroids[i,2] = runif(1, min(df$V2), max(df$V2))
  }
  
  #Initializing clusters vector
  clusters = vector(mode = "numeric", length = nrow(df))
  #Initializing Distance Matrix
  distance_matrix = matrix(0L, nrow = nrow(df), ncol = nrow(centroids))
  
  for(n in 1:30)
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
      centroids[i,1] = (1/sum(clusters[clusters == i])) *
        sum(df[clusters[clusters == i],1])
      
      centroids[i,2] = (1/sum(clusters[clusters == i])) *
        sum(df[clusters[clusters == i],2])
    }
    
    #Evaluating the change about clusters between iterations
    if(n == 1)
    {
      c1 = sum(clusters[clusters == 1])
      c2 = sum(clusters[clusters == 2])
      c3 =sum(clusters[clusters == 3])
    }else
    {
      c1.temp = sum(clusters[clusters == 1])
      c2.temp = sum(clusters[clusters == 2])
      c3.temp = sum(clusters[clusters == 3])
      
      if( ((c1 - c1.temp) == 0) && ((c2 - c1.temp) == 0)
          && ((c3 - c3.temp) == 0))
      {
        my_list = list(centroids, clusters)
        return(my_list)
      }else
      {
        c1 = c1.temp
        c2 = c2.temp
        c3 =c3.temp
      }
    }
  }
  my_list = list(centroids, clusters)
  return(my_list)
}

