order_confusion_matrix = function(confusionMatrix)
{
  auxi = matrix(0, nrow = nrow(prueba), ncol = ncol(prueba))
  
  for (i in 1:ncol(prueba))
  {
    positions = which(prueba == max(prueba), arr.ind = T)
    
    #If the column is empty
    if(zeros_colum(auxi[, positions[1,1]]))
    {
      write("Entra en el if", stdout())
      auxi[, positions[1,1]] = prueba[,positions[1,2]]
      prueba[,positions[1,2]] = vector(mode = "numeric", length = nrow(prueba))
    }else
    {
      write("Entra en el else", stdout())
      
      for (j in 1:ncol(auxi))
      {
        if(zeros_colum(auxi[,j]))
          break
      }
      
      write(sprintf("%f", j), stdout())
      
      if(auxi[positions[1,1], positions[1,1]] >= prueba[positions[1,1], positions[1,2]])
      {
        auxi[, j] = prueba[,positions[1,2]]
        prueba[,positions[1,2]] = vector(mode = "numeric", length = nrow(prueba))
      }
      else
      {
        auxi[, j] = auxi[, positions[1,1]]
        auxi[, positions[1,1]] = prueba[,positions[1,2]]
        prueba[,positions[1,2]] = vector(mode = "numeric", length = nrow(prueba))
      }
    }
    
    auxi
  }
  return(auxiliar)
}