#setwd("C:/Users/deyban.perez/Documents/Repos/AprendizajeNoSupervisado")

####################
#Defining functions#
####################
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

#Function to eval k-means model
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

eval_kmeans_3D = function(df, cstart, cfinish, k, dataname)
{
  model_kmeans = kmeans(x = df[, cstart:cfinish], centers = k)
  plot3d(x = df$V1, y = df$V2, z = df$V3, type = "s" ,col = df$class,
         xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
         main = paste(c("Data Set",dataname), collapse = " ")
         
  points3d(model_kmeans$centers[,c("V1", "V2")],
         col = 6:8,
         pch = 19,
         cex = 2)
  
  return(model_kmeans)
}


#Function to eval k-means model especial
eval_kmeans_especial = function(df, cstart, cfinish, k, dataname)
{
  model_kmeans = kmeans(x = df[, cstart:cfinish], centers = k)
  return(model_kmeans)
}


#Function to evaluate H-Clustering
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
confusion_matrix_evaluation = function(confusionMatrix, dataSet)
{
  hits = sum_diagonal(confusionMatrix)
  hits
  hitRate = hits / nrow(dataSet) * 100
  missRate = 100 - hitRate
  write(sprintf("Tasa de acierto: %f", hitRate), stdout())
  write(sprintf("Tasa de fallo: %f", missRate), stdout())
}

#Confusion matrix convertion
confusion_matrix_convertion = function(df, model)
{
  matrix = table(True = df$V3, Prediction = model$cluster)
  return(matrix[, max.col(matrix)])
}

confusion_matrix_convertion_hclust = function(df, model)
{
  matrix = table(True = df$V3, Prediction = model)
  return(matrix[, max.col(matrix)])
}

#Preprocessing function
pre_processing = function(df)
{
  df$V3 = as.numeric(df$V3)
  df$V3[df$V3 == 2] = 3
  df$V3[df$V3 == 1] = 2
  df$V3[df$V3 == 0] = 1
  
  return(df)
}

pre_processing_especial = function(df)
{
  df$V11 = as.numeric(df$V11)
  df$V11[df$V11 == 1] = 2
  df$V11[df$V11 == 0] = 1
  
  return(df)
}
#####################
#Intsalling Packages#
#####################
install("plot3D")
install("rgl")
###################
#Loading Libraries#
###################
library(plot3D)
library(rgl)

################
#Implementation#
################

############################################################################################
############################Comparing clustering algorithms#################################
############################################################################################
############################################################################################
############################----------------a.csv-------------##############################
############################################################################################
###############
#Reading a.csv#
###############
df_a = read.csv("a.csv",header = F)
#Converting class column to a factor type
df_a = pre_processing(df_a)
#Visualizing data set
plot(df_a[,1:2], col = df_a$V3)
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_a = eval_kmeans(df = df_a, cstart = 1, cfinish = 2, k = 3, dataname = "a.csv")
table_model_kmeans_a = confusion_matrix_convertion(df = df_a, model = model_kmeans_a)
table_model_kmeans_a
confusion_matrix_evaluation(table_model_kmeans_a, df_a)

############
#H-Clusters#
############
input_hierarchical_a = df_a
input_hierarchical_a$V3 = NULL
input_hierarchical_a = as.matrix(input_hierarchical_a)
hierarchical_distance_a = dist(input_hierarchical_a)
###############
#Single method#
###############
model_hierarchical_single_a = eval_hclust(distance = hierarchical_distance_a, mode = "single", centroids = 3, input = input_hierarchical_a, dataname = "a.csv")
table_model_hierarchical_single_a = table(True = df_a$V3, Prediction = model_hierarchical_single_a)
table_model_hierarchical_single_a
confusion_matrix_evaluation(confusionMatrix = table_model_hierarchical_single_a, dataSet = df_a)
#################
#Complete method#
#################
model_hierarchical_complete_a = eval_hclust(distance = hierarchical_distance_a, mode = "complete", centroids = 3, input = input_hierarchical_a, dataname = "a.csv")
table_model_hierarchical_complete_a = confusion_matrix_convertion_hclust(df = df_a, model = model_hierarchical_complete_a )
table_model_hierarchical_complete_a
confusion_matrix_evaluation(table_model_hierarchical_complete_a, df_a)
#################
#Average method#
#################
model_hierarchical_average_a = eval_hclust(distance = hierarchical_distance_a, mode = "average", centroids = 3, input = input_hierarchical_a, dataname = "a.csv")
table_model_hierarchical_average_a = confusion_matrix_convertion_hclust(df = df_a, model = model_hierarchical_average_a )
table_model_hierarchical_average_a
confusion_matrix_evaluation(table_model_hierarchical_average_a, df_a)
############################################################################################
############################----------------a_big.csv-------------##########################
############################################################################################
###################
#Reading a_big.csv#
###################
df_a_big = read.csv("a_big.csv",header = F)
#Converting class column to a factor type
df_a_big = pre_processing(df_a_big)
#Visualizing data set
plot(df_a_big[,1:2], col = df_a_big$V3)
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_a_big = eval_kmeans(df = df_a_big, cstart = 1, cfinish = 2, k = 3, dataname = "a_big.csv")
table_model_kmeans_a_big = confusion_matrix_convertion(df = df_a_big, model = model_kmeans_a_big)
table_model_kmeans_a_big
confusion_matrix_evaluation(table_model_kmeans_a_big, df_a_big)
############################################################################################
############################----------------good_luck.csv-------------##########################
############################################################################################
###################
#Reading good_lock.csv#
###################
df_good_luck = read.csv("good_luck.csv",header = F)
#Converting class column to a factor type
df_good_luck = pre_processing_especial(df_good_luck)
#Visualizing data set
plot(df_good_luck[,1:10], col = df_good_luck$V11)
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_good_luck = eval_kmeans_especial(df = df_good_luck, cstart = 1, cfinish = 10, k = 2, dataname = "good_luck.csv")
table_model_kmeans_good_luck = table(df = df_good_luck$V11, model = model_kmeans_good_luck$cluster)
table_model_kmeans_good_luck
confusion_matrix_evaluation(table_model_kmeans_good_luck, df_good_luck)
############
#H-Clusters#
############
input_hierarchical_good_luck = df_good_luck
input_hierarchical_good_luck$V11 = NULL
input_hierarchical_good_luck = as.matrix(input_hierarchical_good_luck)
hierarchical_distance_good_luck = dist(input_hierarchical_good_luck)
###############
#Single method#
###############
model_hierarchical_single_good_luck = eval_hclust_especial(distance = hierarchical_distance_good_luck, mode = "single", centroids = 2, input = input_hierarchical_good_luck, dataname = "good_luck.csv", cfinish = 10)
table_model_hierarchical_single_good_luck = table(df = df_good_luck$V11, model = model_hierarchical_single_good_luck)
table_model_hierarchical_single_good_luck
confusion_matrix_evaluation(confusionMatrix = table_model_hierarchical_single_good_luck, dataSet = df_good_luck)
#################
#Complete method#
#################
model_hierarchical_complete_good_luck = eval_hclust_especial(distance = hierarchical_distance_good_luck, mode = "complete", centroids = 2, input = input_hierarchical_good_luck, dataname = "good_luck.csv", cfinish = 10)
table_model_hierarchical_complete_good_luck = table(df = df_good_luck$V11, model = model_hierarchical_complete_good_luck)
table_model_hierarchical_complete_good_luck
confusion_matrix_evaluation(confusionMatrix = table_model_hierarchical_complete_good_luck, dataSet = df_good_luck)
#################
#Average method#
#################
model_hierarchical_average_good_luck = eval_hclust_especial(distance = hierarchical_distance_good_luck, mode = "average", centroids = 2, input = input_hierarchical_good_luck, dataname = "good_luck.csv", cfinish = 10)
table_model_hierarchical_average_good_luck = table(df = df_good_luck$V11, model = model_hierarchical_average_good_luck)
table_model_hierarchical_average_good_luck
confusion_matrix_evaluation(confusionMatrix = table_model_hierarchical_average_good_luck, dataSet = df_good_luck)
############################################################################################
############################----------------moon.csv-------------##########################
############################################################################################
###################
#Reading moon.csv#
###################
df_moon = read.csv("moon.csv",header = F)
#Converting class column to a factor type
df_moon = pre_processing(df_moon)
#Visualizing data set
plot(df_moon[,1:2], col = df_moon$V3)
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_moon = eval_kmeans(df = df_moon, cstart = 1, cfinish = 2, k = 2, dataname = "a_moon.csv")
table_model_kmeans_moon = confusion_matrix_convertion(df = df_moon, model = model_kmeans_moon)
table_model_kmeans_moon
confusion_matrix_evaluation(table_model_kmeans_moon, df_moon)
############
#H-Clusters#
############
input_hierarchical_moon = df_moon
input_hierarchical_moon$V3 = NULL
input_hierarchical_moon = as.matrix(input_hierarchical_moon)
hierarchical_distance_moon = dist(input_hierarchical_moon)
###############
#Single method#
###############
model_hierarchical_single_moon = eval_hclust(distance = hierarchical_distance_moon, mode = "single", centroids = 2, input = input_hierarchical_moon, dataname = "moon.csv")
table_model_hierarchical_single_moon = confusion_matrix_convertion_hclust(df = df_moon, model = model_hierarchical_single_moon)
table_model_hierarchical_single_moon
confusion_matrix_evaluation(confusionMatrix = table_model_hierarchical_single_moon, dataSet = df_moon)
#################
#Complete method#
#################
model_hierarchical_complete_moon = eval_hclust(distance = hierarchical_distance_moon, mode = "complete", centroids = 2, input = input_hierarchical_moon, dataname = "moon.csv")
table_model_hierarchical_complete_moon = confusion_matrix_convertion_hclust(df = df_moon, model = model_hierarchical_complete_moon)
table_model_hierarchical_complete_moon
confusion_matrix_evaluation(table_model_hierarchical_complete_moon, df_moon)
#################
#Average method#
#################
model_hierarchical_average_moon = eval_hclust(distance = hierarchical_distance_moon, mode = "average", centroids = 2, input = input_hierarchical_moon, dataname = "moon.csv")
table_model_hierarchical_average_moon = table(True = df_moon$V3, Prediction = model_hierarchical_average_moon)
table_model_hierarchical_average_moon
confusion_matrix_evaluation(table_model_hierarchical_average_moon, df_moon)
############################################################################################
############################----------------h.csv-------------##########################
############################################################################################
###################
#Reading h.csv#
###################
df_h = read.csv("h.csv",header = F)
df_h$class = 7*(df_h$V4-min(df_h$V4))/(max(df_h$V4)-min(df_h$V4))+1
plot3d(x = df_h$V1, y = df_h$V2, z = df_h$V3, type = "s" ,col = df_h$class,
       xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main = "Data Set h.csv")

plot3d(x = df_h$V1, y = df_h$V2, z = df_h$V3, type = "s" ,col = model_kmeans$cluster, xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main = paste(c("Data Set","h.csv"), collapse = " "))
points3d(model_kmeans$centers[,c("V1", "V2", "V3")],
         col = rainbow(7),
         pch = 8,
         cex = 22)

points3d(model_kmeans$centers[,c("V1", "V2", "V3")],
         col = 1:7,
         pch = 8, size = 22,
         cex = 22)


#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_h = eval_kmeans(df = df_h, cstart = 1, cfinish = 3, k = 7, dataname = "h.csv")
table_model_kmeans_moon = confusion_matrix_convertion(df = df_moon, model = model_kmeans_moon)
table_model_kmeans_moon
confusion_matrix_evaluation(table_model_kmeans_moon, df_moon)

############################################################################################
############################----------------s.csv-------------##########################
############################################################################################
###################
#Reading s.csv#
###################
df_s = read.csv("s.csv",header = F)
df_s$class = 7*(df_s$V4-min(df_s$V4))/(max(df_s$V4)-min(df_s$V4))+1
plot3d(x = df_s$V1, y = df_s$V2, z = df_s$V3, type = "s" ,col = df_s$class)
############################################################################################
############################----------------help.csv-------------##########################
############################################################################################
###################
#Reading help.csv#
###################
df_help = read.csv("help.csv",header = F)
df_help$class = 7*(df_help$V4-min(df_help$V4))/(max(df_help$V4)-min(df_help$V4))+1
plot3d(x = df_help$V1, y = df_help$V2, z = df_help$V3, type = "s" ,col = df_help$class)
############################################################################################
############################----------------guess.csv-------------##########################
############################################################################################
###################
#Reading guess.csv#
###################
df_guess = read.csv("guess.csv",header = F)
valor = rep(0,30)
for (k in 1:30)
{
  model_kmeans_guess_jambu = kmeans(df_guess, k)
  valor[k] = model_kmeans_guess_jambu$tot.withinss
}

plot(valor, col = "blue", type = "b")
model_kmeans_guess = eval_kmeans(df = df_guess, cstart = 1, cfinish = 2, k = 5, dataname = "guess.csv")
