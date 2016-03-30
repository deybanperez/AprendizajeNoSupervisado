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

###################
#Loading Libraries#
###################


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
df_a$V3 = as.numeric(df_a$V3)
df_a$V3[df_a$V3 == 2] = 3
df_a$V3[df_a$V3 == 1] = 2
df_a$V3[df_a$V3 == 0] = 1
df_a$V3 = as.factor(df_a$V3)
#Visualizing data set
plot(df_a[,1:2], col = df_a$V3)
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_a = kmeans(x = df_a[, 1:2], centers = 3)
plot(df_a[, 1:2], col = model_kmeans_a$cluster,
     main = "Data Set a.csv", sub = "K-means Algorithm",
     xlab = "Feature 1", ylab = "Feature 2")
points(model_kmeans_a$centers[,c("V1", "V2")],
       col = 7:9,
       pch = 19,
       cex = 2)
table(True = df_a$V3, Prediction = model_kmeans_a$cluster)
############
#H-Clusters#
############
input_hierarchical_a = df_a
input_hierarchical_classes_a = as.numeric(df_a$V3)
input_hierarchical_classes_a[input_hierarchical_classes_a == 1] = 0
input_hierarchical_classes_a[input_hierarchical_classes_a == 2] = 1
input_hierarchical_classes_a[input_hierarchical_classes_a == 0] = 2

input_hierarchical_a$V3 = NULL
input_hierarchical_a = as.matrix(input_hierarchical_a)
hierarchical_distance_a = dist(input_hierarchical_a)
###############
#Single method#
###############
model_hierarchical_single_a = hclust(hierarchical_distance_a, method = "single")
plot(model_hierarchical_single_a)
model_hierarchical_single_cut_a = cutree(model_hierarchical_single_a, k = 3)
plot(x = input_hierarchical_a[,1], y = input_hierarchical_a[,2],
     main = "Dataset a.csv", sub = "Single Hierarchical Clustering Algorithm",
     xlab = "Feature 1", ylab = "Feature 2",
     col = model_hierarchical_single_cut_a)
table(True = input_hierarchical_classes_a, Predicted = model_hierarchical_single_cut_a)
#################
#Complete method#
#################
model_hierarchical_complete_a = hclust(hierarchical_distance_a, method = "complete")
plot(model_hierarchical_complete_a)
model_hierarchical_complete_cut_a = cutree(model_hierarchical_complete_a, k = 3)
plot(x = input_hierarchical_a[,1], y = input_hierarchical_a[,2],
     main = "Dataset a.csv", sub = "Complete Hierarchical Clustering Algorithm",
     xlab = "Feature 1", ylab = "Feature 2",
     col = model_hierarchical_complete_cut_a)
table(True = input_hierarchical_classes_a, Predicted = model_hierarchical_complete_cut_a)
#################
#Average method#
#################
model_hierarchical_average_a = hclust(hierarchical_distance_a, method = "average")
plot(model_hierarchical_average_a)
model_hierarchical_average_cut_a = cutree(model_hierarchical_average_a, k = 3)
plot(x = input_hierarchical_a[,1], y = input_hierarchical_a[,2],
     main = "Dataset a.csv", sub = "Average Hierarchical Clustering Algorithm",
     xlab = "Feature 1", ylab = "Feature 2",
     col = model_hierarchical_average_cut_a)
table(True = input_hierarchical_classes_a, Predicted = model_hierarchical_average_cut_a)
############################################################################################
############################----------------a_big.csv-------------##########################
############################################################################################
###################
#Reading a_big.csv#
###################
df_a_big = read.csv("a_big.csv",header = F)
#Converting class column to a factor type
df_a_big$V3[df_a_big$V3 == 2] = 3
df_a_big$V3[df_a_big$V3 == 1] = 2
df_a_big$V3[df_a_big$V3 == 0] = 1
#Visualizing data set
plot(df_a_big[,1:2], col = df_a_big$V3)
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_a_big = kmeans(x = df_a_big[, 1:2], centers = 3)
plot(df_a_big[, 1:2], col = model_kmeans_a_big$cluster,
     main = "Data set a_big.csv", sub = "K-meas algorithm",
     xlab = "Feature 1", ylab = "Feature 2")
points(model_kmeans_a_big$centers[,c("V1", "V2")],
       col = 6:8,
       pch = 19,
       cex = 2)
table_model_kmeans_a_big = table(True = df_a_big$V3, Prediction = model_kmeans_a_big$cluster)