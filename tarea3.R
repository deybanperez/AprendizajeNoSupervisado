####################
#Defining functions#
####################

###################
#Loading Libraries#
###################


################
#Implementation#
################

#################################
#Comparing clustering algorithms#
#################################
#Reading a.csv
df_a = read.csv("a.csv",header = F)
#Converting class column to a factor type

for (i in 1:nrow(df_a))
{
  if(df_a$V3[i] == 2)
    df_a$V3[i] = 3
}

for (i in 1:nrow(df_a))
{
  if(df_a$V3[i] == "1")
    df_a$V3[i] = 2
}

for (i in 1:nrow(df_a))
{
  if(df_a$V3[i] == "0")
    df_a$V3[i] = 1
}
df_a$V3 = as.factor(df_a$V3)
#Visualizing data set
plot(df_a[,1:2], col = df_a$V3)
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans = kmeans(x = df_a[, 1:2], centers = 3)
plot(df_a[, 1:2], col = model_kmeans$cluster,
     main = "Data set a.csv", sub = "K-meas algorithm",
     xlab = "Feature 1", ylab = "Feature 2")
points(model_kmeans$centers[,c("V1", "V2")],
       col = 7:9,
       pch = 19,
       cex = 2)
table(True = df_a$V3, Prediction = model_kmeans$cluster)
############
#H-Clusters#
############
input_hierarchical = df_a
input_hierarchical_classes = df_a$V3
input_hierarchical$V3 = NULL
input_hierarchical = as.matrix(input_hierarchical)
hierarchical_distance = dist(input_hierarchical)
model_hierarchical_single = hclust(hierarchical_distance, method = "complete")
plot(model_hierarchical_single)
model_hierarchical_single_cut = cutree(model_hierarchical_single, k = 3)
plot(x = df_a$V1, y = df_a$V2, col = model_hierarchical_single_cut)
table(True = df_a$V3, Predicted = model_hierarchical_single_cut)

