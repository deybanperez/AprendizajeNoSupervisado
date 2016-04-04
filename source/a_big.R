###################
#Reading a_big.csv#
###################
df_a_big = read.csv("data/a_big.csv",header = F)
#Converting class column to a factor type
df_a_big = pre_processing(df_a_big)
#Visualizing data set
plot(df_a_big[,1:2], col = df_a_big$V3,
     xlab = "Feature 1", ylab = "Feature 2",
     main = "a_big.csv")
#########
#K-Means#
#########
model_kmeans_a_big = eval_kmeans(df = df_a_big, cstart = 1, cfinish = 2, k = 3, dataname = "a_big.csv")
table_model_kmeans_a_big = order_confusion_matrix(confusion_matrix(df_a_big$V3, model_kmeans_a_big$cluster))
table_model_kmeans_a_big
confusion_matrix_evaluation(table_model_kmeans_a_big, nrow(df_a_big))
######################
#Implementing K-Means#
######################

#Making a stratified sampling
############################################################################
#Calculating probabilities for each element into dataset
prob_1 = 1/sum(df_a_big[,"V3"] == 1);
prob_2 = 1/sum(df_a_big[,"V3"] == 2);
prob_3 = 1/sum(df_a_big[,"V3"] == 3);
############################################################################
#Allocating space for vector of probabilities
probabilities = vector(mode = "numeric", length = nrow(df_a_big))
probabilities[df_a_big[,"V3"] == 1] = prob_1
probabilities[df_a_big[,"V3"] == 2] = prob_2
probabilities[df_a_big[,"V3"] == 3] = prob_3
##############################################################################
#Splitting data into training and testing sets
set.seed(777)
sub = sample(nrow(df_a_big), floor(nrow(df_a_big) * 0.01), prob = probabilities, replace = F)
subset <- df_a_big[sub, ]
##############################################################################
#Visializing proportions for training
sum(subset[,"V3"] == 1)
sum(subset[,"V3"] == 2)
sum(subset[,"V3"] == 3)
#Visualizing data set
plot(subset[,1:2], col = subset$V3,
     xlab = "Feature 1", ylab = "Feature 2",
     main = "subset_a_big.csv")

#Selecting random centroids

centroids = matrix(0L, nrow = 3, ncol = 2)
{
  for (i in 1:nrow(centroids))
  {
    centroids[i,1] = runif(1, min(subset$V1), max(subset$V1))
    centroids[i,2] = runif(1, min(subset$V2), max(subset$V2))
  }
}

#Calculing the distance between points and centroids
distance_matrix = matrix(0L, nrow = nrow(subset), ncol = nrow(centroids))

for (i in 1:nrow(centroids))
{
  distance_matrix[,i] = euc.dist(centroids[i,1], centroids[i,2], subset[,1], subset[,2])
}


which.min(distance_matrix[2998,])


distance_matrix

