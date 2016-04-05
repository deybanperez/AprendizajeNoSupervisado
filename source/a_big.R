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
prob_1 = 1/sum(df_a_big[, "V3"] == 1);
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
best = 0

sub = sample(nrow(df_a_big), floor(nrow(df_a_big) * 0.01),
             prob = probabilities, replace = F)

subset <- df_a_big[sub, ]
for (i in 1:50)
{
  model_temporal = K_Means_Deyban(subset, k = 3)
  table_model_kmeans_deyban_a_big = order_confusion_matrix(confusion_matrix(subset$V3, model_temporal[[2]]))
  temporal = confusion_matrix_evaluation_deyban(table_model_kmeans_deyban_a_big, nrow(subset))
  
  if(temporal > best)
  {
    best_centroids = model_temporal[[1]]
    best_klusters = model_temporal[[2]]
    best = temporal
  }
}

best
table_model = order_confusion_matrix(confusion_matrix(subset$V3, best_klusters))

confusion_matrix_evaluation(table_model, nrow(subset))

plot(subset[, 1:2], col = best_klusters,
     main = paste(c("Data set", "Deyban"), collapse = " "), sub = "K-means algorithm",
     xlab = "Feature 1", ylab = "Feature 2")
points(best_centroids[,1:2],
       col = 6:8,
       pch = 19,
       cex = 2)

best_centroids

model_big = K_Means_Deyban(df_a_big, 3, best_centroids)
table_model_kmeans_model_big = order_confusion_matrix(confusion_matrix(df_a_big$V3, model_big[[2]]))
rate_big = confusion_matrix_evaluation_deyban(table_model_kmeans_model_big, nrow(df_a_big))
rate_big

plot(df_a_big[, 1:2], col = model_big[[2]],
     main = paste(c("Data set", "Deyban"), collapse = " "), sub = "K-means algorithm",
     xlab = "Feature 1", ylab = "Feature 2")

points(model_big[[1]][,1:2],
       col = 6:8,
       pch = 19,
       cex = 2)
