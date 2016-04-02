###################
#Reading moon.csv#
###################
df_moon = read.csv("data/moon.csv",header = F)
#Converting class column to a factor type
df_moon = pre_processing(df_moon)
#Visualizing data set
plot(df_moon[,1:2], col = df_moon$V3,
     xlab = "Feature 1", ylab = "Feature 2",
     main = "moon.csv")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_moon = eval_kmeans(df = df_moon, cstart = 1, cfinish = 2, k = 2, dataname = "moon.csv")
table_model_kmeans_moon = order_confusion_matrix(confusion_matrix(df_moon$V3, model_kmeans_moon$cluster))
table_model_kmeans_moon
confusion_matrix_evaluation(table_model_kmeans_moon, nrow(df_moon))
############
#H-Clust#
############
input_hierarchical_moon = df_moon
input_hierarchical_moon$V3 = NULL
input_hierarchical_moon = as.matrix(input_hierarchical_moon)
hierarchical_distance_moon = dist(input_hierarchical_moon)
###############
#Single method#
###############
model_hierarchical_single_moon = eval_hclust(distance = hierarchical_distance_moon, mode = "single", centroids = 2, input = input_hierarchical_moon, dataname = "moon.csv")
table_model_hierarchical_single_moon = order_confusion_matrix(confusion_matrix(df_moon$V3, model_hierarchical_single_moon))
table_model_hierarchical_single_moon
confusion_matrix_evaluation(table_model_hierarchical_single_moon, nrow(df_moon))
###############
#Complete method#
###############
model_hierarchical_complete_moon = eval_hclust(distance = hierarchical_distance_moon, mode = "complete", centroids = 2, input = input_hierarchical_moon, dataname = "moon.csv")
table_model_hierarchical_complete_moon = order_confusion_matrix(confusion_matrix(df_moon$V3, model_hierarchical_complete_moon))
table_model_hierarchical_complete_moon
confusion_matrix_evaluation(table_model_hierarchical_complete_moon, nrow(df_moon))
###############
#Average method#
###############
model_hierarchical_average_moon = eval_hclust(distance = hierarchical_distance_moon, mode = "average", centroids = 2, input = input_hierarchical_moon, dataname = "moon.csv")
table_model_hierarchical_average_moon = order_confusion_matrix(confusion_matrix(df_moon$V3, model_hierarchical_average_moon))
table_model_hierarchical_average_moon
confusion_matrix_evaluation(table_model_hierarchical_average_moon, nrow(df_moon))