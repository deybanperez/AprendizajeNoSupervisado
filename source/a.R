###############
#Reading a.csv#
###############
df_a = read.csv("data/a.csv",header = F)
#Converting class column to a factor type
df_a = pre_processing(df_a)
#Visualizing data set
plot(df_a[,1:2], col = df_a$V3,
     xlab = "Feature 1", ylab = "Feature 2",
     main = "a.csv")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_a = eval_kmeans(df = df_a, cstart = 1, cfinish = 2, k = 3, dataname = "a.csv")
table_model_kmeans_a = order_confusion_matrix(confusion_matrix(df_a$V3, model_kmeans_a$cluster))
table_model_kmeans_a
confusion_matrix_evaluation(table_model_kmeans_a, nrow(df_a))
#########
#H-Clust#
#########
input_hierarchical_a = df_a
input_hierarchical_a$V3 = NULL
input_hierarchical_a = as.matrix(input_hierarchical_a)
hierarchical_distance_a = dist(input_hierarchical_a)
###############
#Single method#
###############
model_hierarchical_single_a = eval_hclust(distance = hierarchical_distance_a, mode = "single", centroids = 3, input = input_hierarchical_a, dataname = "a.csv")
table_model_hierarchical_single_a = order_confusion_matrix(confusion_matrix(df_a$V3, model_hierarchical_single_a))
table_model_hierarchical_single_a
confusion_matrix_evaluation(table_model_hierarchical_single_a, nrow(df_a))
###############
#Complete method#
###############
model_hierarchical_complete_a = eval_hclust(distance = hierarchical_distance_a, mode = "complete", centroids = 3, input = input_hierarchical_a, dataname = "a.csv")
table_model_hierarchical_complete_a = order_confusion_matrix(confusion_matrix(df_a$V3, model_hierarchical_complete_a))
table_model_hierarchical_complete_a
confusion_matrix_evaluation(table_model_hierarchical_complete_a, nrow(df_a))
###############
#Complete method#
###############
model_hierarchical_average_a = eval_hclust(distance = hierarchical_distance_a, mode = "average", centroids = 3, input = input_hierarchical_a, dataname = "a.csv")
table_model_hierarchical_average_a = order_confusion_matrix(confusion_matrix(df_a$V3, model_hierarchical_average_a))
table_model_hierarchical_average_a
confusion_matrix_evaluation(table_model_hierarchical_average_a, nrow(df_a))