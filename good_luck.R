###################
#Reading good_lock.csv#
###################
df_good_luck = read.csv("good_luck.csv",header = F)
#Converting class column to a factor type
df_good_luck = pre_processing_especial(df_good_luck)
#Visualizing data set
plot(df_good_luck[,1:10], col = df_good_luck$V11,
     main = "Matriz de Dispersion good_luck.csv")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_good_luck = eval_kmeans_especial(df = df_good_luck, cstart = 1, cfinish = 10, k = 2, dataname = "good_luck.csv")
table_model_kmeans_good_luck = order_confusion_matrix(confusion_matrix(df_good_luck$V11, model_kmeans_good_luck$cluster))
table_model_kmeans_good_luck
confusion_matrix_evaluation(table_model_kmeans_good_luck, nrow(df_good_luck))
############
#H-Clust#
############
input_hierarchical_good_luck = df_good_luck
input_hierarchical_good_luck$V11 = NULL
input_hierarchical_good_luck = as.matrix(input_hierarchical_good_luck)
hierarchical_distance_good_luck = dist(input_hierarchical_good_luck)
###############
#Single method#
###############
model_hierarchical_single_good_luck = eval_hclust_especial(distance = hierarchical_distance_good_luck, mode = "single", centroids = 2, input = input_hierarchical_good_luck, dataname = "good_luck.csv", cfinish = 10)
table_model_hierarchical_single_good_luck = order_confusion_matrix(confusion_matrix(df_good_luck$V11, model_hierarchical_single_good_luck))
table_model_hierarchical_single_good_luck
confusion_matrix_evaluation(table_model_hierarchical_single_good_luck, nrow(df_good_luck))
#################
#Complete method#
#################
model_hierarchical_complete_good_luck = eval_hclust_especial(distance = hierarchical_distance_good_luck, mode = "complete", centroids = 2, input = input_hierarchical_good_luck, dataname = "good_luck.csv", cfinish = 10)
table_model_hierarchical_complete_good_luck = order_confusion_matrix(confusion_matrix(df_good_luck$V11, model_hierarchical_complete_good_luck))
table_model_hierarchical_complete_good_luck
confusion_matrix_evaluation(table_model_hierarchical_complete_good_luck, nrow(df_good_luck))
#################
#Average method#
#################
model_hierarchical_average_good_luck = eval_hclust_especial(distance = hierarchical_distance_good_luck, mode = "average", centroids = 2, input = input_hierarchical_good_luck, dataname = "good_luck.csv", cfinish = 10)
table_model_hierarchical_average_good_luck = order_confusion_matrix(confusion_matrix(df_good_luck$V11, model_hierarchical_average_good_luck))
table_model_hierarchical_average_good_luck
confusion_matrix_evaluation(table_model_hierarchical_average_good_luck, nrow(df_good_luck))
