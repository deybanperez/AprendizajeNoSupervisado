###################
#Reading h.csv#####
###################
df_h = read.csv("h.csv",header = F)
df_h$V5 = floor(df_h$V4) - min(floor(df_h$V4)) +1
plot(df_h[1:3], col= get_colors(11, df_h),
     main = "Matriz Dispersion h.csv")
plot3d(df_h$V1, df_h$V2, df_h$V3, col=get_colors(11, df_h),
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set h.csv")
df_h$V5 = NULL
df_h$class = 7*(df_h$V4-min(df_h$V4))/(max(df_h$V4)-min(df_h$V4)) + 1
df_h$class = floor(df_h$class)
df_h$class[df_h$class == 8] = 7
plot3d(df_h$V1, df_h$V2, df_h$V3, col=df_h$class,
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set h.csv")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_h = eval_kmeans_3D(df = df_h, cstart = 1, cfinish = 3, k = 7, dataname = "h.csv")
table_model_kmeans_h = order_confusion_matrix(confusion_matrix(df_h$class, model_kmeans_h$cluster))
table_model_kmeans_h
confusion_matrix_evaluation(table_model_kmeans_h, nrow(df_h))
############
#H-Clusters#
############
input_hierarchical_h = df_h
input_hierarchical_h$V4 = NULL
input_hierarchical_h$class = NULL
input_hierarchical_h = as.matrix(input_hierarchical_h)
hierarchical_distance_h = dist(input_hierarchical_h)
###############
#Single method#
###############
model_hierarchical_single_h = eval_hclust_3D(distance = hierarchical_distance_h, mode = "single", centroids = 7, input = input_hierarchical_h, dataname = "h.csv")
table_model_hierarchical_single_h = order_confusion_matrix(confusion_matrix(df_h$class, model_hierarchical_single_h))
table_model_hierarchical_single_h
confusion_matrix_evaluation(table_model_hierarchical_single_h, nrow(df_h))
#################
#Complete method#
#################
model_hierarchical_complete_h = eval_hclust_3D(distance = hierarchical_distance_h, mode = "complete", centroids = 7, input = input_hierarchical_h, dataname = "h.csv")
table_model_hierarchical_complete_h = order_confusion_matrix(confusion_matrix(df_h$class, model_hierarchical_complete_h))
table_model_hierarchical_complete_h
confusion_matrix_evaluation(table_model_hierarchical_complete_h, nrow(df_h))
#################
#Average method#
#################
model_hierarchical_average_h = eval_hclust_3D(distance = hierarchical_distance_h, mode = "average", centroids = 7, input = input_hierarchical_h, dataname = "h.csv")
table_model_hierarchical_average_h = order_confusion_matrix(confusion_matrix(df_h$class, model_hierarchical_average_h))
table_model_hierarchical_average_h
confusion_matrix_evaluation(table_model_hierarchical_average_h, nrow(df_h))
