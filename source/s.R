###################
#Reading s.csv#
###################
df_s = read.csv("data/s.csv",header = F)
df_s$V5 = floor(df_s$V4) - min(floor(df_s$V4)) +1
plot(df_s[,1:3], col= get_colors(11, df_s),
     main = "Matriz Dispersion s.csv")
plot3d(df_s$V1, df_s$V2, df_s$V3, col=get_colors(11, df_s),
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set s.csv")
df_s$V5 = NULL
df_s$class = 7*(df_s$V4-min(df_s$V4))/(max(df_s$V4)-min(df_s$V4)) + 1
df_s$class = floor(df_s$class)
df_s$class[df_s$class == 8] = 7
plot3d(df_s$V1, df_s$V2, df_s$V3, col=df_s$class,
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set s.csv")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_s = eval_kmeans_3D(df = df_s, cstart = 1, cfinish = 3, k = 7, dataname = "s.csv")
table_model_kmeans_s = order_confusion_matrix(confusion_matrix(df_s$class, model_kmeans_s$cluster))
table_model_kmeans_s
confusion_matrix_evaluation(table_model_kmeans_h, nrow(df_h))
############
#H-Clusters#
############
input_hierarchical_s = df_s
input_hierarchical_s$V4 = NULL
input_hierarchical_s$class = NULL
input_hierarchical_s = as.matrix(input_hierarchical_s)
hierarchical_distance_s = dist(input_hierarchical_s)
###############
#Single method#
###############
model_hierarchical_single_s = eval_hclust_3D(distance = hierarchical_distance_s, mode = "single", centroids = 7, input = input_hierarchical_s, dataname = "s.csv")
table_model_hierarchical_single_s = order_confusion_matrix(confusion_matrix(df_s$class, model_hierarchical_single_s))
table_model_hierarchical_single_s
confusion_matrix_evaluation(table_model_hierarchical_single_s, nrow(df_s))
#################
#Complete method#
#################
model_hierarchical_complete_s = eval_hclust_3D(distance = hierarchical_distance_s, mode = "complete", centroids = 7, input = input_hierarchical_s, dataname = "s.csv")
table_model_hierarchical_complete_s = order_confusion_matrix(confusion_matrix(df_s$class, model_hierarchical_complete_s))
table_model_hierarchical_complete_s
confusion_matrix_evaluation(table_model_hierarchical_complete_s, nrow(df_s))
#################
#Average method#
#################
model_hierarchical_average_s = eval_hclust_3D(distance = hierarchical_distance_s, mode = "average", centroids = 7, input = input_hierarchical_s, dataname = "s.csv")
table_model_hierarchical_average_s = order_confusion_matrix(confusion_matrix(df_s$class, model_hierarchical_average_s))
table_model_hierarchical_average_s
confusion_matrix_evaluation(table_model_hierarchical_average_s, nrow(df_s))