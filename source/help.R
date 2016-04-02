df_help = read.csv("data/help.csv",header = F)
df_help$V5 = floor(df_help$V4) - min(floor(df_help$V4)) +1
plot(df_help[,1:3], col= get_colors(20, df_help),
     main = "Matriz Dispersion help.csv")
plot3d(df_help$V1, df_help$V2, df_help$V3, col=get_colors(11, df_help),
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set help.csv")
df_help$V5 = NULL
df_help$class = 2
df_help$class[df_help$V1 < 10] = 1
df_help$class[df_help$V1 > 42] = 3
plot3d(df_help$V1, df_help$V2, df_help$V3, col=df_help$class,
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set help.csv")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_help = eval_kmeans_3D(df = df_help, cstart = 1, cfinish = 3, k = 3, dataname = "help.csv")
table_model_kmeans_help = order_confusion_matrix(confusion_matrix(df_help$class, model_kmeans_help$cluster))
table_model_kmeans_help
confusion_matrix_evaluation(table_model_kmeans_help, nrow(df_help))
############
#H-Clusters#
############
input_hierarchical_help = df_help
input_hierarchical_help$V4 = NULL
input_hierarchical_help$class = NULL
input_hierarchical_help = as.matrix(input_hierarchical_help)
hierarchical_distance_help = dist(input_hierarchical_help)
###############
#Single method#
###############
model_hierarchical_single_help = eval_hclust_3D(distance = hierarchical_distance_help, mode = "single", centroids = 3, input = input_hierarchical_help, dataname = "help.csv")
table_model_hierarchical_single_help = order_confusion_matrix(confusion_matrix(df_help$class, model_hierarchical_single_help))
table_model_hierarchical_single_help
confusion_matrix_evaluation(table_model_hierarchical_single_help, nrow(df_help))
###############
#Complete method#
###############
model_hierarchical_complete_help = eval_hclust_3D(distance = hierarchical_distance_help, mode = "complete", centroids = 3, input = input_hierarchical_help, dataname = "help.csv")
table_model_hierarchical_complete_help = order_confusion_matrix(confusion_matrix(df_help$class, model_hierarchical_complete_help))
table_model_hierarchical_complete_help
confusion_matrix_evaluation(table_model_hierarchical_complete_help, nrow(df_help))
###############
#Average method#
###############
model_hierarchical_average_help = eval_hclust_3D(distance = hierarchical_distance_help, mode = "average", centroids = 3, input = input_hierarchical_help, dataname = "help.csv")
table_model_hierarchical_average_help = order_confusion_matrix(confusion_matrix(df_help$class, model_hierarchical_average_help))
table_model_hierarchical_average_help
confusion_matrix_evaluation(table_model_hierarchical_average_help, nrow(df_help))