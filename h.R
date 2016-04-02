###################
#Reading h.csv#####
###################
df_h = read.csv("h.csv",header = F)
df_h$V5 = floor(df_h$V4) - min(floor(df_h$V4)) +1
plot(df_h, col= get_colors(11, df_h))
plot3d(df_h$V1, df_h$V2, df_h$V3, col=get_colors(11, df_h),
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set h.csv")
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