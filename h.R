###################
#Reading h.csv#####
###################
df_h = read.csv("h.csv",header = F)
df_h$V4 = floor(df_h$V4) - min(floor(df_h$V4)) +1
plot(df_h, col= get_colors(11, df_h))
plot3d(df_h$V1, df_h$V2, df_h$V3, col=get_colors(11, df_h),
       type="s", xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
       main= "Data Set h.csv")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_h = eval_kmeans_3D(df = df_h, cstart = 1, cfinish = 3, k = 11, dataname = "h.csv")