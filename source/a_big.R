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
