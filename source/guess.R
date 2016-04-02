###################
#Reading guess.csv#
###################
df_guess = read.csv("data/guess.csv",header = F)
valor = rep(0,30)
for (k in 1:30)
{
  model_kmeans_guess_jambu = kmeans(df_guess, k)
  valor[k] = model_kmeans_guess_jambu$tot.withinss
}
plot(valor, col = "blue", type = "b",
     main = "Codo de Jambu")
#Aplying algorithms
#########
#K-Means#
#########
model_kmeans_guess = eval_kmeans(df = df_guess, cstart = 1, cfinish = 2, k = 5, dataname = "guess.csv")
#########
#H-Clust#
#########
input_hierarchical_guess = df_guess
input_hierarchical_guess$V3 = NULL
input_hierarchical_guess = as.matrix(input_hierarchical_guess)
hierarchical_distance_guess = dist(input_hierarchical_guess)
###############
#Single method#
###############
model_hierarchical_single_guess = eval_hclust(distance = hierarchical_distance_guess, mode = "single", centroids = 5, input = input_hierarchical_guess, dataname = "guess.csv")
#################
#Complete method#
#################
model_hierarchical_complete_guess = eval_hclust(distance = hierarchical_distance_guess, mode = "complete", centroids = 5, input = input_hierarchical_guess, dataname = "guess.csv")
#################
#Average method##
#################
model_hierarchical_average_guess = eval_hclust(distance = hierarchical_distance_guess, mode = "average", centroids = 5, input = input_hierarchical_guess, dataname = "guess.csv")
