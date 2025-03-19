
#distance matrix

# Load data
data(USArrests)
#normalize the data by column
df <- scale(USArrests)

install.packages("factoextra") #this took a long time
library(factoextra)

#have to run this to close the current graphics device
#combining base plotting functions with ggplot2 causes errors
dev.off()

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



#computing k-means clustering in R

# We can compute k-means in R with the kmeans function. 
# Here will group the data into two clusters (centers = 2). 
# The kmeans function also has an nstart option that attempts multiple 
# initial configurations and reports on the best one. 
# For example, adding nstart = 25 will generate 25 initial configurations. 
# This approach is often recommended.

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

#The output of kmeans is a list with several bits of information. The most important being:
# cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# centers: A matrix of cluster centers.
# totss: The total sum of squares.
# withinss: Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.

#We can also view our results by using fviz_cluster
#This provides a nice illustration of the clusters. 
# If there are more than two dimensions (variables), fviz_cluster will 
# perform principal component analysis (PCA) and plot the data points 
# according to the first two principal components that explain 
# the majority of the variance.

fviz_cluster(k2, data = df)


# We can also execute the same process for 3, 4, and 5 clusters

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)





#determining optimal clusters

# The total within-cluster sum of square (wss) measures the compactness 
# of the clustering and we want it to be as small as possible. 
# Thus, we can use the following algorithm to define the optimal clusters:

# 1.Compute clustering algorithm (e.g., k-means clustering) for different values of k. 
#     For instance, by varying k from 1 to 10 clusters
# 2. For each k, calculate the total within-cluster sum of square (wss)
# 3. Plot the curve of wss according to the number of clusters k.
# 4. The location of a bend (knee) in the plot is generally considered 
#     as an indicator of the appropriate number of clusters.
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- sapply(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#Fortunately, this process to compute the “Elbow method” 
# has been wrapped up in a single function (fviz_nbclust):
set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")



