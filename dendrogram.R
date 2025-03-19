
# Classfication, dendrogram and heatmap

# Hierarchical Clustering -> hclust

# Load data
data(USArrests)
# Compute distances and hierarchical clustering
dd <- dist(scale(USArrests), method = "euclidean")
set.seed(100)
hc <- hclust(dd)
## the abs(id) in hc$merge indicates the rowID of USArrests

hc
# Call:
#   hclust(d = dd)
# 
# Cluster method   : complete 
# Distance         : euclidean 
# Number of objects: 50 


# plot.hclust()
plot(hc)
# Error in plot.new() : figure margins too large

#reset graphical parameters to default:
par(mar = c(5, 4, 4, 2) + 0.1)  # Default margins
plot(hc)
# Error in plot.new() : figure margins too large

#reduce graphical parameters manually:
par(mar = c(3, 3, 2, 1))  # Smaller margins
plot(hc)


# Put the labels at the same height: hang = -1
plot(hc, hang = -1, cex = 0.6)


# plot.dendrogram() function

# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(hc)

# Default plot
plot(hcd, type = "rectangle", ylab = "Height")

# Triangle plot
plot(hcd, type = "triangle", ylab = "Height")

# Zoom in to the first dendrogram
plot(hcd, xlim = c(1, 20), ylim = c(1,8))


# Define nodePar
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")


# Horizontal plot
# change the edge of the plot
par(mar = c(1,1,1,6))
plot(hcd,  xlab = "Height",
     nodePar = nodePar, horiz = TRUE)

