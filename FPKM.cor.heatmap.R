
#remove all variables from environment
rm(list = ls())

#reset plot margins -> so 4 plots don't display at same time
par(mfrow=c(1,1))

#set wd 
setwd('/Users/veronicagosnell/Desktop/R/ComputationalGenomicsR.Module10')

#load previous saved FPKM data
fpkm <- read.table('vehicle_drug_feature_counts.fpkm.txt',
                   header = T, sep = '\t', row.names = 1)

#check first few rows to ensure table loaded correctly
head(fpkm)


# This code calculates the biological correlation matrix for FPKM values 
# and visualizes it using heatmaps, leveraging both base R, gplots (heatmap.2), and ggplot2

#biological correlation calculation and heatmap
corF <- cor(fpkm) #Computes the Pearson correlation matrix for FPKM values, showing the similarity between samples.
corF

# Basic Heatmap using Base R
heatmap(corF) #generates basic heatmap of the correlation matrix
              #uses default hierarchial clustering


# Enhanced Heatmap using heatmap.2 (from gplots)
library(gplots)


heatmap.2(corF, Colv= F, dendrogram = 'row', #Colv=F: disables column clustering, keeps only row dendrogram
                                              #dendrogram='row': displays row-wise hierarchial clustering
          trace = 'n',      # removes histogram traces inside the heatmap
          denscol = 'navy'  # changes color of density plot in the color key
)


##change the direction of column lable
heatmap.2(corF, Colv= F, dendrogram = 'row',
          trace = 'n', 
          denscol = 'navy', srtCol = 0 #sets col label text angle to horizontal
)


## change the color of heatmap
colPlate <- colorRampPalette(c('red', 'white', 'navy'))(100) #colorRampPallette: defines custom color gradient w/ 100 color levels
heatmap.2(corF, Colv= F, dendrogram = 'row',
          trace = 'n',  
          denscol = 'navy', srtCol = 0, ## the angle of y column label
          col = colPlate #applies this color scheme to the heatmap
)


## removing the lable in the heatmap
## change the size of the key
heatmap.2(corF, Colv= F, dendrogram = 'row',
          trace = 'n',  
          denscol = NA, srtCol = 0, #denscol=NA: removes color density line from key 
                                    #srtCol=0: horizontal y column label = 0 degrees
          col = colPlate, 
          key.title = '', #removes key title
          key.ylab = NA, key.xlab = NA,
          keysize=0.75, #shrinks key size
          #remove the y-axis
          #key.par = list(mar = c(1,0.3,1,1))
)



#drawing heatmap by ggplot2
corF <- cor(fpkm)
corF

##load package
library(reshape2)
melted_corF <- melt(corF) #melt(CorF): converts correlation matrix to long format (3 cols: Var1, Var2, value) 
                          #necessary for ggplot2
length(corF) # Check the number of elements

nrow(melted_corF) # Check the number of rows

#heatmap drawing
library(ggplot2)
ggplot(data = melted_corF, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()  #creates heatmap using colored tiles


# Creating an Upper Triangular Correlation Heatmap

# Get lower triangular of the correlation matrix
get_upper_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA #lower.tri(cormat): identifies lower triangular matrix elements.
                                  #cormat[lower.tri(cormat)] <- NA: sets lower triangle to NA (hides it)
  return(cormat)
}

# Only keep lower triangular
# Converts only the upper triangle of the correlation matrix into a long format.
upper_tri        <- get_upper_tri(corF) 
melted_upper_tri <- melt( upper_tri )


ggplot(data = melted_upper_tri, aes(Var2, Var1, fill = value) +
  geom_tile(color = "white") + #creates heatmap tiles with white borders
  scale_fill_gradient2(low = "navy", high = "red", mid = "white", #Defines a diverging color scale from navy (low correlation) → white (neutral) → red (high correlation)
                       midpoint = 0.9, limit = c(0.8,1), #Adjusts color scaling to highlight strong correlations.
                       #space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + #uses minimalistic theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, #rotates x axis for better readability
                                   size = 12, hjust = 1)) +
  coord_fixed() #Fixes the aspect ratio, ensuring square tiles



  # ✅ Computes correlation matrix (corF) to assess similarity between samples.
  # ✅ Visualizes correlation using heatmaps (Base R, heatmap.2, ggplot2).
  # ✅ Customizes heatmap colors, labels, and removes lower triangular part.
  # ✅ Uses ggplot2 for an advanced heatmap with gradient colors.


  
  
  