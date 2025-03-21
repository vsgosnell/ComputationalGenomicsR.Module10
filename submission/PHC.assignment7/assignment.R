
#remove all variables from environment
rm(list = ls())

#reset plot margins -> so 4 plots don't display at same time
par(mfrow=c(1,1))

#set wd 
setwd('/Users/veronicagosnell/Desktop/R/ComputationalGenomicsR.Module10')

#load the data
df <- read.table('vehicle_drug_feature_counts.txt',
                 header = T, sep = '\t', row.names = 1)


# 1) Please calculate FPKM for each column in the reads counts (5 points)

#data normalization -> FPKM -> fragments per kilobase of transcript per million mapped reads
dfReads <- df[,6:9] #selects col 6-9 from df & stores in dfReads
fpkm    <- apply(dfReads, 2, #applies function to dfReads column-wise aka 2
                 function(x){x/df$Length *10^9/sum(x)}) #x=one column of read counts
                    #FPKM=raw counts/gene length x total reads
                            #x/df$Length= normalize by gene length -> col Length has gene lengths
                                        #10^9/sum(x)=scale by total read count -> to get reads per billion(?) mapped reads
#result is FPKM matrix where each col represents a sample

# Convert to a data frame
fpkm <- as.data.frame(fpkm)

# Rename columns to match original sample names
colnames(fpkm) <- colnames(df)[6:9]

# Set row names to match genes
rownames(fpkm) <- rownames(df)

# Print FPKM values
print(fpkm)




# 2) Generating the scatter plot for each pair of biological replicates. 
# You should provide two plots, one is vehicle_rep1 vs. vehicle_rep2, 
# another one is drug_rep1 vs. drug_rep2. 
# The X and Y axis should be the log2(FPKM) value of each bio-rep. 
# The plot should provide the meaning of X and Y labels (20 points).



# Load required package
library(ggplot2)

# Convert FPKM values to log2 scale (adding 1 to avoid log(0))
log2_fpkm <- log2(fpkm + 1)
# Convert FPKM to log2 Scale: Since gene expression data varies over a large range, log transformation stabilizes variance.

# Create a data frame for plotting
fpkm_plot <- data.frame(
  vehicle_rep1 = log2_fpkm[, "vehicle_rep1.bam"],
  vehicle_rep2 = log2_fpkm[, "vehicle_rep2.bam"],
  drug_rep1 = log2_fpkm[, "drug_rep1.bam"],
  drug_rep2 = log2_fpkm[, "drug_rep2.bam"]
)
# Create a Data Frame: Organizes FPKM values for the vehicle and drug replicates.

# Error in `[.data.frame`(log2_fpkm, , "vehicle_rep1") : undefined columns selected

#check fpkm df
print(fpkm)
#use vehicle_rep1.bam, vehicle_rep2.bam, drug_rep1.bam, drug_rep2.bam as column names




# Scatter plot for Vehicle (Control) Replicates
ggplot(fpkm_plot, aes(x = vehicle_rep1, y = vehicle_rep2)) +
  geom_point(color = "blue", alpha = 0.5) + #plots data points
  geom_smooth(method = "lm", se = FALSE, color = "red") + #adds linear regression line
  labs(x = "log2(FPKM) of Control Replicate 1", #provides meaningful axis labels
       y = "log2(FPKM) of Control Replicate 2",
       title = "Scatter Plot of Control Replicates") +
  theme_minimal() #provides minimal theme = clean layout

#Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state

#reset graphics device
dev.off()



# Scatter plot for Drug (Treatment) Replicates
ggplot(fpkm_plot, aes(x = drug_rep1, y = drug_rep2)) +
  geom_point(color = "purple", alpha = 0.5) + #plots data points
  geom_smooth(method = "lm", se = FALSE, color = "red") + #adds linear regression line
  labs(x = "log2(FPKM) of Treatment Replicate 1", #provides meaningful axis labels
       y = "log2(FPKM) of Treatment Replicate 2",
       title = "Scatter Plot of Treatment Replicates") +
  theme_minimal() #provides minimal theme = clean layout






# 3) Generating the boxplots according to FPKM value of each assay. 
# You should consider putting four boxplots in one figure panel. 
# The beginning two boxplots are the two bio-reps from ‘vehicle_rep’, 
# while the two following ones are ‘drug_rep’. 
# The Y-axis of the boxplot should be log2(FPKM). 
# The X-axis should be two ‘veicale_rep’ and two ‘drug_rep’. 
# Please consider giving different colors to distinguish the two assays 
# (vehicle_rep and drug_rep) (25 points).



# Create a data frame for plotting
fpkm_plot <- data.frame(
  Sample = rep(c("vehicle_rep1", "vehicle_rep2", "drug_rep1", "drug_rep2"),
               each = nrow(log2_fpkm)),
  log2_FPKM = c(log2_fpkm[, "vehicle_rep1.bam"],
                log2_fpkm[, "vehicle_rep2.bam"],
                log2_fpkm[, "drug_rep1.bam"],
                log2_fpkm[, "drug_rep2.bam"]),
  Assay = rep(c("Control", "Control", "Treatment", "Treatment"),
              each = nrow(log2_fpkm))
)


# Boxplot for FPKM values
ggplot(fpkm_plot, aes(x = Sample, y = log2_FPKM, fill = Assay)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Control" = "blue", "Treatment" = "red")) +
  labs(x = "Sample", y = "log2(FPKM)", title = "Boxplots of FPKM values") +
  theme_minimal()



#update boxplot to change individual x axis labels of groups
ggplot(fpkm_plot, aes(x = Sample, y = log2_FPKM, fill = Assay)) +
  geom_boxplot() +
  labs(x = "Sample Groups", y = "log2(FPKM)", title = "Boxplot of FPKM Values") +
  scale_x_discrete(labels = c("vehicle_rep1" = "Control Rep 1", 
                              "vehicle_rep2" = "Control Rep 2", 
                              "drug_rep1" = "Treatment Rep 1", 
                              "drug_rep2" = "Treatment Rep 2")) +
  theme_minimal()






# 4) Similar to question C, but generating a histogram for each assay according to FPKM value. 
# The four histograms should have the same lower and the upper limit on the x-axis and y-axis. 
# Each histogram should clearly annotate the assay's name and have the same number 
# of the breakpoints between histogram cells (hint: using breaks in traditional R plot) (25 points).



# Histograms for each assay
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2x2 layout
breaks <- 50  # Set number of breaks for consistency
xlim_range <- range(log2_fpkm, na.rm = TRUE)
ylim_range <- c(0, max(hist(log2_fpkm[, "vehicle_rep1.bam"], breaks = breaks, plot = FALSE)$counts,
                       hist(log2_fpkm[, "vehicle_rep2.bam"], breaks = breaks, plot = FALSE)$counts,
                       hist(log2_fpkm[, "drug_rep1.bam"], breaks = breaks, plot = FALSE)$counts,
                       hist(log2_fpkm[, "drug_rep2.bam"], breaks = breaks, plot = FALSE)$counts))

hist(log2_fpkm[, "vehicle_rep1.bam"], breaks = breaks, col = "blue", main = "Histogram of Control Rep 1",
     xlab = "log2(FPKM)", xlim = xlim_range, ylim = ylim_range)

hist(log2_fpkm[, "vehicle_rep2.bam"], breaks = breaks, col = "blue", main = "Histogram of Control Rep 2",
     xlab = "log2(FPKM)", xlim = xlim_range, ylim = ylim_range)

hist(log2_fpkm[, "drug_rep1.bam"], breaks = breaks, col = "purple", main = "Histogram of Treatment Rep 1",
     xlab = "log2(FPKM)", xlim = xlim_range, ylim = ylim_range)

hist(log2_fpkm[, "drug_rep2.bam"], breaks = breaks, col = "purple", main = "Histogram of Treatment Rep 2",
     xlab = "log2(FPKM)", xlim = xlim_range, ylim = ylim_range)






# 5) Generating a heatmap to compare the correlation between each assay. 
# The heatmap should contain all Pearson correlation between each pair of the assays. 
# The meaning of column and row should be list on the plot and the color legend need to be provided (25 points).


# produce a heatmap where the color intensity represents the correlation values between assays
install.packages("pheatmap")
library(pheatmap)

# Compute Pearson correlation matrix
cor_matrix <- cor(log2_fpkm, method = "pearson")

# Define color gradient
color_palette <- colorRampPalette(c("blue", "white", "red"))(100)

# Generate heatmap
pheatmap(cor_matrix,
         color = color_palette,
         clustering_method = "complete",  # Hierarchical clustering
         display_numbers = TRUE,  # Show correlation values
         main = "Pearson Correlation Heatmap",
         fontsize = 12,
         labels_row = c("Control 1", "Control 2", "Treatment 1", "Treatment 2"),
         labels_col = c("Control 1", "Control 2", "Treatment 1", "Treatment 2")
)

