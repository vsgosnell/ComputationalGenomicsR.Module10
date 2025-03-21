
#remove all variables from environment
rm(list = ls())

#reset plot margins -> so 4 plots don't display at same time
par(mfrow=c(1,1))

#set wd and load the data
setwd('/Users/veronicagosnell/Desktop/R/ComputationalGenomicsR.Module10')
df <- read.table('vehicle_drug_feature_counts.txt',
                 header = T, sep = '\t', row.names = 1)


#data normalization -> FPKM -> fragments per kilobase million
dfReads <- df[,6:9] #selects col 6-9 from df & stores in dfReads
fpkm    <- apply(dfReads, 2, #applies function to dfReads column-wise aka 2
                 function(x){x/df$Length *10^9/sum(x)}) #x=one column of read counts
                      #FPKM=raw counts/gene length x total reads
                            #x/df$Length= normalize by gene length -> col Length has gene lengths
                            #10^9/sum(x)=scale by total read count -> to get reads per billion mapped reads
                            #result is FPKM matrix where each col represents a sample

##change the col name
colnames(fpkm) <- c('c1','c2','t1','t2')

##save the data
write.table(fpkm, file = 'vehicle_drug_feature_counts.fpkm.txt', #saves fpkm to a tab-separated file
            row.names = T, col.names = T, sep = '\t', #include row and col names; use tab separator (TSV format)
            quote = F) #do not enclose text in quotes



# compare the expression by scatter plot (biological replicates)

plot(fpkm[,1],fpkm[,2]) #plots raw FPKM values from 1st and 2nd cols
                        #Each point represents a gene's expression level in two biological replicates.

## change to log2
plot(log2(fpkm[,1]), log2(fpkm[,2])) #converts fpkm values to log2 scale
                                      #This transformation helps normalize the data and reduces the impact of extreme values.

## change the points color 
plot(log2(fpkm[,1]), log2(fpkm[,2]),
     col = 'red', pch = 19) #pch=19 uses solid circles as points

plot(log2(fpkm[,1]), log2(fpkm[,2]),
     bg = 'red', pch = 21) #bg=red: sets background color of points to red
                            #pch=21 uses hollow circles -> borders can have different color

## change the label
plot(log2(fpkm[,1]), log2(fpkm[,2]), 
     bg = 'red', pch = 21,
     xlab = 'log2(control1_FPKM)', #labels x axis
     ylab = 'log2(control2_FPKM)') #labels y axis

## change the direction of the number at y-axis
plot(log2(fpkm[,1]), log2(fpkm[,2]),
     bg = 'red', pch = 21,
     xlab = 'log2(control1_FPKM)',
     ylab = 'log2(control2_FPKM)', las = 1) #las=1: makes y axis labels horizontal

##add regression line
id <- which(is.finite(log2(fpkm[,2])) & is.finite(log2(fpkm[,1])) )
      #ensures both values are finite -> avoids issues with NaN or -Inf from log2(0)
      #which: returns row indices that satisfy the condition
      #id: used to filter valid data points for regression

nrow(fpkm) # Total number of rows (genes)

length(id) # Number of valid data points -> # of genes with finite log2 values

#add regression line
abline(lm(log2(fpkm[id,2]) ~ log2(fpkm[id,1]))) #fits a linear regression model: log2(FPKM in replicate 2) = a + b * log2(FPKM in replicate 1)

## change the width of the line 
abline(lm(log2(fpkm[id,2]) ~ log2(fpkm[id,1])),
       col = 'black', lwd = 3) #lwd=3: makes line thicker




# use ggplot2 to draw scatter plot

#load library
library(ggplot2)

#Create a Data Frame for Plotting
fpkm_c <- data.frame(c1 = as.numeric( fpkm[,1] ), #Converts two columns (biological replicates) 
                                                  #from the fpkm matrix into a numeric data frame fpkm_c
                     c2 = as.numeric( fpkm[,2] ) ) #as.numeric(fpkm[,1]) ensures that values are numeric, avoiding potential format issues.

#ggplot scatter plot
ggplot(fpkm_c, aes(x = c1, y = c2)) + #uses control 1 for x axis and control 2 for y axis
  geom_point() + #adds scatter plot points
  geom_smooth(method = lm, se = F) #method=lm: fits linear regression line
                                    #se=F: removes shaded confidence interval around the regression line

## with other color 
ggplot(fpkm_c, aes(x = c1, y = c2)) +
  geom_point(col = 'red') + #colors scatter points in red
  geom_smooth(method = lm, se = F, col = 'navy') #colors regression line navy blue


# use ggplot2 to draw scatter plot with 2d density estimation
ggplot(log2(fpkm_c), aes(x = c1, y = c2)) + #log2(fpkm_c): applies log2 transformation to normalize FPKM values
  geom_point() + #plots scatter points
  geom_density_2d() #adds contour lines representing areas with higher density of points

# This code creates scatter plots using ggplot2 
# to compare two biological replicates (c1 and c2), 
# applies linear regression, and incorporates 2D density estimation.

