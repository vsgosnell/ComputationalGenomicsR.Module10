
# Loading the FPKM reads counts

norData <- read.table('/Users/veronicagosnell/Desktop/R/ComputationalGenomicsR.Module10/vehicle_drug_feature_counts.fpkm.txt', row.names = 1, header = 1)
colnames(norData) <- c('c1','c2','t1','t2')

# Draw the boxplot for each column from FPKM
boxplot(log2(norData))

## remove the outline
boxplot(log2(norData), outline = F)

## change the color of the box
boxplot(log2(norData), outline = F, col = c('red','red',
                                            'navy','navy'))
##remove the border
boxplot(log2(norData), outline = F, col = c('red','red','navy','navy'),
        border = c('red','red','navy','navy'))

## add the median color, and y label 
boxplot(log2(norData), outline = F, col = c('red','red',
                                            'navy','navy'),
        border = c('red','red','navy','navy'),
        medcol = rep('white', 4), ylab = 'log2(FPKM)')




# Generating hist plots for each FPKM column


#Let's first generate hist plot for control rep1
hist(log2(norData$c1))

# changing the x label
hist(log2(norData$c1), xlab = 'log2(FPKM)')

# changing the direction of the y-axis and changing the main title of the figure
hist(log2(norData$c1), xlab = 'log2(FPKM)', las = 1,
     main = 'Histogram of control rep1')

# adding the color for the plot
hist(log2(norData$c1), xlab = 'log2(FPKM)', las = 1,
     main = 'Histogram of control rep1', col = 'cornflowerblue')

# adding more breakpoints between histogram cells
hist(log2(norData$c1), xlab = 'log2(FPKM)', las = 1,
     main = 'Histogram of control rep1', col = 'cornflowerblue',
     breaks = 100)

# generating the hist plot for each column of the data
par(mfrow = c(2,2), mar = c(6,4,1,1)) #splits plotting area into 2x2 grid, allowing 4 hists to be plotted in single figure
                                      #mar -> sets plot margins: bottom (for x-axis labels), left (for y-axis labels), top, right
idCol <- 1 #tracks column index for labeling histograms
apply(log2(norData), 2, function(x) #apply -> applies log2 transformation to all values in norData -> normalizes expression values
{                                   #applies the function column-wise aka 2
  hist(x, xlab = 'log2(FPKM)', las = 1, #plots hist for column x
                                        #labels x-axis: log2(FPKM)
                                        #las=1 -> makes axis labels horizontal
       main = paste('Histogram of', colnames(norData)[idCol], #generates title dynamically using col name
                    sep = '_'), #joins words with an underscore
       col = 'gold', xlim = c(-5, 15), ylim = c(-5, 200), #fills bars gold, sets x and y-axis ranges
       breaks = 100)  #uses 100 bins for the hist
  idCol <<- idCol + 1 #<<- is special assignment operator that modifies idCol outside the function scope
                      #ensures the next histogram gets the correct col name
})

# The code generates histograms for each column in norData, 
# which contains normalized expression values (likely FPKM values). 
# It uses log2 transformation to make the data more normally distributed before plotting.

