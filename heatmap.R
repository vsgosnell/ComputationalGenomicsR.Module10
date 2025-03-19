
# heat map

install.packages("gplots")
library('gplots')

#check the summary of the data
apply(USArrests,1,function(x){quantile(x)})


df <- t(apply(as.matrix(USArrests), 1, function(x){(x - mean(x))/sd(x)}))

aa <- heatmap.2(df[hc$order,], Rowv = F,Colv = F, 
                dendrogram= 'n', 
                scale = 'none', trace = 'none',margins =c(10,8),
                col = colorRampPalette(c('navy', 'white','red'))(n = 10))

