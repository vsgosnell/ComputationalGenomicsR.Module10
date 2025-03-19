# https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/


dat1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)


ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge(),
           colour="black") +
  scale_fill_manual(values=c("#999999", "#E69F00"))



mf_col <- c("#3CC3BD", "#FD8210")
barplot(dat1, beside = TRUE, border=NA, col=mf_col)
# Error in barplot.default(dat1, beside = TRUE, border = NA, col = mf_col) : 
# 'height' must be a vector or a matrix

legend("topleft", row.names(dat1), pch=15, col=mf_col)


# converted the data frame to a matrix for barplot():

dat1mat <- matrix( dat1$total_bill,
                   nrow = 2,
                   byrow=TRUE,
                   dimnames = list(c("Female", "Male"), c("Lunch", "Dinner"))
)

dat1mat

