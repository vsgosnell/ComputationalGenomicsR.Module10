# https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/

# bar chart

# The data frame below represents 
# (imaginary) average bills for lunch and dinner.
#using base graphics:
dat <- data.frame(
  time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)

#load ggplot library
library(ggplot2)

#using ggplot2:
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") +
  guides(fill=FALSE) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people")

par(las=1)
barplot(dat$total_bill,
        names.arg=dat$time,
        col="#AFC0CB",
        border=FALSE,
        main="Average Bill for Two-Person Meal")




#ggplot has built-in data management

#load the required package per the url of this tutorial
install.packages("reshape2")
library("reshape2")

# Bar graph of counts
ggplot(data=tips, aes(x=day)) +
  geom_bar(stat="count")
# gives you a bar chart where the height shows the number of tips per day.


# in base graphics, you work with the data outside of the visualization functions. 
# In this case, you can use table() to aggregate by day, and you pass that result to barplot().

# Number of tips per day
tips_per_day <- table(tips$day)

# Bar graph of counts
barplot(tips_per_day)


# For this example, I’d order the bars by time though — 
# Thursday through Sunday — instead of order of appearance in the data frame. 
# In base graphics, you work outside the barplot() function. 
# Order how you want and pass the result to the function.

# Order by day
tips_per_day <- tips_per_day[c("Thur", "Fri", "Sat", "Sun")]


# With ggplot2, you prepare the data in a similar fashion, before the ggplot() call:
# Reorder
tips$day <- factor(tips$day, c("Thur", "Fri", "Sat", "Sun"))






ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(colour="black", stat="identity",
           position=position_dodge(),
           size=.3) +                       
  scale_fill_hue(name="Sex of payer") +     
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people") +    
  theme_bw()



par(cex=1.2, cex.axis=1.1)
barplot(dat1mat, beside = TRUE, border=NA, col=mf_col,
        main="Average Bill for Two People", yaxt="n")
axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)),
     las=1, cex.axis=0.8)
grid(NA, NULL, lwd=1, lty=1, col="#ffffff")
abline(0, 0)
text(1.5, dat1mat["Female", "Lunch"], "Female", pos=3)
text(2.5, dat1mat["Male", "Lunch"], "Male", pos=3)
text(1.5, dat1mat["Female", "Lunch"]+0.7, "SEX OF PAYER",
     pos=3, cex=0.75)

