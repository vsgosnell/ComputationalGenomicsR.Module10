# https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/

# line chart

ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line(colour="red", linetype="dashed", size=1.5) +
  geom_point(colour="red", size=4, shape=21, fill="white")


plot(c(1,2), dat$total_bill, type="l", xlab="time", ylab="",
     lty=2, lwd=3, col="red")
points(c(1,2), dat$total_bill, pch=21, col="red", cex=2,
       bg="white", lwd=3)






ggplot(data=dat1, aes(x=time, y=total_bill, group=sex,
                      shape=sex, colour=sex)) +
  geom_line(aes(linetype=sex), size=1) +    
  geom_point(size=3, fill="white") +        
  expand_limits(y=0) +                      
  scale_colour_hue(name="Sex of payer", l=30) +
  scale_shape_manual(name="Sex of payer", values=c(22,21)) +
  scale_linetype_discrete(name="Sex of payer") +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people") +    
  theme_bw() +
  theme(legend.position=c(.7, .4))




par(cex=1.2, cex.axis=1.1)
matplot(dat1mat, type="b", lty=1, pch=19, col=fm_col,
        cex=1.5, lwd=3, las=1, bty="n", xaxt="n",
        xlim=c(0.7, 2.2), ylim=c(12,18), ylab="",
        main="Average Bill for Two People", yaxt="n")
#Error: object 'fm_col' not found


axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)),
     las=1, cex.axis=0.8, col=NA, line = -0.5)


grid(NA, NULL, lty=3, lwd=1, col="#000000")
abline(v=c(1,2), lty=3, lwd=1, col="#000000")
mtext("Lunch", side=1, at=1)
mtext("Dinner", side=1, at=2)
text(1.5, 17.3, "Male", srt=8, font=3)
text(1.5, 15.1, "Female", srt=33, font=3)
# Warning messages for all commands in this block ^
# 1: In doTryCatch(return(expr), name, parentenv, handler) :
#   invalid graphics state
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   invalid graphics state

