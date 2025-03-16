# https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/

# line chart

ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line(colour="red", linetype="dashed", size=1.5) +
  geom_point(colour="red", size=4, shape=21, fill="white")


plot(c(1,2), dat$total_bill, type="l", xlab="time", ylab="",
     lty=2, lwd=3, col="red")
points(c(1,2), dat$total_bill, pch=21, col="red", cex=2,
       bg="white", lwd=3)










