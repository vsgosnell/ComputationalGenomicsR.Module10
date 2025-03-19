# https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/


# drawing lines

# With ggplot2, there’s geom_errorbar(), 
# and with base graphics there’s lines() and segments().

bp <- ggplot(dat, aes(x=cond, y=result)) +
  geom_bar(position=position_dodge(), stat="identity")
bp + geom_errorbar(width=0.5, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")
# Error in `geom_bar()`:
# ! Problem while computing aesthetics.
# ℹ Error occurred in the 1st layer.
# Caused by error:
#   ! object 'cond' not found
# Run `rlang::last_trace()` to see where the error occurred.


bar_width <- 2
bp_df <- barplot(dat$result, names.arg=dat$cond, ylim=c(0,13), las=1, width=bar_width, border=NA)

# Error in barplot.default(dat$result, names.arg = dat$cond, ylim = c(0,  : 
# 'height' must be a vector or a matrix

bp_df
x0 <- bp_df[,1] - 0.5*bar_width/2
x1 <- bp_df[,1] + 0.5*bar_width/2
y0 <- dat$hline
y1 <- dat$hline
segments(x0, y0, x1, y1, col="#AA0000", lwd=3)
#all other commands fail bc bp_df was not created

library("ggplot2")


sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill))
 geom_point(shape=1)
sp + facet_grid(. ~ sex)
sp
#does not produce plot

#not continuing with this tutorial -> too many errors produced


