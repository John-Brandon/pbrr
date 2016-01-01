###
# Functions for plotting PBR simulation output
# OS: Mac OS 10.10.4
# R version 3.2.0 (2015-04-16)
# Author: John R. Brandon <jbrandon@gmail.com>
#
###
library(dplyr)
library(ggplot2)

Box <- function(output, ii, xx, pch=16){
  points(x = xx[ii], y = median(Dets), pch = 16)
  lines(c(xx[ii], xx[ii]), c(quantile(output, 0.05), quantile(output, 0.95)), lty = 1)
  abline(h = 0.5, lty = 2, col = "gray") # base case MNPL
}

zeh_plot = function(cv_n){
### Plot median and 90% interval. cf Figs 4 and 5 in Wade (1998)
  plot(0, 0, xlab=expression('N'[MIN]*' Percentile'), ylab="Depletion (1+)", type="n", xlim=c(0.0,0.55), ylim=c(0,1.0), yaxs="i", xaxs="i")
  for (ii in 1:length(lower_tail)){
    Box(depl[,ii], ii = ii, xx = lower_tail)
    if(ii > 1) lines(x = c(lower_tail[ii-1], lower_tail[ii]), y = c(median(depl[,ii-1]), median(depl[,ii])))
  }
  # title(main = expression("Cetacean: "*CV[N]*" = 0.20"))
  # title(main = expression("Cetacean: "*CV[N]*" = "*round(cv_n)))
}


