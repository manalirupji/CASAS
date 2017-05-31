optimalcut_plot <- function(data = data, variable)
{

  QS2 = data
  outcome=QS2[, 'time']
  censor=QS2[, 'censor']
  marker = QS2[, 'xvar']

  fit <- coxph(Surv(as.numeric(outcome), as.numeric(factor(censor))) ~ marker) # z1=age
  c1 <- cutp(fit)$marker
  data.table::setorder(c1, "marker")
  ## [] below is used to print data.table to console
  percentile <- ecdf(c1$marker)
  #png("MR.png",height=7,width=10,units="in",res=100)
  mresid <- resid(fit)
  plot(marker, mresid, xaxt = "n",
       xlab=variable, ylab="Martingale Residuals",
       main='Martingale Residual Plot')
  lines(lowess(marker, mresid),col='red')
  range = max(marker) - min(marker)
  #axis(1, seq(min(marker), max(marker), by = range/6))
  axis(1, seq(min(marker), max(marker), by = range/6), labels = paste0(round(seq(min(marker), max(marker), by = range/6), 4), " (", round(percentile(seq(min(marker), max(marker), by = range/6)), 2)*100, "%)"))
  #dev.off()
  
  #perc <- c1[order(c1$Q),][nrow(c1), 1]
  
  #p <- percentile(perc)
  #return(p)
  
}

#optimalcut_plot(QS2, "log2_SAMHD1_Plus1")