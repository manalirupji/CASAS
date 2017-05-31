#library(survMisc)

#setwd("C:/Users/xzhan60/Downloads/bioinformatics research projects/quantile regression/Quantile analysis data/RT No Chemo/")

#QS<-read.table(file = "GBMLGG_survival_analysis_RT_noChemo.csv", header=T, sep=",", stringsAsFactors = F)
#QS$OS_CENSOR  <- QS$vital_status
#QS$OS_SURTIME <- ifelse(QS$vital_status==1, as.numeric(QS$days_to_death), as.numeric(QS$days_to_last_followup))
#QS$OS_SURTIME <- QS$OS_SURTIME/365
#QS1 <- QS[complete.cases(QS$OS_SURTIME),]
#QS2 <- QS1[, c('patient_id', 'OS_CENSOR', 'OS_SURTIME', 'log2_SAMHD1_Plus1')]

optimalcut <- function(data = data)
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
#mresid <- resid(fit)
#plot(marker, mresid, xaxt = "n",
#     xlab=variable, ylab="Martingale Residuals",
#     main='Martingale Residual Plot')
#lines(lowess(marker, mresid),col='red')
#range = max(marker) - min(marker)
#axis(1, seq(min(marker), max(marker), by = range/6))
#dev.off()

perc <- c1[order(c1$Q),][nrow(c1), 1]

p <- percentile(perc)
return(p)

}

#perc <- optimalcut(QS2, "log2_SAMHD1_Plus1")

