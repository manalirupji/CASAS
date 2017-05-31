
########################################################################
#######Quantile regression for subpopulation #######
########################################################################

#QS1<-read.table(file = "C:/Users/xzhan60/Downloads/bioinformatics research projects/apps/Quantile Analysis/data/BRCA_for_quantile_survival_analysis.csv", header=T, sep=",", stringsAsFactors = F)
#res <- QSE(QS1, "log2_SAMHD1_Plus1", 25, "Not Low")

QSE<-function(data, percentile, exp.ref){

  percentile = percentile
  
  QS2 = data
  outcome=QS2[, 'time']
  censor=QS2[, 'censor']
  exp=as.factor(QS2[, 'Group'])
  
  
  Dpoint=10
  Dlabel = "T"
  iter = 1000 
  
  exp <- relevel(exp , ref=exp.ref)
  cvt.length <- length(levels(exp))
  char.cvt<-levels(exp)
  
  ####
  upperQ = 1
  taus <-(1/Dpoint)*upperQ*seq(1,Dpoint,1)
  taus[10] <- upperQ
  
  fit<- cequre(outcome,censor, model.matrix(~exp)[, -1], taus=taus, res=iter)
  
  for (k in 2:cvt.length){
    m_survt <- (round(fit$bt[k,]+ fit$bt[1,],1)+round(fit$bt[1,],1))/2
  }
  
  surv <- survfit(Surv(as.numeric(outcome), as.numeric(factor(censor))) ~ exp)
  #len<-floor(max(surv$time))
  sum <- summary(surv)
  survp <- sum$surv
  grp <- factor(sum$strata)
  df <- data.frame(group=grp, survp=survp)
  res <- tapply(df$survp, df$group, mean)
  
  len<-max(surv$time)
  pos<-seq(0,len, len/Dpoint)
  NR<-nrisk(surv, times=pos)

  A<-survdiff(Surv(outcome,censor) ~ exp) 
  p.valdec <- 1 - pchisq(A$chisq, length(A$n) - 1)
  p.val <- round(1 - pchisq(A$chisq, length(A$n) - 1),3)
  
  for (k in 2:cvt.length){
    est<-fit$bt[k,]
    va <- fit$va[k,k,]
    va <- sapply(va,function(x) ifelse(x<0,0,x))
    #This assumes that the length of exposure is the n, i.e. there are no missing outcomes
    low <-fit$bt[k,]-qt(0.975,df=c(length(exp)-1))*sqrt(va)
    up <-fit$bt[k,]+qt(0.975,df=c(length(exp)-1))*sqrt(va)
    
  }
  
  cox <- coxph( Surv(outcome,censor) ~ exp) 
  coxsum <- summary(cox, conf.int=0.95) 
  
  coxout <- cbind.data.frame(as.vector(coxsum$conf.int[,2]), as.vector(coxsum$conf.int[,3]),
    as.vector(coxsum$conf.int[,4]), as.vector(coxsum$coefficients[, 4]), as.vector(coxsum$coefficients[, 5]))
  colnames(coxout) <- c("Hazard Ratio", "95% CI Lower Limit", "95% CI Upper Limit", "Z Score", "P-value")
  
  coxest <- log(1/coxsum$conf.int[1])
  coxlow <- log(1/coxsum$conf.int[3])
  coxup <- log(1/coxsum$conf.int[4])
  
  #All_cox <- c(1.1, 1, coxlow, coxup, coxest)
  #CI_Data <- rbind.data.frame(CI_Data, All_cox)
  taus2 <- c(taus, 1.1)
  gene <- c(paste("Q", 1:10, sep = ""), "Overall")
  low2 <- c(low, coxlow)
  up2 <- c(up, coxup)
  est2 <- c(est, coxest)
  
  #CI_Data <- cbind.data.frame(taus, paste("Q", 1:10, sep = ""), low, up, est)
  CI_Data <- cbind.data.frame(taus2, gene, low2, up2, est2)
  colnames(CI_Data) <- c("ID", "Gene", "CI_lower", "CI_higher", "PointEstimate")
  CI_Data$boxcolor <- apply(CI_Data[, c(3, 4)], 1, function(x) boxColor(low= x[1], high= x[2],  col_low = "blue", col_mid = "black", col_high= "red"))
  CI_Data$ID <- rev(1:11/10)
  CI_Data <- CI_Data[order(-CI_Data$ID),]
  
  #A<-survdiff(Surv(outcome,censor) ~ exp) 
  #p.valdec <- 1 - pchisq(A$chisq, length(A$n) - 1)
  #p.val <- round(1 - pchisq(A$chisq, length(A$n) - 1),3)
  
  #p.ind <- ifelse(p.val <= 0.05, ind, 0)
  if (length(res) < 2){
    p.ind <- 3
  } else if (p.val > 0.05) {
    p.ind <- 0
  } else if (res[[1]] > res[[2]]){
    p.ind <- 1
  } else if (res[[1]] < res[[2]]){
    p.ind <- 2
  }
  
  all <- c("Overall", p.ind)
  
  for (k in 2:cvt.length){
    est<-fit$bt[k,]
    va <- fit$va[k,k,]
    va <- sapply(va,function(x) ifelse(x<0,0,x))
    #This assumes that the length of exposure is the n, i.e. there are no missing outcomes
    low <-fit$bt[k,]-qt(0.975,df=c(length(exp)-1))*sqrt(va)
    up <-fit$bt[k,]+qt(0.975,df=c(length(exp)-1))*sqrt(va)
  }
  
  CI_Data2 <- cbind.data.frame(taus, as.character(paste("Q", 1:10, sep = "")), low, up, est, stringsAsFactors=FALSE)
  colnames(CI_Data2) <- c("ID", "Gene", "CI_lower", "CI_higher", "PointEstimate")
  CI_Data2$indicator <- apply(CI_Data2[, c(3, 4)], 1, function(x) significant(low= x[1], high= x[2]))
  CI_Data2 <- CI_Data2[, c("Gene", "indicator")]
  
  CI_Data2 <- rbind.data.frame(CI_Data2, all)

  CI_Data3 <- CI_Data[, c(2, 5, 3, 4)]
  CI_Data3$indicator <- apply(CI_Data3[, c(3, 4)], 1, function(x) significant(low= x[1], high= x[2]))
  
  results <- list(exp = exp, fit = fit, m_survt = m_survt, cvt.length = cvt.length, 
                  NR = NR, p.val = p.val, CI_Data = CI_Data, CI_Data2 = CI_Data2, CI_Data3 = CI_Data3, surv = surv, 
                  taus = taus, pos = pos, char.cvt = char.cvt, exp.ref = exp.ref, coxout = coxout, percentile = percentile)
  
  return(results)
}


