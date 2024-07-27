setwd(readClipboard())
getwd()
library(ggplot2);library(reshape2);library(psych);library(pheatmap);library(corrplot)
var1 <- read.csv("traits.csv",row.names = 1,encoding = "utf-8")
var2 <- read.csv("biotaA.csv",row.names = 1,encoding = "utf-8")


double_cor <- function(data1, data2, wid, hei){
  r <- matrix(NA,ncol = ncol(data1),nrow = ncol(data2))
  p <- matrix(NA,ncol = ncol(data1),nrow = ncol(data2))
  sig <- matrix(NA,ncol = ncol(data1),nrow = ncol(data2))
  r <- as.data.frame(r)
  p <- as.data.frame(p)
  sig <- as.data.frame(sig)
  for (i in 1:ncol(data1)) {
    for (j in 1:ncol(data2)) {
      name1 <- colnames(data1)[i]
      name2 <- colnames(data2)[j]
      res <- corr.test(data1[,i],data2[,j],method = 'pearson')
      r[j,i] <-  res$r
      p[j,i] <-  res$p
      colnames(r)[i] <- name1
      rownames(r)[j] <- name2
      colnames(p)[i] <- name1
      rownames(p)[j] <- name2
    }
  }
  for (m in 1:ncol(p)) {
    for (n in 1:nrow(p)) {
      if (p[n,m] >= 0.05) sig[n,m] <- ''
      else if (p[n,m] >= 0.01 & p[n,m] < 0.05) sig[n,m] <- '*'
      else if (p[n,m] >= 0.001 & p[n,m] < 0.01) sig[n,m] <- '**'
      else if (p[n,m] < 0.001) sig[n,m] <- '***'
      colnames(sig) <- colnames(p)
      rownames(sig) <- rownames(p)
    }
  }
  result <- list(r = r, p = p, sig = sig)
  pdf("1pearson.pdf",width = wid , height = hei)
  corrplot(corr = as.matrix(r),
           method = 'square', 
           col = RColorBrewer::brewer.pal(10, "RdBu"),
           number.cex = 0.8, 
           tl.col = "black",
           #diag = FALSE, 
           tl.cex = 0.8,
           p.mat =  as.matrix(p),
           sig.level = c(.001, .01, .05),
           insig = "label_sig",
           pch.cex = 1.5, 
           tl.srt = 0,
           pch.col = "black")
  dev.off()
  
}
gg <- double_cor(var1,var2)






