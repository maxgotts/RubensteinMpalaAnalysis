bootstrap.t.test <- function(z,c,value=c("NDVI","EVI"),bootstrap=20000,title=paste("T-test on ",paste(value,collapse=", "),sep=""),colours=c("#34568B","#6B5B95"), perc=0.85) {
  num.samples <- min(floor(perc*nrow(z)),floor(perc*nrow(c)))
  cat("**",title,"**\n\n")
  
  cat("Running ",bootstrap," bootstrapped samples with n=",num.samples," on each iteration on ",length(value)," variables\n\n",sep="")
  
  p.values <- data.frame()
  for (val in value) {
    ps_normal <- c()
    ps_nonparam <- c()
    for (i in 1:bootstrap) {
      z.sample <- z[sample(nrow(z), num.samples), ]
      c.sample <- c[sample(nrow(c), num.samples), ]
      
      s <- shapiro.test(z.sample[,val]-c.sample[,val])
      if (s$p.value > 0.05) {
        t <- t.test(z.sample[,val], c.sample[,val], paired = TRUE, alternative = "two.sided")
        ps_normal <- c(ps_normal, t$p.value)
      } else {
        w <- wilcox.test(z.sample[,val], c.sample[,val], paired = TRUE, alternative = "two.sided")
        ps_nonparam <- c(ps_nonparam, w$p.value)
      }
    }
    temp.p.values <- data.frame(
      p=c(
        ps_normal,
        ps_nonparam
      ),label=c(
        rep("Normal",times=length(ps_normal)),
        rep("Non-parametric",times=length(ps_nonparam))
      ),variable=val
    )
    p.values <- rbind(p.values,temp.p.values)
    cat("RESULTS: variable=",val,"\n",sep="")
    cat("A total of ",sum(p.values$p>0.05)," out of ",length(p.values$p)," (",100*sum(p.values$p>0.05)/length(p.values$p),"%) of p-values were > 0.05\n",sep="")
    cat("Mean p-value = ",mean(p.values$p)," +/- ",sd(p.values$p),"\n\n",sep="")
  }
  
  par(mar=c(5,5,2,1)+.1)
  
  (gg <- ggplot(p.values, aes(p, fill=label))+
      geom_histogram(position = "stack", bins=round(bootstrap/50))+
      theme_classic()+
      facet_wrap(~variable)+
      labs(x="P-values",y="Count",fill="Test",title=title)+
      scale_color_manual(values=colours))
  # aes(y=stat(count)/bootstrap)+
  # scale_y_continuous(labels = scales::percent)+
  
  out <- list(p.values,gg)
  names(out) <- c("p.values","plot")
  
  return(out)
}



## RUN TEST ##
df <- read.csv("~/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")
cattle.abbr <- c("Cattle","CKC","CC","MC")
zebra.abbr <- c("GZ","PZ")
z <- filter(df, Species %in% zebra.abbr)
c <- filter(df, Species %in% cattle.abbr)

(z.c.boot <- bootstrap.t.test(z,c,bootstrap=50000,title="Comparison of zebra vs cattle selection of vegetation based on NDVI and EVI"))$plot

## OUTPUT ##
# Running 50000 bootstrapped samples with n=33 on each interation on 2 variables
# 
# RESULTS: variable=NDVI
# A total of 44851 out of 50000 (89.702%) of p-values were > 0.05
# Mean p-value = 0.3597991 +/- 0.2747791
# 
# RESULTS: variable=EVI
# A total of 82780 out of 100000 (82.78%) of p-values were > 0.05
# Mean p-value = 0.2860951 +/- 0.257312
