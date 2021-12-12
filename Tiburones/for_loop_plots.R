setwd("/Users/Sua/OneDrive/R Avanzado/Tarea 1")
tib <- read.csv("silvertips.csv", header= T, sep= ",")

tib$month <- factor(tib$month.year, levels= c("May-13", "Jun-13", "Jul-13", "Aug-13", 
                                         "Sep-13", "Oct-13", "Nov-13", "Dec-13", 
                                         "Jan-14", "Feb-14", "Mar-14", "Apr-14"))
dev.off()

par(mar=c(5,5,3,2), mfrow=c(3,3))

for (i in 1:length(unique(tib$month.year))){

tib_month <- tib[tib$month.year==paste(unique(tib$month.year)[i]),]

for (j in 1: length(unique(tib_month$tag))) {
  
tib_tag <- tib_month[tib_month$tag==paste(unique(tib_month$tag)[j]),]
tib_m <- mean(tib_tag$depth)
tib_m_d <- mean(tib_tag$depth[tib_tag$diel=="day"])
tib_m_n <- mean(tib_tag$depth[tib_tag$diel=="night"])

tib_se_d <- sd(tib_tag$depth[tib_tag$diel=="day"])/sqrt(length(tib_tag$depth[tib_tag$diel=="day"]))
tib_se_n <- sd(tib_tag$depth[tib_tag$diel=="night"])/sqrt(length(tib_tag$depth[tib_tag$diel=="night"]))

hist(tib_tag$depth, main= paste(unique(tib$month.year)[i],"(",unique(tib_month$tag)[j],")"), xlab=NULL, ylab= NULL)

abline( v= tib_m, lwd=3, col="black")
abline( v= tib_m_d, lty=3, lwd=3, col="red")
abline( v= tib_m_n, lty=3, lwd=3, col="blue")

rect( xleft=(tib_m_d - tib_se_d),0,xright=(tib_m_d + tib_se_d),50, col=rgb(1,0,0,0.5), lwd=0.1)
rect( xleft=(tib_m_n - tib_se_n),0,xright=(tib_m_n + tib_se_n),50, col=rgb(0,0,1,0.5), lwd=0.1)
mtext("Frequency", side=2, outer=T, line = -2)
mtext("Depth", side=1, outer=T, line = -2)
}
}


