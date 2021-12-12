                          #### uNIVERSIDAD DE COSTA RICA ####
              #### II- 2019 Programación y métodos estadísticos avanzados en R ####
                            #### SÚA JIMÉNEZ BRENES ####
                                 #### TAREA 1 ####                        
                               
setwd("/Users/Sua/OneDrive/R Avanzado/Tarea 1")
tib <- read.csv("silvertips.csv", header= T, sep= ",")

tib$month <- factor(tib$month, levels= c("May-13", "Jun-13", "Jul-13", "Aug-13", 
                                         "Sep-13", "Oct-13", "Nov-13", "Dec-13", 
                                         "Jan-14", "Feb-14", "Mar-14", "Apr-14"))
####
####
                            #### GRÁFICOS DE BARRAS ####
####
####

png("Gráficos de barras.png", width=11, units="in", height=7.5, res = 600)

# GRÁFICO DE BARRAS DE LA PROFUNDIDAD SEGÚN DÍA/NOCHE SEGÚN EL MES DEL AÑO

prom_depth1 <- tapply(tib$depth, list(tib$diel, tib$month.year), mean)

desv_depth1 <- tapply(tib$depth, list(tib$diel, tib$month.year), sd)

meses <- levels(tib$month)

par(mar = c(5, 5, 3, 2), mfrow=c(2, 1))

barx <- barplot(prom_depth1, col = c("white", "black"), 
                beside = T, ylab = "Depht (m)", 
                ylim = c(0, max(prom_depth1 + desv_depth1) + 10), 
                names.arg = meses, las = 1, yaxs = "i", cex.lab=1.2)

arrows(barx, prom_depth1 + desv_depth1, barx, prom_depth1, angle = 90, code = 1, length = 0.05)

legend(30, 56, bty = "n", 
                cex = 1, legend = c("Day", "Night"), x.intersp
                =0.4, y.intersp= 0.7, pch=c(0, 15))

mtext("Month-Year", side = 1, line = 2, cex=1.2)

box(bty = "l")

# GRÁFICO BARRAS DE LA PROFUNDIDAD SEGÚN LA HORA DEL DÍA POR SEXO

prom_depth2 <- tapply(tib$depth, list(tib$sex, tib$hour), mean)

desv_depth2 <- tapply(tib$depth, list(tib$sex, tib$hour), sd)

horas <- seq(1, 24, 1)

bary<- barplot(prom_depth2, seq(1, 12, 0.5), col = c("white", "black"), 
               beside = T, ylab = "Depht (m)", 
               ylim = c(0, max(prom_depth2 + desv_depth2) + 10), 
               names.arg = horas, las = 1, yaxs = "i", cex.lab=1.2)
 
rect(55, 0, 145, 50, density=1000, col="grey")
 
barplot(prom_depth2, seq(1, 12, 0.5), col = c("white", "black"), 
              beside = T, ylab = "Depht (m)", 
              xlab = "Time of the day", 
              ylim = c(0, max(prom_depth2 + desv_depth2) + 10), 
              names.arg = horas, las = 1, yaxs = "i", add=T, cex.lab=1.2)
 
mtext("Time of the day", side = 1, line = 2, cex=1.2)
 
arrows(bary, prom_depth2 + desv_depth2, bary, prom_depth2, angle = 90, code = 1, length = 0.05)
 
legend(180, 50, bty = "n", 
       cex = 1.1, legend = c("Female", "Male"), x.intersp
       =0.4, y.intersp= 0.7, pch=c(22, 15))
 
box(bty = "l")
 
dev.off()
                 
####
####
                                 #### GRÁFICOS DE PUNTOS ####
####
####

png("Gráficos de puntos.png", width=11, units="in", height=7.5, res = 600)


## GRAFICO DE PUNTOS DE LA PROFUNDIDAD SEGÚN DÍA/NOCHE SEGÚN EL MES DEL AÑO

par(mar = c(2, 5, 3.1, 2), mfrow=c(2, 1))

prom_depth3 <- as.data.frame(tapply(tib$depth, list(tib$month.yearID, tib$diel), mean))
desv_depth3 <- as.data.frame(tapply(tib$depth, list(tib$month.yearID, tib$diel), sd))
meses <- levels(tib$month)

plotx <- plot(seq(1.25, 12.25, 1), -prom_depth3$night, 
              xlim = c(0, 12.25), 
              ylim = c(-max(prom_depth3)-20, 0), 
              pch=19, cex=1.2, col="black", 
              xaxt = 'n', 
              bty = 'n', 
              ylab="Depth (m)", xlab="", cex.lab=1.2)

arrows(seq(1.25, 12.25, 1), -prom_depth3$night +desv_depth3$night , 
       seq(1.25, 12.25, 1), -prom_depth3$night -desv_depth3$night, 
       angle = 90, code = 3, length = 0.05)

arrows(seq(0.75, 12, 1), -prom_depth3$day +desv_depth3$day, 
       seq(0.75, 12, 1), -prom_depth3$day -desv_depth3$day, 
       angle = 90, code = 3, length = 0.05)

points(seq(0.75, 12, 1), -prom_depth3$day, 
       ylim = c(-max(prom_depth3), 0), 
       pch=22, col="black", bg="white", cex=1.2)

mtext("Month", side = 3, line = 2.2, cex=1.2)

axis(3, at=1:12, labels=meses, line=-0.5)

axis(3, at=-1:13, labels=F, lwd.tick=0, line=-0.5 )


legend(x=10, y=-40, bty = "n", 
       cex = 1, legend = c("Day", "Night"), x.intersp
       =0.5, y.intersp= 0.7, pch=c(22, 19))

## GRÁFICO DE PUNTOS DE LA PROFUNDIDAD SEGÚN LA HORA DEL DÍA POR SEXO

prom_depth4 <- as.data.frame(tapply(tib$depth, list(tib$hour, tib$sex), mean))
desv_depth4 <- as.data.frame(tapply(tib$depth, list(tib$hour, tib$sex), sd))

plot(1, type="n", 
     xlim=c(0, 23), ylim = c(-max(prom_depth4)-20, 0), 
     xaxt = 'n', bty = 'n', 
     ylab="Depth (m)", xlab="", cex.lab=1.2)



rect(5.5, -0.5, 17.5, -60, density=1000, col="grey")

points(seq(-0.25, 23, 1), -prom_depth4$Female, 
       pch=19, cex=1.2, col="black")

arrows(seq(-0.25, 23, 1), -prom_depth4$Female +desv_depth4$Female , 
       seq(-0.25, 23, 1), -prom_depth4$Female -desv_depth4$Female, 
       angle = 90, code = 3, length = 0.05)

arrows(seq(0.25, 23.25, 1), -prom_depth4$Male +desv_depth4$Male, 
       seq(0.25, 23.25, 1), -prom_depth4$Male -desv_depth4$Male, 
       angle = 90, code = 3, length = 0.05)
  
points(seq(0.25, 23.25, 1), -prom_depth4$Male, 
       pch=22, col="black", bg="white", cex=1.2)

mtext("Time of the day", side = 3, line = 2.2, cex=1.2)

axis(3, at=0:23, line =-0.5)

axis(3, at=-3:23.5, labels=F, lwd.tick=0, line=-0.5)

legend(x=18, y=-37, bty = "n", 
       cex = 1, legend = c("Male", "Female"), x.intersp
       =0.5, y.intersp= 0.7, pch=c(22, 19))

dev.off()
