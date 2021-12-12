#Abrir base de datos
setwd("/Users/Sua/OneDrive/R Avanzado/Tarea 1")
tib <- read.csv("silvertips.csv", header= T, sep= ",")

#Gr�fico 1 

tib$month <- factor(tib$month, levels= c("May-13", "Jun-13", "Jul-13", "Aug-13",
                                         "Sep-13", "Oct-13", "Nov-13", "Dec-13",
                                         "Jan-14", "Feb-14", "Mar-14", "Apr-14"))

png("Gr�ficos de barras.png", width=11,
    units="in", height=7.5, res = 600)
# Altura de las barras
xx <- tapply(tib$depth,list( tib$diel,tib$month.year ), mean)

# Desviación estándard
yy <- tapply(tib$depth,list( tib$diel,tib$month.year ), sd)


# Número de replicas
zz <- tapply(tib$depth,list( tib$diel,tib$month.year ), length)


# Nombres de estaciones
meses <- levels(tib$month)


# Crear gráfico
par(mar = c(5, 5, 3, 2), mfrow=c(2,1))

barx <- barplot(xx, col = c("white", "black"), 
                beside = T, ylab = "Depht (m)", 
                ylim = c(0, max(xx + yy) + 10),
                names.arg = meses, las = 1, yaxs = "i", cex.lab=1.2)

# Barras de error
arrows(barx, xx + yy, barx, xx, angle = 90, code = 1, length = 0.05)


# Generar una leyenda
legend(30, 56, bty = "n",
       cex = 1,legend = c("Day","Night"), x.intersp
=0.4, y.intersp= 0.7, pch=c(0, 15))

mtext("Month-Year", side = 1, line = 2, cex=1.2)

box(bty = "l")



# Altura de las barras
x <- tapply(tib$depth,list( tib$sex,tib$hour ), mean)
# Desviación estándard
y <- tapply(tib$depth,list(tib$sex,tib$hour ), sd)
# Número de replicas
z <- tapply(tib$depth,list(  tib$sex,tib$hour ), length)


# Nombres de estaciones
horas <- seq(1,24,1)
str(tib)

# Crear gráfico
x
bary <- barplot(x,seq(1,12,0.5), col = c("white", "black"), 
                beside = T, ylab = "Depht (m)", 
                ylim = c(0, max(x + y) + 10),
                names.arg = horas, las = 1, yaxs = "i", cex.lab=1.2)

rect(0,0,55,50, density=1000, col="grey")

rect(145,0,245,50, density=1000, col="grey")

barplot(x,seq(1,12,0.5), col = c("white", "black"), 
        beside = T, ylab = "Depht (m)", 
        xlab = "Time of the day", 
        ylim = c(0, max(x + y) + 10),
        names.arg = horas, las = 1, yaxs = "i", add=T, cex.lab=1.2)

mtext("Time of the day", side = 1, line = 2, cex=1.2)

# Barras de error
arrows(bary, x + y, bary, x, angle = 90, code = 1, length = 0.05)


# Generar una leyenda
legend(180, 50, bty = "n",
       cex = 1.1,legend = c("Female","Male"), x.intersp
       =0.4, y.intersp= 0.7, pch=c( 22,15))


box(bty = "l")

dev.off()
###
###
###
###
###

png("Gr�ficos de puntos.png", width=11,
    units="in", height=7.5, res = 600)

par(mar = c(2, 5, 3.1, 2), mfrow=c(2,1))

##gr�fico 3

xx2 <- as.data.frame(tapply(tib$depth,list( tib$month.yearID,tib$diel ), mean))
yy2 <- as.data.frame(tapply(tib$depth,list( tib$month.yearID,tib$diel ), sd))

plotx <- plot(seq(1.25,12.25,1),-xx2$night,
     xlim = c(0.75,12.25),
     ylim = c(-max(xx2)-20,0),
     pch=19,cex=1.2, col="black",
     xaxt = 'n', 
     bty = 'n',
     ylab="Depth (m)", xlab="", cex.lab=1.2)

arrows(seq(1.25,12.25,1), -xx2$night +yy2$night ,
       seq(1.25,12.25,1),-xx2$night -yy2$night,
       angle = 90, code = 3, length = 0.05)

arrows(seq(0.75,12,1), -xx2$day +yy2$day,
       seq(0.75,12,1),-xx2$day -yy2$day,
       angle = 90, code = 3, length = 0.05)

points(seq(0.75,12,1), -xx2$day,
       ylim = c(-max(xx2),0),
       pch=22, col="black", bg="white", cex=1.2)

mtext("Month", side = 3, line = 2.2, cex=1.2)

axis(3, at=1:12, labels=meses)

axis(3, at=-1:12,labels=F, lwd.tick=0 )


legend(x=10,y=-40, bty = "n",
       cex = 1,legend = c("Day","Night"), x.intersp
       =0.2, y.intersp= 0.7, pch=c(22,19))

##Gr�fico 4

xx3 <- as.data.frame(tapply(tib$depth,list( tib$hour, tib$sex ), mean))
yy3 <- as.data.frame(tapply(tib$depth,list( tib$hour, tib$sex  ), sd))

plot(seq(-0.25,23,1),-xx3$Female,
     xlim = c(-1,23),
     ylim = c(-max(xx3)-20,0),
     pch=19,cex=1.2, col="black",
     xaxt = 'n', 
     bty = 'n',
     ylab="Depth (m)", xlab="", cex.lab=1.2)

arrows(seq(-0.25,23,1), -xx3$Female +yy3$Female ,
       seq(-0.25,23,1),-xx3$Female -yy3$Female,
       angle = 90, code = 3, length = 0.05)

arrows(seq(0.25,23.25,1), -xx3$Male +yy3$Male,
       seq(0.25,23.25,1),-xx3$Male -yy3$Male,
       angle = 90, code = 3, length = 0.05)

points(seq(0.25,23.25,1), -xx3$Male,
       pch=22, col="black", bg="white", cex=1.2)

mtext("Hour", side = 3, line = 2.2, cex=1.2)


axis(3, at=0:23)

axis(3, at=-3:23,labels=F, lwd.tick=0 )


legend(x=18,y=-37, bty = "n",
       cex = 1,legend = c("Male","Female"), x.intersp
       =0.2, y.intersp= 0.7, pch=c(22,19))

dev.off()



