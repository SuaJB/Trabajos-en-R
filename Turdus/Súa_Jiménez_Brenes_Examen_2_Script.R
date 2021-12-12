#Obtención de datos de la librería Xeno-Canto
install.packages("warbleR")
library(warbleR)
library(ggplot2)

##Creación de base de datos del género Turdus
setwd("/Users/Sua/OneDrive/Carpetas viejas/R Avanzado/Examen 2/")
write.csv(turdus,'turdus.csv')
turdus <- read.csv("turdus.csv")
turdus <- querxc("Turdus", download = FALSE)
head(turdus)


#Limpiar la base de datos

##Selección de las filas que contengan "song" para la columna "Vocalizatión type"

turdus_0 <- turdus[grep("song", turdus$Vocalization_type),]

############El argumento "ingore.case" permite ignorar las mayusculas cuando tiene por valor TRUE

turdus2 <- turdus[grep("song", turdus$Vocalization_type, ignore.case = TRUE),]
#Visualización de los datos



dist_bird <- function(data) { 
data$Latitude <- as.numeric(as.character(data$Latitude))
Lat_Up <- NULL
Lat_Down <- NULL
Lat_Mid <- NULL
Specie <- NULL

for(i in 1: length(unique(data$Specific_epithet))){
  subdata <- data[data$Specific_epithet==unique(data$Specific_epithet)[i],]
  upper_lim <- max(subdata$Latitude, na.rm = TRUE)
  lower_lim <- min(subdata$Latitude, na.rm = TRUE)
  midpoint <-upper_lim - (upper_lim - lower_lim)/2
  
Lat_Up <- c(Lat_Up, upper_lim)
Lat_Down <- c(Lat_Down, lower_lim)
Lat_Mid <- c(Lat_Mid, midpoint)  
Specie <- c(Specie, paste("Turdus", " ", unique(data$Specific_epithet)[i]))
  }

data_final_Lat <<- data.frame(Specie, Lat_Up, Lat_Down, Lat_Mid)

}
    
#Probando la función  
dist_bird(turdus)

##Longitud
dist_bird_long <- function(data, zone) { 
  data$Longitude <- as.numeric(as.character(data$Longitude))
  Lon_Up <- NULL
  Lon_Down <- NULL
  Lon_Mid <- NULL
  Location <- NULL
  Specie <- NULL
  
  for(i in 1: length(unique(data$Specific_epithet))){
    subdata <- data[data$Specific_epithet==unique(data$Specific_epithet)[i],]
    upper_lim <- max(subdata$Longitude, na.rm = TRUE)
    lower_lim <- min(subdata$Longitude, na.rm = TRUE)
    midpoint <-upper_lim - (upper_lim - lower_lim)/2
    
    if (upper_lim > -25 & -25 > lower_lim) zona <- "Ambos" else if(lower_lim > -25) zona <- "Viejo Mundo" else zona <- "Nuevo Mundo"
    
    Lon_Up <- c(Lon_Up, upper_lim)
    Lon_Down <- c(Lon_Down, lower_lim)
    Lon_Mid <- c(Lon_Mid, midpoint)  
    Location <- c(Location, zona)
    Specie <- c(Specie, paste("Turdus", " ", unique(data$Specific_epithet)[i]))
  }
  
  data_final_Lon <<- data.frame(Specie, Location, Lon_Up, Lon_Down, Lon_Mid)
  
  if( zone != "Ambos") data_final_Lon <<- data_final_Lon[data_final_Lon$Location==zone,]
  
}

dist_bird_long(turdus, zone = "Ambos")

dist_bird_long(turdus, zone = "Nuevo Mundo")

dist_bird_long(turdus, zone = "Viejo Mundo")
data <- turdus

data$Date <- as.Date(data$Date)
data$year <- format(data$Date,"%Y")
data$month <- format(data$Date,"%m")
data <- data[order(as.Date(data$Date, format="%d/%m/%Y")),]

xx <- NULL

if (x!=length(data$Date)) {
  if (data$year[x]==data$year[x+1]!=data$year[x-1] | data$year[x]!=data$year[x+1]==data$year[x-1] ) {
   xx<- c(xx, 1)
    ifelse(xx<- c(xx, 0))
  }

  
  }
x <- 4



#Rango fechas

##Limpiando la base
library(reshape2)

temp_bird <- function(data) { 

  data <- data[-grep("0216|0206|NA|0201", data$Date),]
  data <- data[!is.na(data$Date),]
  data$year <- format(data$Date,"%Y")
  inicio <- NULL
  final <- NULL
  Rango <- NULL
  Tipo <- NULL
  Specie <- NULL
  Year <- NULL
  

 for(g in 1:2) { 
  
  if(g==1) {
    data1 <- data[grep("song", data$Vocalization_type, ignore.case = TRUE),]
    tipo <- "Canto"
  } else { 
    data1 <- data[-grep("song", data$Vocalization_type, ignore.case = TRUE),]
    tipo <- "Llamado"
  }
   
  for(f in 1: length(na.omit(unique(data1$year)))){  
   
  data2 <- data1[data1$year==na.omit(unique(data1$year))[f],]
   
  for(i in 1: length(unique(data2$Specific_epithet))){
    
    subdata <- data2[data2$Specific_epithet==unique(data2$Specific_epithet)[i],]
    final<- max(subdata$Date, na.rm = TRUE)
    inicio <- min(subdata$Date, na.rm = TRUE)
    rango <- as.numeric(final-inicio)
    
    Tipo <- c(Tipo, tipo)
    Year <- c(Year,na.omit(unique(data1$year))[f])
    Rango <- c(Rango, rango)
    Specie <- c(Specie, paste("Turdus", " ", unique(data2$Specific_epithet)[i]))
  }
  }
}

  data_final <- data.frame(Specie,Tipo, Year, Rango)
  data_final <- data_final[data_final$Rango!=0,]
  data_final <<- data_final[data_final$Rango!=0,]
  cantos <<- data_final[data_final$Tipo=="Canto",]
  llamados <<- data_final[data_final$Tipo=="Llamado",]
  voca <- merge(cantos,llamados, by=c("Specie","Year"), all=FALSE)
  voc <- reshape2::melt(voca)
  voc <-melt(voca)
  levels(voc$variable) <- c("Canto", "Llamado")
  
  
  boxplot(voc$value~voc$variable, xlab="Tipo de Vocalización"
          , ylab= "Rango temporal (Días)", col=c("#61A0E7","#6DE761"))
  
  print( paste("Su valor de p es:", round(t.test(voc$value~voc$variable, paired=T)$p.value,5)))
  
  }

#Probando la función
temp_bird(turdus)

  
#cantos
cantos_mean<- aggregate(cantos$Rango, list(cantos$Specie), mean)
colnames(cantos_mean)<- c("Specie","Rango")
Lat_cantos<- merge(cantos_mean,data_final_Lat[, c(1,4)], by="Specie", all=FALSE)


#Visualización de los datos

plot(Lat_cantos$Lat_Mid, Lat_cantos$Rango)

hist(Lat_cantos$Lat_Mid)
hist(Lat_cantos$Rango)

#Supuesto normalidad
qqnorm(Lat_cantos$Lat_Mid, col = "grey70", pch = 20, cex= 2)
qqline(Lat_cantos$Lat_Mid, col = "red3", lwd = 2, lty = 5)

qqnorm(Lat_cantos$Rango, col = "grey70", pch = 20, cex= 2)
qqline(Lat_cantos$Rango, col = "red3", lwd = 2, lty = 5)

#Modelo

m0 <- lm(Rango ~ 1, data=Lat_cantos)
m1 <- lm(Rango ~ Lat_Mid, data=Lat_cantos)
summary(m1)
AIC(m0, m1)


plot(abs(Lat_cantos$Lat_Mid), Lat_cantos$Rango)

hist(abs(Lat_cantos$Lat_Mid))

hist(log(abs(Lat_cantos$Lat_Mid)))

Lat_cantos$Lat_Mid <- log(abs(Lat_cantos$Lat_Mid))


hist(Lat_cantos$Lat_Mid)

qqnorm(Lat_cantos$Lat_Mid, col = "grey70", pch = 20, cex= 2)
qqline(Lat_cantos$Lat_Mid, col = "red3", lwd = 2, lty = 5)

m0 <- lm(Rango ~ 1, data=Lat_cantos)
m1 <- lm(Rango ~ Lat_Mid, data=Lat_cantos)

AIC(m0,m1)
summary(m1)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(m1)

##Grafico cantos

newdata <- data.frame(Lat_Mid = seq(min(Lat_cantos$Lat_Mid), max(Lat_cantos$Lat_Mid), l = 1000))
newdata2 <- cbind(newdata, predict(m1, newdata, 
                                   type = "response", 
                                   se.fit = TRUE))

# Objeto con valores predichos por el modelo
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

# Guardar valores predichos en nueva base de datos
newdata3 <- data.frame(newdata2$Lat_Mid,
                       fit, LL, UL)

names(newdata3) <- c("Lat_Mid", "fit", "LL", "UL")

# Grafico 1{

library(ggplot2)

p1 <- ggplot(newdata3, aes(Lat_Mid, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "navyblue", alpha = 0.30) +
  geom_line(colour = "navyblue", size = 1.5, lty = 5) +
  labs(x = "log(Distancia al ecuador(°))", y = "Rango temporal de cantos (días)") +
  theme_classic() +
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = .7),
        axis.line.y = element_line(color="black", size = .7)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  theme(text = element_text(size=12)) 

p1

p1 + geom_point(data = Lat_cantos, aes(Lat_Mid, Rango), 
                shape = 16, color = "red",
                alpha = 0.5,
                size = 3,
                show.legend = F)






##LLamados 

llamados_mean<- aggregate(llamados$Rango, list(llamados$Specie), mean)
colnames(llamados_mean)<- c("Specie","Rango")

Lat_llamados<- merge(llamados_mean,data_final_Lat[, c(1,4)], by="Specie", all=FALSE)

plot(Lat_llamados$Lat_Mid, Lat_llamados$Rango)

hist(Lat_llamados$Rango)

#Supuesto normalidad
qqnorm(Lat_llamados$Lat_Mid, col = "grey70", pch = 20, cex= 2)
qqline(Lat_llamados$Lat_Mid, col = "red3", lwd = 2, lty = 5)

qqnorm(Lat_llamados$Rango, col = "grey70", pch = 20, cex= 2)
qqline(Lat_llamados$Rango, col = "red3", lwd = 2, lty = 5)

#Modelo

m0 <- lm(Rango ~ 1, data=Lat_llamados)
m1 <- lm(Rango ~ Lat_Mid, data=Lat_llamados)

AIC(m0, m1)
summary(m1)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(m1)

##Grafico

newdata <- data.frame(Lat_Mid = seq(min(Lat_llamados$Lat_Mid), max(Lat_llamados$Lat_Mid), l = 1000))
newdata2 <- cbind(newdata, predict(m1, newdata, 
                                   type = "response", 
                                   se.fit = TRUE))



# Objeto con valores predichos por el modelo
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

# Guardar valores predichos en nueva base de datos
newdata3 <- data.frame(newdata2$Lat_Mid,
                       fit, LL, UL)

names(newdata3) <- c("Lat_Mid", "fit", "LL", "UL")

# Grafico llamados

p2 <- ggplot(newdata3, aes(Lat_Mid, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "navyblue", alpha = 0.30) +
  geom_line(colour = "navyblue", size = 1.5, lty = 5) +
  labs(x = "log(Distancia al ecuador(°))", y = "Rango temporal de llamados (días)") +
  theme_classic() +
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = .7),
        axis.line.y = element_line(color="black", size = .7)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  theme(text = element_text(size=12)) 
#+scale_y_continuous(expand = c(0, 0),  breaks = seq(0, 1, 0.25)) 
p2

p2 + geom_point(data = Lat_llamados, aes(Lat_Mid, Rango), 
                shape = 16, color = "red",
                alpha = 0.5,
                size = 3,
                show.legend = F)

