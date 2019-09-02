source('./scripts/funciones_Camilo.R')

load.lib("dplyr", "plotrix", "purrr", "ggplot2")
info <- readRDS('./data/BBVA/data_wrangling/vandalismo/201907-geo_vandalismo.RDS')

nivel <- unique(info[["NIVEL DE PROVISION"]])
per <- 0

for (i in 1:length(nivel)){
  niv <- subset(info,(`NIVEL DE PROVISION` == nivel[i]))
  per[i] <- (nrow(niv)/nrow(info))*100
}

Per_Nivel <- do.call(rbind, Map(data.frame, Nivel=nivel, Porcentaje=per))

fechas <- unique(info[["Mes"]])
per <- 0

for (i in 1:length(fechas)){
  query <- subset(info, (`Mes`== fechas[i]))
  per[i] <- (nrow(query)/nrow(info))*100
}

Per_Fecha <- do.call(rbind, Map(data.frame, Mes=fechas, Porcentaje=per))
Per_Fecha <- Per_Fecha[order(Per_Fecha$Mes),]
Per_Fecha$Mes <- as.Date(Per_Fecha$Mes)

# Pie Chart
png(file = "PieVanMes.jpg")
lbl = c("Sep-18", "Oct-18", "Nov-18", "Dic-18", "Ene-19", "Feb-19", "Mar-19", "Abr-19", "May-19", "Jun-19")
pie3D(Per_Fecha$Porcentaje, labels = lbl, explode = 0.1, main = "Distribución Vandalización por mes")
dev.off()

#Bar Chart
png(file = "BarVanMes.jpg")
lbl = c("Sep-18", "Oct-18", "Nov-18", "Dic-18", "Ene-19", "Feb-19", "Mar-19", "Abr-19", "May-19", "Jun-19")
barplot(Per_Fecha$Porcentaje, names.arg = lbl, main = "Vandalización por mes", ylab = "Porcentaje (sobre vandalizados)", ylim = c(0,13))
dev.off()

#Tabla partes vandalizadas

table1 <- matrix(c(3.10, 0.68, 0.05, 0.52, 93.78, 1.88, 3.52, 0.95, 0.19, 1.03, 94.12, 0.18),ncol=6,byrow=TRUE)
colnames(table1) <- c("Dispensador","Fascia", "Monitor", "Teclados", "Lectora", "Otros")
rownames(table1) <- c("BBVA", "Entidad X")
barplot(table1, main = "Partes vandalizadas", ylab = "Porcentaje (sobre vandalizados)", beside = TRUE, ylim = c(0,100))

#Cajeros vecinos

cajerosCom <- readRDS('./data/BBVA/data_wrangling/cajeros_vecinos/201907_cajeros_vecinos.RDS')
cajeros <- cajerosCom[,c(-2,-1,-3)]

ban <- colnames(cajeros)
Total <- do.call(rbind, Map(data.frame, Banco=ban, Conteo=0))

max <- 0
for (i in 1:nrow(cajeros)){
  max <- sort(cajeros[i,], decreasing = TRUE, partial=4)[1:4]
  pos <- tail(order(cajeros[i,]),4)
  Maximos <- do.call(rbind, Map(data.frame, Posicion=pos, Numero=max))
  if (max[3] != max[4]) {
    Maximos <- Maximos[-4,]
  }
  Maximos <- Maximos[Maximos$Numero != 0, ]
  for (j in Maximos$Posicion){
    Total[j,2] <- Total[j,2] + 1
  }
}

# Gráfica de Top 3
# Frecuencia con la que aparece como uno de los 3 bancos más cercanos a un cajero de BBVA

png(file = "CajerosCercanos.jpg")
ggplot(data=Total, aes(x=reorder(Banco, -Conteo), y=Conteo, fill = Banco)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label= Banco), vjust=0.5, color="black",
            position = position_stack(vjust = 0.5), size=3.5, angle = 90)+
  labs(y = "Frecuencia") +
  annotate("text", label = "Frecuencia - top 3 cajeros cercanos", x=6, y=950, size = 4.5, colour = "darkblue")+
  scale_fill_brewer(palette="Paired")+
  # scale_fill_manual(values = c("Servibanca" = "#339900", "Bancolombia" = "#FFCC33", "Davivienda" = "#CC0000", 
  #                              "Caja_Social" = "#3333FF", "Otros" = "#330066", "Colpatria" = "#FF0000",
  #                              "ATH" = "#0000CC", "Agrario" = "#66FF33", "Falabella" = "#99FF00",
  #                              "Itau" = "#FF9900", "Citibank" = "#0000FF"))
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line = element_line( linetype = "dotted"))
dev.off()

#Entidades con Mayor Número de cajeros cercanos

Sumas <- do.call(rbind, Map(data.frame, Banco=ban, Sum=0))

for (i in 1:ncol(cajeros)){
  Sumas[i,2] <- sum(cajeros[i])  
}

png(file = "CajerosCercanos2.jpg")
ggplot(data=Sumas, aes(x=reorder(Banco, -Sum), y=Sum, fill = Banco)) +
  geom_bar(stat="identity", position=position_dodge())+
  # geom_text(aes(label= Banco), vjust=0.5, color="black",
  #           position = position_stack(vjust = 0.5), size=3.5, angle = 90)+
  labs(y = "Número de cajeros cercanos") +
  annotate("text", label = "Cantidad de cajeros cercanos", x=6, y=7000, size = 5.7, colour = "darkblue")+
  scale_fill_brewer(palette="Paired")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line = element_line( linetype = "dotted"))
dev.off()


