# Datos higrologicos ejercicio 2

inp <- read.csv("FDC.csv", na.strings = "")

head(inp)
View(inp)
## Gráfico Histograma de los r?os Estrella y Banana, volumen del agua y tiempo
plot(
  inp[,2],
  type = "l", col="brown",
  main = ("Hidrograma de los ríos Estrella y Banano"),
  xlab = ("Tiempo"),
  ylab = ("Nivel del caudal")
)
  lines(inp[,3], col= "pink")
  
  
summary(inp[,2:3])

hist(inp[,2])
hist(inp[,3])

Tempodate <-strptime(inp[,1], format= "%d/%m/%Y")

### Gráfico de barras conteo de agua por día y nivel del caudal
plot(
  hist(inp[,2]),
  main = ("Histograma de Estrella"),
  height = inp$Estrella,
  names.arg = inp$fecha,
  xlab = ("Agua por d?a"),
  ylab = ("Nivel del caudal"),
  col = ("Orange")
)
plot(
  hist(inp[,3]),
  main = ("Histograma de Banano"),
  height = inp$Banano,
  names.arg = inp$fecha,
  xlab = ("Agua por d?a"),
  ylab = ("Nivel del caudal"),
  col = ("pink")
)

names(inp) <- c("fecha", "Estrella","Banano")
attach(inp)
plot(Estrella)

#### Gr�fico de puntos asociados directamente al nombre del r�o
plot(
  inp$Estrella,
  main = ("Rio Estrella"),
  xlab = ("Tiempo"),
  ylab = ("Nivel del caudal"),
  col = "Pink"
)
plot(
  inp$Banano,
  main = ("Rio Banano"),
  xlab = ("Tiempo"),
  ylab = ("Nivel del caudal"),
  col = "Yellow"
)

Tempodate <- strptime(inp[,1], format= "%d/%m/%Y")

MaQ_Estrella <- tapply(inp[,2], format(Tempdate, format="%Y"), FUN=sum)
MaQ_Banano <- tapply(inp[,3], format(Tempdate, format="%Y"), FUN=sum)
write.csv(MaQ_Estrella, file="MaQ.csv")


plot(MaQ_Banano,
     ylim = c(100,3000),
     xlab = ("Tiempo"),
     ylab = ("Nivel de caudal"),
     main = ("Comparaci?n de niveles de caudal de los r?os en Banano y Estrella del a?o 1963 al 1983 "),
     col= "blue"
)
lines (MaQ_Estrella, col= "red")

MaQ_Estrella <- tapply(Estrella, format(Tempdate, format="%m"),FUN= sum)
MaQ_Banano <- tapply(Estrella, format(Tempdate, format="%m"),FUN= sum)

corinp <- cor(inp[,2:3], method= "spearman")
corinp

### Gr?fico del caudal de r?os uno contra otro
plot(
  inp$Estrella,
  inp$Banano,
  xlab = ("Estrella"),
  ylab = ("Banano"),
  main = ("Relaci?n del caudal de los r?os Banano y Estrella"),
  col= "pink"
)
inp.lm <- lm(inp[,2]~inp[,3], data=inp)
summary(inp.lm)


