# ESERCITAZIONI STATISTICA MEDICA MATTEO #

# ESERCITAZIONE 1 #
# ESERCIZIO 1A:
y <- c(29, 42, 20, 36, 23, 23, 32, 19, 40, 23) # vettore di decessi
n <- length(y) # lunghezza vettore (numero di osservazioni)

## Moda
sort(table(y), decreasing = T)
## Media
mean(y)
# Altro modo per calcolare la media
# sum(y)/n
## Mediana
median(y)
# Altro modo per calcolare la mediana (caso numero pari di osservazioni)
# (sort(y)[n/2]+sort(y)[n/2+1])/2
## Varianza
var(y)
# Altro modo per calcolare la varianza
# sum((y-mean(y))^2)/(n-1)
## Deviazione standard
sd(y)
## Coefficiente di variazione
sd(y) / mean(y)

# ESERCIZIO 1B:
## Quartili, min, max e media
summary(y)
## Diagramma quantile
plot(sort(y), ((1:10) - 0.5) / n,
    xlab = "Decessi", ylab = "Rango relativo",
    main = "Diagramma quantile dei decessi",
    type = "b"
)
## Quartili e centili
quantile(y)
quantile(y, probs = seq(0, 1, 0.01))

# ESERCIZIO 2:
df <- data.frame(
    CLASSI <- c("[155.5 - 160.5)", "[160.5 - 165.5)", "[165.5 - 170.5)", "[170.5 - 175.5)", "[175.5 - 180.5)", "[180.5 - 185.5)"),
    VALORE_CENTRALE_CLASSE <- c(158, 163, 168, 173, 178, 183),
    FREQUENZA_ASSOLUTA <- c(10, 13, 15, 23, 20, 9)
)
colnames(df) <- c("CLASSI", "VALORE_CENTRALE_CLASSE", "FREQUENZA_ASSOLUTA")
df$FREQUENZA_RELATIVA <- df$FREQUENZA_ASSOLUTA / sum(df$FREQUENZA_ASSOLUTA)
df$FREQUENZA_RELATIVA_CUMULATA <- cumsum(df[, 4])
media <- weighted.mean(df[, 2], df[, 3])
n <- sum(df[, 3])

## Scarti dalla media al quadrato
(df[, 2] - media)^2
## Scarti pesati per numerosità della classe
((df[, 2] - media)^2) * df[, 3]
## Devianza
devianza <- sum(((df[, 2] - media)^2) * df[, 3])
## Varianza
varianza <- devianza / (n - 1)
## Calcolo della deviazione standard
sqrt(varianza)

## Ogiva
lim_sup <- c(160.5, 165.5, 170.5, 175.5, 180.5, 185.5)
plot(lim_sup, df[, 5],
    type = "b",
    xlab = "Altezza",
    ylab = "Frequenza relativa cumulata"
)
## Poligono di frequenze relative
plot(df[, 2], df[, 4],
    main = "Frequenza relativa dell'altezza",
    type = "b",
    xlab = "Altezza",
    ylab = "Frequenza relativa"
)
