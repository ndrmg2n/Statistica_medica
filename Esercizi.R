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

# SONO STATI SALTATI GLI ESERCIZI 3 E 4

# ESERCIZIO 5A:
df <- data.frame(
    fumatore <- c(75, 8000, 8075),
    non_fumatore <- c(5, 2750, 2755),
    totale <- c(80, 10750, 10830)
)
colnames(df) <- c("Fumatori", "Non fumatori", "Totale")
rownames(df) <- c("Caso", "Controllo", "Totale")

## Rischio fumatori
r1 <- df[1, 1] / df[3, 1]
## Rischio non fumatori
r2 <- df[1, 2] / df[3, 2]
## Rapporto tra rischi
r1 / r2
## Odds ratio
df[1, 1] * df[2, 2] / (df[1, 2] * df[2, 1])

# ESERCIZIO 5B:
p <- 1
q <- 2 * 80 / 10750
df2 <- df
df2[1, ] <- df[1, ] * p
df2[2, ] <- round(df[2, ] * q)
df2[3, ] <- df2[1, ] + df2[2, ]

################################################

# ESERCITAZIONE 2 #
rm(list = ls())
# ESERCIZIO 1:
## Malati con variante FV Leiden
a <- 14
## Malati senza variante FV Leiden
b <- 179 - a
## Sani con variante FV Leiden
c <- 42
## Sani senza variante FV Leiden
d <- 763 - 42
## Matrice dei dati
mx <- matrix(data = c(a, b, c, d), nrow = 2, byrow = T)
rownames(mx) <- c("Malato", "Sano")
colnames(mx) <- c("Con variante", "Senza variante")
colSums(mx)
rowSums(mx)

## Funzione per inferenza statistica
OR <- function(a, b, c, d) {
    OR <- (a * d) / (b * c)
    se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
    z <- (log(OR) - log(1)) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- exp(log(OR) - 1.96 * se)
    up <- exp(log(OR) + 1.96 * se)

    cat("Odds Ratio =", OR, "\n")
    cat("SE[log(OR)] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI OR = (", lo, "to", up, ")\n")
}
OR(a, b, c, d)

## Malati con variante G202110A
a2 <- 5
## Malati senza variante G202110A
b2 <- 188 - a2
## Sani con variante G202110A
c2 <- 18
## Sani senza variante G202110A
d2 <- 763 - c2

OR(a2, b2, c2, d2)
