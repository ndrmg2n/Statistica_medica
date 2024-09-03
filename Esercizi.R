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

# ESERCIZIO 2:
rm(list = ls())
## Malati esposti al metilprendisolone (dose elevata)
y1 <- 7
## Malati non esposti al metilprendisolone (dose standard)
y0 <- 13
## Numero non esposti
n0 <- 30
## Numero esposti
n1 <- 30

mx <- matrix(c(y1, y0, n1 - y1, n0 - y0), nrow = 2, byrow = T)
rownames(mx) <- c("Malati", "Sani")
colnames(mx) <- c("Esposti", "Non esposti")

RD <- function(y1, y0, n1, n0) {
    R1 <- y1 / n1
    R0 <- y0 / n0
    RD <- R1 - R0
    se <- sqrt((R1 * (1 - R1)) / n1 + (R0 * (1 - R0)) / n0)
    z <- (RD - 0) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- RD - 1.96 * se
    up <- RD + 1.96 * se

    cat("Risk Difference =", RD, "\n")
    cat("SE[RD] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI RD = (", lo, "to", up, ")\n")
}
RD(y1, y0, n1, n0)

## Malati esposti al metilprendisolone reimmessi al giorno 23 (dose elevata)
y1 <- 7
## Malati non esposti al metilprendisolone reimmessi al giorno 23 (dose standard)
y0 <- 16

RD(y1, y0, n1, n0)

## Malati esposti al metilprendisolone morti (dose elevata)
y1 <- 1
## Malati non esposti al metilprendisolone morti (dose standard)
y0 <- 4

RD(y1, y0, n1, n0)

# ESERCIZIO 3:
rm(list = ls())
df <- data.frame(
    Dilatazione <- c(3.7, 3.9, 2.8, 4.1, 3.4, 4.0, 4.3, 3.6, 4.6, 4.0), # dilatazione utero durante il parto (in cm)
    Trattamento <- c(rep("P", 5), rep("F, 5")) # trattamento o meno ad un farmaco per facilitare la dilatazione
)
colnames(df) <- c("Dilatazione (cm)", "Trattamento")
df

y1 <- df[, 1][df[, 2] == "F"]
y0 <- df[, 1][df[, 2] == "P"]

## Funzione per test-z di una differenza fra medie
MD <- function(y1, y0, type) { # type è una variabile che è "1-group" nel caso di campioni appaiati e "2-group" nel caso di campioni indipendenti
    n1 <- length(y1)
    n0 <- length(y0)
    m1 <- mean(y1)
    m0 <- mean(y0)

    if (type == "2-group") {
        s1 <- var(y1)
        s0 <- var(y0)
        sp <- ((n1 - 1) * s1 + (n0 - 1) * s0) / (n1 + n0 - 2)
    } else {
        sp <- var(y1 - y0) / 2
    }

    MD <- m1 - m0
    se <- sqrt(sp * (1 / n1 + 1 / n0))
    z <- (MD - 0) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- MD - 1.96 * se
    up <- MD + 1.96 * se

    cat("Mean Difference =", MD, "\n")
    cat("SE[MD] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI MD = (", lo, "to", up, ")\n")
}

MD(y1, y0, type = "2-group")

# ESERCIZIO 4:
rm(list = ls())

y0 <- c(39, 24, 14, 31, 26, 28, 44, 29, 29, 30) # piastrine pre trattamento
y1 <- c(44, 35, 20, 28, 23, 39, 47, 37, 30, 30) # piastrine post trattamento

## Funzione per test-z di una differenza fra medie
MD <- function(y1, y0, type) {
    n1 <- length(y1)
    n0 <- length(y0)
    m1 <- mean(y1)
    m0 <- mean(y0)

    if (type == "2-group") {
        s1 <- var(y1)
        s0 <- var(y0)
        sp <- ((n1 - 1) * s1 + (n0 - 1) * s0) / (n1 + n0 - 2)
    } else {
        sp <- var(y1 - y0) / 2
    }

    MD <- m1 - m0
    se <- sqrt(sp * (1 / n1 + 1 / n0))
    z <- (MD - 0) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- MD - 1.96 * se
    up <- MD + 1.96 * se

    cat("Mean Difference =", MD, "\n")
    cat("SE[MD] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI MD = (", lo, "to", up, ")\n")
}

MD(y1, y0, type = "1-group")

# ESERCIZIO 5:
rm(list = ls())

# y: casi di varicella
# M: # massa degli utilizzatori (persone-anno)
y1 <- 5 + 14 # casi di varicella tra chi usa steroidi
y0 <- 252 # casi di varicella tra chi non usa steroidi
M1 <- 3269 + 3133
M0 <- 136272
mx <- matrix(c(y1, y0, M1, M0), nrow = 2, byrow = T)
rownames(mx) <- c("Casi di varicella", "Persone-anno")
colnames(mx) <- c("Uso steroidi", "Non uso steroidi")

rr <- function(y1, y0, M1, M0) {
    r1 <- y1 / M1
    r0 <- y0 / M0
    rr <- r1 / r0
    se <- sqrt(1 / y1 + 1 / y0)
    z <- (log(rr) - log(1)) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- exp(log(rr) - 1.96 * se)
    up <- exp(log(rr) + 1.96 * se)

    cat("rate ratio =", rr, "\n")
    cat("SE[rr] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI rr = (", lo, "to", up, ")\n")
}

rr(y1, y0, M1, M0)

## Rate ratio tra utilizzanti di steroidi inalati e orali
y1 <- 5
y0 <- 14
M1 <- 3269
M0 <- 3133
rr(y1, y0, M1, M0)

# ESERCIZIO 6:
## Esito negativo prima e dopo il trattamento
e <- 7
## Esito positivo prima e negativo dopo il trattamento
f <- 5
## Esito negativo prima e positivo dopo il trattamento
g <- 15
## Esito positivo prima e dopo il trattamento
h <- 5
mx <- matrix(c(e, f, g, h), nrow = 2, byrow = T)
colnames(mx) <- c("Prima -", "Prima +")
rownames(mx) <- c("Dopo -", "Dopo +")

RDa <- function(e, f, g, h) {
    n <- e + f + g + h
    R1 <- (e + f) / n
    R0 <- (e + g) / n
    RD <- R1 - R0
    se <- sqrt((f + g) / n^2)
    z <- (RD - 0) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- RD - 1.96 * se
    up <- RD + 1.96 * se

    cat("Risk Difference =", RD, "\n")
    cat("SE[RD] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI RD = (", lo, "to", up, ")\n")
}
RDa(e, f, g, h)

################################################

# ESERCITAZIONE 3 #
rm(list = ls())
# ESERCIZIO 1:
install.packages("mefa")
library(mefa)

dati1 <- read.csv2("diet.csv", header = T, sep = ",")
colnames(dati1) <- c("carbohydrate", colnames(dati1)[-1])
summary(dati1)
## Variabile dipendente
y <- dati1$carbohydrate
## Regressori
x1 <- dati1$age
x2 <- dati1$weight
x3 <- dati1$protein
x4 <- dati1$regime - 1

formula <- reformulate(colnames(dati1)[-1], colnames(dati1)[1])
fit1 <- lm(formula, data = dati1)
summary(fit1)

confint.default(fit1)

# ESERCIZIO 2:
dati2 <- read.csv("stroke_in_young.csv", header = T, sep = ",")
colnames(dati2)[1] <- "id"
summary(dati2)

## Modello A
dati2_ <- dati2
dati2_[, 10] <- as.factor(dati2[, 10])
dati2_[, 11] <- as.factor(dati2[, 11])
dati2_[, 12] <- as.factor(dati2[, 12])
dati2_[, 13] <- as.factor(dati2[, 13])
formula <- reformulate(colnames(dati2_)[10:13], colnames(dati2_)[2])

y <- dati2$cc
x.fattore5 <- as.factor(dati2$v)
x.fattore2 <- as.factor(dati2$ii)
x.mthfr <- as.factor(dati2$mthfr)
x.apoe <- as.factor(dati2$apoe)

M1 <- glm(formula, data = dati2_, family = binomial(link = "logit"))
summary(M1)
exp(summary(M1)$coefficients[, 1])

## Modello B
dati2_[, 10] <- as.numeric(dati2[, 10])
dati2_[, 11] <- as.numeric(dati2[, 11])
dati2_[, 12] <- as.numeric(dati2[, 12])
dati2_[, 13] <- as.numeric(dati2[, 13])
formula <- reformulate(colnames(dati2_)[10:13], colnames(dati2_)[2])

M1 <- glm(formula, data = dati2_, family = binomial(link = "logit"))
summary(M1)
exp(summary(M1)$coefficients[, 1])

## Modello C
dati2_[, 10] <- 1 * (dati2[, 10] > 1)
dati2_[, 11] <- 1 * (dati2[, 11] > 1)
dati2_[, 12] <- 1 * (dati2[, 12] > 2)
dati2_[, 13] <- 1 * (dati2[, 13] > 1)
formula <- reformulate(colnames(dati2_)[10:13], colnames(dati2_)[2])

M1 <- glm(formula, data = dati2_, family = binomial(link = "logit"))
summary(M1)
exp(summary(M1)$coefficients[, 1])

## Modello D
GS <- dati2_[, 10] + dati2_[, 11] + dati2_[, 12] + dati2_[, 13] # numero di varianti a rischio (es. 1 = una variante a rischio)
formula <- reformulate("GS", colnames(dati2_)[2])

M1 <- glm(formula, data = dati2_, family = binomial(link = "logit"))
summary(M1)
exp(summary(M1)$coefficients[, 1])

## Modello E
dati2_[, 3] <- as.factor(dati2[, 3])
dati2_[, 6] <- as.factor(dati2[, 6])
dati2_[, 7] <- as.factor(dati2[, 7])
dati2_[, 8] <- as.factor(dati2[, 8])
dati2_[, 9] <- as.factor(dati2[, 9])

formula <- reformulate(c("GS", colnames(dati2_)[c(3:9)]), colnames(dati2_)[2])

M1 <- glm(formula, data = dati2_, family = binomial(link = "logit"))
summary(M1)
exp(summary(M1)$coefficients[, 1])

# ESERCIZIO 3:
rm(list = ls())

dati3 <- read.csv2("pfo.csv", header = T, sep = ",")
dati3[, c(4:9, 11)] <- dati3[, c(4:9, 11)] - 1

t <- dati3$follow_up_time
d <- dati3$recurrences
x <- as.factor(dati3$PFO)

## Stima del rischio istantaneo col metodo Kaplan-Meier
install.packages("survival")
library(survival)
surv_obj <- Surv(t, d)
KM <- survfit(surv_obj ~ x)
plot(surv_obj)

## Fine esercizio perché non va la funzione "survfit"

# ESERCITAZIONE 4 #
rm(list = ls())
source("functions.R")
# ESERCIZIO 1:
mx <- matrix(c(1, 1, 1, 6, 1, 1, 0, 30, 1, 0, 1, 8, 1, 0, 0, 19, 0, 1, 1, 4, 0, 1, 0, 20, 0, 0, 1, 9, 0, 0, 0, 24), byrow = T, ncol = 4)
colnames(mx) <- c("z", "x", "y", "count")
## X: alcolista / non alcolista
## Z: fumo / non fumo
## Y: tumore alla bocca / non tumore alla bocca


# Z X    | Y+| Y-
#-------|---|---
# Z+X+   | 6 | 30          "+" = 1   "-" = 0
# Z+X-   | 8 | 19
# Z-X+   | 4 | 20
# Z-X-   | 9 | 24

## Valutazione del confodimento
## Step 1: valutazione dell'associazione tra X ed Y

#    | X+| X-
# ---|---|---
#  Y+| 10| 17
#  Y-| 50| 43

## Malati (y = 1) alcolisti (x = 1) ## Casi esposti
y1 <- 10
## Malati non alcolisti (y = 1, x = 0) ## Casi non esposti
y0 <- 17
## Totale esposti
n1 <- 10 + 50
## Totale non esposti
n0 <- 17 + 43

RR(y1, y0, n1, n0)

## Step 2: valutazione dell'associazione tra X e Z

#    | X+| X-
# ---|---|---
#  Z+| 36| 27
#  Z-| 24| 33

## Alcolisti (x = 1) fumatori (z = 1)
y1 <- 36
## Alcolisti non fumatori (x = 1, z = 0)
y0 <- 24
## Totale esposti
n1 <- 36 + 27
## Totale non esposti
n0 <- 24 + 33

RR(y1, y0, n1, n0)

## Step 3: valutazione dell'effetto di X e Z su Y
library(mefa)

z <- mx[, 1]
x <- mx[, 2]
y <- mx[, 3]
count <- mx[, 4]

df <- rep(data.frame(z, x, y), times = count)

formula <- reformulate(colnames(df)[1:2], colnames(df)[3])
fit1 <- glm(formula, data = df, family = binomial(link = "log"))
summary(fit1)
exp(summary(fit1)$coefficients)
exp(confint.default(fit1))

# ESERCIZIO 2:
rm(list = ls())
source("functions.R")

# Z X    | Y+| Y-
# -------|---|---
# Z+X+   | 48| 14          "+" = 1   "-" = 0
# Z+X-   | 39| 78
# Z-X+   | 77| 38
# Z-X-   | 78|105

## X: allele e4 di APOE
## Z: allele T di TNF-a
## Y: alzheimer

## Valutazione della moderazione
## Step 1: valutazione dell'associazione tra X ed Y

#    | X+| X-
# ---|---|---
#  Y+|125|117
#  Y-| 52|183

## Malati (y = 1) con l'allele e4 di APOE (x = 1) ## Casi esposti
a <- 125
## Malati senza l'allele e4 di APOE (y = 1, x = 0) ## Casi non esposti
b <- 117
## Sani esposti
c <- 52
## Sani non esposti
d <- 183

OR(a, b, c, d)

## Step 2: valutazione dell'associazione tra X ed Z

#    | Z+| Z-
# ---|---|---
#  X+| 62|115
#  X-|117|183

## Allele e4 di APOE (x = 1) con l'allele T di TNF-a (z = 1)
a <- 62
## Allele e4 di APOE senza l'allele T di TNF-a (x = 1, z = 0)
b <- 115
## Senza allele e4 di APOE con l'allele T di TNF-a
c <- 117
## Senza allele e4 di APOE e l'allele T di TNF-a
d <- 183

OR(a, b, c, d)

## Step 3: valutazione dell'effetto di X e Z su Y
z <- c(rep(1, 4), rep(0, 4))
x <- c(rep(c(1, 1, 0, 0), 2))
y <- c(rep(c(1, 0), 4))
count <- c(48, 13, 39, 78, 77, 38, 78, 105)
df <- rep(data.frame(z, x, y), times = count)

formula <- reformulate(c(colnames(df)[1:2], "x:z"), colnames(df)[3])
fit1 <- glm(formula, data = df, family = binomial(link = "logit"))
summary(fit1)
