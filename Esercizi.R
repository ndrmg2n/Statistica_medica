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
