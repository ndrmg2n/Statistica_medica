# ODDS RATIO
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

# RATE DIFFERENCE
rd <- function(y1, y0, M1, M0) {
    r1 <- y1 / M1
    r0 <- y0 / M0
    rd <- r1 - r0
    se <- sqrt(r1 / M1 + r0 / M0)
    z <- (rd - 0) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- rd - 1.96 * se
    up <- rd + 1.96 * se

    cat("Rate Difference =", rd, "\n")
    cat("SE[rd] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI rd = (", lo, "to", up, ")\n")
}

# RISK RATIO
RR <- function(y1, y0, n1, n0) {
    R1 <- y1 / n1
    R0 <- y0 / n0
    RR <- R1 / R0
    se <- sqrt((1 - R1) / y1 + (1 - R0) / y0)
    z <- (log(RR) - log(1)) / se
    p <- 2 * (1 - pnorm(abs(z)))

    lo <- exp(log(RR) - 1.96 * se)
    up <- exp(log(RR) + 1.96 * se)

    cat("Risk Ratio =", RR, "\n")
    cat("SE[RR] =", se, "\n")
    cat("z-test =", z, "\n")
    cat("P(Z-sided) =", p, "\n")

    cat("95%CI RR = (", lo, "to", up, ")\n")
}

# RISK DIFFERENCE
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

# MEAN DIFFERENCE
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

# RATE RATIO
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

# RISK DIFFERENCE (dati appaiati)
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
