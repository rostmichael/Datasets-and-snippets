rm(list = ls())

data <- c(50, 88, 155, 379, 11, 63, 43, 62, 110, 300, 14, 144)
X <- matrix(data, ncol = 2)
X
colnames(X) <- c("Drinkers", "Nondrinkers")
rownames(X) <- c("Arson", "Rape", "Violence", "Theft", "Coining", "Fraud")
X


plot.cont2 <- function(X, alpha) {
  propo <- prop.table(X, 1)
  pr <- propo[, 1]
  soucet <- apply(X, 1, sum)
  par(mar = c(5, 4, 4, 4) + 0.1)
  gra <-
    barplot(
      pr,
      ylim = c(0, 1),
      col = c("darkgreen"),
      space = 0.85,
      ylab = "Relativní četnost",
      las = 1
    )
  par(new = TRUE)
  abline(h = 0)
  axis(
    side = 4,
    at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    labels = c(0, 20, 40, 60, 80, 100),
    las = 1
  )
  mtext("Procenta",
        side = 4,
        col = "black",
        line = 2)
  half <- qnorm(alpha / 2) * sqrt(pr * (1 - pr) / soucet)
  error.bar <-
    function(x,
             pr,
             upper = half,
             lower = half,
             length = 0.05,
             lwd = 1.5,
             ...) {
      if (length(x) != length(pr) |
          length(pr) != length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
      arrows(
        x,
        pr + upper,
        x,
        pr - lower,
        angle = 90,
        code = 3,
        length = length,
        ...
      )
    }
  error.bar(gra, pr, half)
}

plot.cont2(X, 0.05)



plot.c2 <- function(X, alpha = 0.05, col = "darkgreen") {
  pr <- prop.table(X)
  soucet <- sum(X)
  par(mar = c(5, 4, 4, 4) + 0.1)
  gra <-
    barplot(
      pr,
      ylim = c(0, 1),
      col = col,
      space = 0.85,
      xlab = "Categories",
      ylab = "Relative frequency",
      las = 1
    )
  par(new = TRUE)
  abline(h = 0)
  axis(
    side = 4,
    at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    labels = c(0, 20, 40, 60, 80, 100),
    las = 1
  )
  mtext("in %",
        side = 4,
        col = "black",
        line = 2)
  half <- qnorm(alpha / 2) * sqrt(pr * (1 - pr) / soucet)
  error.bar <-
    function(x,
             pr,
             upper = half,
             lower = half,
             length = 0.05,
             lwd = 1.5,
             ...) {
      if (length(x) != length(pr) |
          length(pr) != length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
      arrows(
        x,
        pr + upper,
        x,
        pr - lower,
        angle = 90,
        code = 3,
        length = length,
        ...
      )
    }
  error.bar(gra, pr, half)
}


X <- c(180, 120, 107, 51, 24)
names(X) <- LETTERS[1:5]
X

plot.c2(X, 0.05)




barswithci <- function(X, alpha = 0.05, col = "darkgreen") {
  pr <- prop.table(X)
  soucet <- sum(X)
  par(mar = c(5, 4, 4, 4) + 0.1)
  gra <-
    barplot(
      pr,
      ylim = c(0, 1),
      col = col,
      space = 0.85,
      xlab = "Categories",
      ylab = "Relative frequency",
      las = 1
    )
  par(new = TRUE)
  abline(h = 0)
  axis(
    side = 4,
    at = c(0, pr, 100),
    labels = c(0, round(pr * 100, 2), 100),
    las = 1
  )
  mtext("in %",
        side = 4,
        col = "black",
        line = 2)
  half <- qnorm(alpha / 2) * sqrt(pr * (1 - pr) / soucet)
  error.bar <-
    function(x,
             pr,
             upper = half,
             lower = half,
             length = 0.05,
             lwd = 1.5,
             ...) {
      if (length(x) != length(pr) |
          length(pr) != length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
      arrows(
        x,
        pr + upper,
        x,
        pr - lower,
        angle = 90,
        code = 3,
        length = length,
        ...
      )
    }
  error.bar(gra, pr, half)
}

X <- c(180, 120, 107, 51, 24)
names(X) <- LETTERS[1:5]

X

plot.c2(X, 0.05)
