library(tibble)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(dplyr)

sigma <- function(x) 1/(1+exp(-x))

dsigma <- function(x) {
  s <- sigma(x)
  s*(1-s)
}

a <- function(X, w, b) {
  apply(sigma(outer(X,w) + outer(rep.int(1, length(X)), b)), 1, sum)
}

single <- function(x, y, w, b, eta) {
  yhat <- a(x, w, b)

  L <- (yhat-y)^2
  dLda <- 2*(yhat-y)
  
  dsgm <- dsigma(x*w + b)
  dadw <- dsgm * x
  dadb <- dsgm
  
  dLdw <- dLda * dadw
  dLdb <- dLda * dadb
  
  w <- w - eta * dLdw
  b <- b - eta * dLdb
  
  list(w = w, b = b, Loss = L)
}

epoch <- function(X, Y, w, b, eta) {
  ans <- lapply(seq(length(X)), function(i) {
    ans <- single(X[i], Y[i], w, b, eta)
    w <<- ans$w
    b <<- ans$b
    ans
  })
  tail(ans, 1)
}

optimize <- function(X, Y, w, b, eta, epochs) {
  tr <- list()
  
  for(e in seq(epochs)) {
    et <- epoch(X, Y, w, b, eta)
    w <- last(et)$w
    b <- last(et)$b
    
    tr <- append(tr, et)
  }
  
  lapply(tr, function(p) {
    unlist(p[c("w", "b", "Loss")])
  })
}

result <- function(X, Y, w, b) {
  yhat <- vapply(X, function(x) a(x, w, b), numeric(1))
  tibble(x = X, y = Y, yhat) %>%
    pivot_longer(c("y", "yhat")) %>%
    ggplot(aes(x = x, y = value, color = name)) + geom_point()
}

extract <- function(row, what) {
  as.numeric(select(row, starts_with(what)))
}

experiment <- function(X, Y, D = 2, w = rnorm(D), b = rnorm(D), eta = 0.002, epochs = 2000) {
  trace <- optimize(X, Y, w, b, eta, epochs)
  trace <- as_tibble(do.call(rbind, trace))
  
  ans <- trace[1,]
  p1 <- result(X, Y, extract(ans, "w"), extract(ans, "b"))
  
  ans <- tail(trace, 1)
  p2 <- result(X, Y, extract(ans, "w"), extract(ans, "b"))

  p3 <- trace %>%
    mutate(t = seq_along(w1)) %>%
    pivot_longer(c(starts_with("w"), starts_with("b"))) %>%
    ggplot(aes(x = t, y = value, color = name)) + geom_line()
  
  p4 <- trace %>%
    mutate(t = seq_along(w1)) %>%
    ggplot(aes(x = t, y = Loss)) + geom_line()
  
  
  sx <- seq(min(X), max(X), 0.05)
  sd <- lapply(seq(D), function(d) {
    w <- ans[[paste0("w", d)]]
    b <- ans[[paste0("b", d)]]
    sy <- a(sx, w, b)
    tibble(x = sx, y = sy, d = as.character(d))
  })
  sd <- do.call(rbind, sd)
  p5 <- ggplot(sd, aes(x = x, y = y, color = d)) + geom_line()
  
  grid.arrange(p1, p2, p3, p5, nrow = 2)
}

f <- function(x) x^2
X <- runif(100, -2, 2)
Y <- f(X) + rnorm(length(X), sd=.1)

experiment(X, Y, epochs = 500, w = c(-2, 2))

experiment(X, Y, epochs = 500, D = 10)


