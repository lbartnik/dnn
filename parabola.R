library(tibble)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(plyr)
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
  
  tr <- lapply(tr, function(p) {
    unlist(p[c("w", "b", "Loss")])
  })
  
  as_tibble(do.call(rbind, tr))
}

result <- function(X, Y, w, b) {
  yhat <- vapply(X, function(x) a(x, w, b), numeric(1))
  tibble(x = X, y = Y, yhat) %>%
    pivot_longer(c("y", "yhat")) %>%
    ggplot(aes(x = x, y = value, color = name)) + geom_point()
}

result2 <- function(X, Y, ans) {
  result(X, Y, extract(ans, "w"), extract(ans, "b"))
}

extract <- function(row, what) {
  as.numeric(select(row, starts_with(what)))
}

experiment <- function(X, Y, D = 2, w = rnorm(D), b = rnorm(D), eta = 0.002, epochs = 2000) {
  trace <- optimize(X, Y, w, b, eta, epochs)
  
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

if (FALSE) {
  experiment(X, Y, epochs = 500, w = c(-2, 2))

  experiment(X, Y, epochs = 1000, D = 2, eta = 0.01)
}



grid <- function(X, Y, b, eta=0.01, epochs=250) {
  run_no <- 0
  enumerator <- function() {
    run_no <<- run_no + 1
    run_no
  }
  
  w <- seq(-4, 4, 0.1)
  W <- expand_grid(w1 = w, w2 = w)
  
  W %>%
    alply(1, function(w) {
      trace <- optimize(X, Y, as.numeric(w), b, eta, epochs) %>%
        mutate(stage = 'intermediate',
               i = seq(2, length(stage)+1))
      trace$stage[nrow(trace)] <- 'finish'
      
      bind_rows(
        bind_cols(w, tibble(b1 = b[1], b2 = b[2], stage = 'start', i = 1)),
        trace
      ) %>%
        mutate(no = enumerator())
    }) %>%
    bind_rows
}

if (FALSE) {
  ANS <- grid(X, Y, c(-1, -1))
 
  hist(log(ANS$Loss))
  
  pdf("by_Loss.pdf")
  ANS %>%
    mutate(ll = log(Loss)) %>%
    arrange(ll) %>%
    a_ply(1, function(ans) {
      print(result2(X, Y, ans) + ggtitle(ans$ll))
    })
  dev.off()
  
  ANS %>%
    mutate(ll = log(Loss)) %>%
    ggplot() +
    geom_point(aes(x = w1, y = w2, color = ll))

  ANS %>%
    mutate(ll = log(Loss)) %>%
    ggplot() +
    geom_point(aes(x = b1, y = b2, color = ll))

  classify100 <- function(w1, w2) {
    if (w1 > 3 && w2 < -3) return(1)
    if (w1 < -3 && w2 > 3) return(1)
    if (w1 < -3 && w2 < 1) return(2)
    if (w1 < 1 && w2 < -2) return(2)
    if (w1 > 2 && abs(w2) < .5) return(3)
    if (abs(w1) < .5 && w2 > 2) return(3)
    if (w1 > 2 && w2 > 2) return(4)
    return(5)
  }

  classify250 <- function(w1, w2) {
    if (w1 > 3 && w2 < -3) return(1)
    if (w1 < -3 && w2 > 3) return(1)
    if (w1 < -2 && w2 < -2) return(2)
    if (w1 < -3 && w2 < 1) return(3)
    if (w1 < 1 && w2 < -3) return(3)
    if (w1 > 2 && w2 > 2) return(4)
#    if (w1 > 3 && abs(w2) < .5) return(4)
#    if (abs(w1) < .5 && w2 > 3) return(4)
    return(6)
  }
  
    
  ANS2 <- ANS %>%
    ddply(.(no), function(run) {
      run$group <- classify250(tail(run, 1)$w1, tail(run, 1)$w2)
      run
    }) %>%
    mutate(group = as.factor(group))
    
  
  ANS2 %>%
    filter(stage == 'finish') %>%
    ggplot() +
    geom_point(aes(x = w1, y = w2, color = group))
  
  ANS2 %>%
    as_tibble %>%
    filter(i %% 10 == 0 | stage %in% c("finish", "start")) %>%
    ggplot() +
    geom_point(aes(x = w1, y = w2, color = group, size=stage), alpha = .1) +
    scale_size_manual(values=c(1.3, .05, 1.3)) +
    scale_shape_manual(values = c(21, 16, 2))

  ggsave("trace_250_dense.pdf", width = 12, height = 10)
  
  
}


