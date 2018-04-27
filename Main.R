F <- function(x) {
  F <- sin(x)
}

K <- function(z) {
  if (abs(z) < 1) {
    K <- 0.75*(1-z^2)
  } else {
    K <- 0
  }
}

yk <- function(data, x, h) {
  xe <- sapply((data$X - x)/h, K)
  yk <- sum(xe*data$Y)/sum(xe)
}

n <- 101

x <- seq(from=-3, to=3, length.out=n)
y <- sapply(x, F)
data <- data.frame(X=x, Y=y)
er <- seq(0, 0, length.out=n)
hh <- seq(0, 1, length.out=n)
for (i in 1:n) {
  for (j in 1:n) {
    subdata <- data.frame(X=data$X[-j], Y=data$Y[-j])
    er[i] <- er[i] + (yk(subdata, x[j], hh[i]) - y[j])^2
  }
}
plot(hh, er, col="red", type="l")
