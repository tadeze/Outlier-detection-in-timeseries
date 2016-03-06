innov_outlier_effect <- function (ar, ma, lag) {
  ar <- ifelse(rep(is.null(ar), lag), rep(0, lag), c(ar, rep(0, lag - length(ar))))
  ma <- ifelse(rep(is.null(ma), lag), rep(0, lag), c(ma, rep(0, lag - length(ma))))
  effect <- numeric(lag)
  effect[1] <- 1
  for (k in 1:(lag - 1)) {
    effect[1 + k] <- ma[k]
    for (j in 1:k) {
      effect[1 + k] <- effect[1 + k] + ar[j] * effect[1 + k - j]
    }
  }
  effect
}

par(mfrow = c(3, 2))

lag <- 10
k <- 0:(lag - 1)

ar <- 0.7
plot(k, innov_outlier_effect(ar, NULL, 10), type = 'l', lty = 2,
     ylim = c(-1, 1), xlab = "Lag", ylab = "Outlier effect", main = "AR(1)")
ar <- 0.2
lines(k, innov_outlier_effect(ar, NULL, 10), lty = 3)
ar <- -0.7
lines(k, innov_outlier_effect(ar, NULL, 10), lty = 4)
abline(h = 0)

ar <- c(-0.4, -0.2, 0.048)
plot(k, innov_outlier_effect(ar, NULL, 10), type = 'l', lty = 2,
     ylim = c(-1.2, 1.2), xlab = "Lag", ylab = "Outlier effect", main = "AR(3)")
ar <- c(0.4, 0.2, 0.048)
lines(k, innov_outlier_effect(ar, NULL, 10), lty = 3)
ar <- c(-1.2, -0.44, -0.048)
lines(k, innov_outlier_effect(ar, NULL, 10), lty = 4)
abline(h = 0)

lag <- 5
k <- 0:(lag - 1)

ma <- 0.7
plot(k, innov_outlier_effect(NULL, ma, 5), type = 'l', lty = 2,
     ylim = c(-1, 1), xlab = "Lag", ylab = "Outlier effect", main = "MA(1)")
ma <- 0.2
lines(k, innov_outlier_effect(NULL, ma, 5), lty = 3)
ma <- -0.7
lines(k, innov_outlier_effect(NULL, ma, 5), lty = 4)
abline(h = 0)

ma <- c(-0.4, -0.2, 0.048)
plot(k, innov_outlier_effect(NULL, ma, 5), type = 'l', lty = 2,
     ylim = c(-1.2, 1.2), xlab = "Lag", ylab = "Outlier effect", main = "MA(3)")
ma <- c(0.4, 0.2, 0.048)
lines(k, innov_outlier_effect(NULL, ma, 5), lty = 3)
ma <- c(-1.2, -0.44, -0.048)
lines(k, innov_outlier_effect(NULL, ma, 5), lty = 4)
abline(h = 0)

lag <- 10
k <- 0:(lag - 1)

ar <- 0.2
ma <- 0.5
plot(k, innov_outlier_effect(ar, ma, 10), type = 'l', lty = 2,
     ylim = c(-1, 1), xlab = "Lag", ylab = "Outlier effect", main = "ARMA(1,1)")
ar <- -0.7
lines(k, innov_outlier_effect(ar, ma, 10), lty = 3)
ma <- -0.3
lines(k, innov_outlier_effect(ar, ma, 10), lty = 4)
abline(h = 0)

lag <- 10
k <- 0:(lag - 1)

ar <- c(-0.4, -0.2, 0.048)
ma <- c(-0.4, -0.2, 0.048)
plot(k, innov_outlier_effect(ar, ma, 10), type = 'l', lty = 2,
     ylim = c(-1, 1), xlab = "Lag", ylab = "Outlier effect", main = "ARMA(3,3)")
ar <- c(0.4, 0.2, 0.048)
lines(k, innov_outlier_effect(ar, ma, 10), lty = 3)
ma <- c(-1.2, -0.44, -0.048)
lines(k, innov_outlier_effect(ar, ma, 10), lty = 4)
abline(h = 0)