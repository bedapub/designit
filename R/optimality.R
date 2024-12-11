d_eff <- function(m) {
  if (!is.matrix(m)) m <- as.matrix(m)
  p <- ncol(m)
  n <- nrow(m)
  # Calculate information matrix
  infomat <- t(m) %*% m
  # Calculate determinant of information matrix
  det_calc <- det(infomat)
  # Set determinants equal to zero if less than zero due to R not being able to precisely calculate determinant
  if (det_calc < 0) {det_calc <- 0}
  # Calculate D-efficiency
  100 * ((det_calc)^(1 / p)) / n
}

a_eff <- function(m) {
  if (!is.matrix(m)) m <- as.matrix(m)
  p <- ncol(m)
  n <- nrow(m)
  # Calculate information matrix
  infomat <- t(m) %*% m
  # Calculate inverse
  inv <- MASS::ginv(infomat)
  # Trace
  trc <- sum(diag(inv), na.rm = TRUE)
  # Calculate A-optimality
  100 * ((p / n) / trc)
}

g_eff <- function(m) {
  if (!is.matrix(m)) m <- as.matrix(m)
  p <- ncol(m)
  n <- nrow(m)
  # Calculate information matrix
  infomat <- t(m) %*% m
  # Calculate hat matrix
  hat <- m %*% MASS::ginv(infomat) %*% t(m)
  # Calculate G-optimality
  100 * sqrt((p/n) / max(diag(hat)))
}
