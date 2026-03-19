#' My first function
#'
#' @param a A numerical vector.
#' @param b Also a numerical vector.
#'
#' @return A numerical vector of a + b * a.
#' @export
#'
#' @examples
#' myfunc(3, 5)
myfunc <- function(a, b) {
  result <- a * b + a
  return(result)
}

#' My first function
#'
#' @param y A numerical column vector.
#' @param X Also a numerical matrix.
#'
#' @return A numerical vector of estimated coefficients using OLS as the estimator.
#' @export
#'
#' @examples
#' estimate_beta(y = mtcars$mpg, X = mtcars[,c("hp","wt")])
estimate_beta <- function(y, X) {
  # Making sure y is a column vector
  if (is.vector(y)) {
    y <- matrix(y, ncol = 1)
  } else {
    y <- as.matrix(y)
  }

  # Coerce X to matrix
  X <- as.matrix(X)

  # Checks
  if (!is.numeric(y) || !is.numeric(X)) {
    stop("Both y and X must be numeric.")
  }
  if (ncol(y) != 1) {
    stop("y must be n x 1.")
  }
  if (nrow(y) != nrow(X)) {
    stop("y and X must have the same number of rows.")
  }

  XtX <- t(X) %*% X
  Xty <- t(X) %*% y

  # Better than det()==0 for numerical stability
  if (rcond(XtX) < .Machine$double.eps) {
    stop("X'X is singular or near-singular (multicollinearity).")
  }

  beta_hat <- solve(XtX) %*% Xty
  return(beta_hat)
}


brunonian_theme <- function() {
  theme(plot.background = ggplot2::element_rect(fill = "#f7f3e9", color = NA),
      panel.background = ggplot2::element_rect(fill = "#fffaf0", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#d8c9a8", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank()) 
}
