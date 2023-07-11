#' Polynomial Regression
#'
#' This function performs polynomial regression on a given dataset, fitting a polynomial model of specified degree.
#'
#' @param x A numeric vector of predictor variable values.
#' @param y A numeric vector of response variable values.
#' @param degree The degree of the polynomial model to fit.
#' @return A list containing the polynomial coefficients and the predicted values.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 6, 8, 10)
#' result <- Polynomial_regression(x, y, degree = 2)
#' result$coefficients
#' # Expected Output: (Intercept)   predictors1   predictors2
#' #                  0.000000e+00  2.000000e+00 -1.718126e-16
#' result$predicted_values
#' # Expected Output: 1  2  3  4  5
#' #                  2  4  6  8 10
#'
#' @export
Polynomial_regression <- function(x, y, degree) {
  if (length(x) != length(y)) {
    stop("The lengths of x and y must be equal.")
  }

  if (degree < 1) {
    stop("The degree must be a positive integer.")
  }

  # Create matrix of predictors
  predictors <- poly(x, degree, raw = TRUE)

  # Fit polynomial regression model
  model <- lm(y ~ predictors)

  # Extract coefficients
  coefficients <- coef(model)

  # Predict values
  predicted_values <- predict(model, newdata = data.frame(x))

  result <- list(coefficients = coefficients, predicted_values = predicted_values)
  return(result)
}
