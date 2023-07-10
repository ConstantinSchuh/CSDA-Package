#' Computes the correlation between two vectors
#'
#' This function calculates the correlation between two numeric vectors
#' using the `cor` function in R. It also returns a list that contains both
#' the correlation coefficient and the description of the correlation.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @return A list with the correlation coefficient and the description of the correlation.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 6, 8, 10)
#' Correlation(x, y)
#' # Returns: List(correlation = 1, description = "Positive correlation")

Correlation <- function(x, y) {
  correlation <- cor(x, y)
  if (correlation > 0) {
    description <- "Positive correlation"
  } else if (correlation < 0) {
    description <- "Negative correlation"
  } else {
    description <- "No correlation"
  }
  result <- list(correlation = correlation, description = description)
  return(result)
}



