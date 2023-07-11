#' Matrix Inversion
#'
#' This function calculates the inverse of a square matrix using the Gauss-Jordan elimination method.
#'
#' @param matrix A square matrix.
#' @return The inverse of the input matrix.
#' @examples
#' matrix <- matrix(c(2, 3, 1, 4), nrow = 2)
#' inverse_matrix <- calculate_matrix_inverse(matrix)
#' # Returns:  0.8 -0.2
#' #          -0.6  0.4
#'
#' @export
calculate_matrix_inverse <- function(matrix) {
  if (!is.matrix(matrix) || nrow(matrix) != ncol(matrix)) {
    stop("The input must be a square matrix.")
  }

  n <- nrow(matrix)
  augmented_matrix <- cbind(matrix, diag(n))

  for (i in 1:n) {
    pivot_row <- augmented_matrix[i, ]
    pivot <- pivot_row[i]

    if (pivot == 0) {
      stop("The matrix is not invertible.")
    }

    augmented_matrix[i, ] <- pivot_row / pivot

    for (j in 1:n) {
      if (j != i) {
        factor <- augmented_matrix[j, i]
        augmented_matrix[j, ] <- augmented_matrix[j, ] - factor * augmented_matrix[i, ]
      }
    }
  }

  inverse <- augmented_matrix[, (n + 1):(2 * n)]
  return(inverse)
}
