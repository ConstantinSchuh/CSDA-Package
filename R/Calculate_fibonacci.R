#' Calculate Fibonacci Sequence
#'
#' This function generates the Fibonacci sequence up to a specified limit.
#'
#' @param limit An integer specifying the maximum value in the Fibonacci sequence.
#' @return A numeric vector containing the Fibonacci sequence.
#' @examples
#' calculate_fibonacci(10)
#' # Returns: 0, 1, 1, 2, 3, 5, 8
#'
#' @export
calculate_fibonacci <- function(limit) {
  if (limit < 0) {
    stop("The limit must be a non-negative integer.")
  }

  sequence <- c(0, 1)
  while (sequence[length(sequence)] + sequence[length(sequence) - 1] <= limit) {
    sequence <- c(sequence, sequence[length(sequence)] + sequence[length(sequence) - 1])
  }

  return(sequence)
}
