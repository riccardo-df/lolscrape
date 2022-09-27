#' Waiting for Next Iteration (Internal Use)
#'
#' It stops the execution of R for a random number of seconds. Useful for web scarping.
#'
#' @param min Minimum amount of seconds to be waited.
#' @param max Maximum amount of seconds to be waited.
sleep <- function(min = 0, max = 0) {
  ## Handling inputs and checks.
  if (min < 0) stop("Negative time does not make sense.", call. = FALSE)
  if (max < 0) stop("Negative time does not make sense.", call. = FALSE)

  Sys.sleep(stats::runif(1, min = min, max = max))
}
