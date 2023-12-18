#' returns a sample set of vital rates
#'
#' Super simple.  Primarily here to show how someone
#' could create a set of vitals in a tibble
#' @export
example_vitals <- function() {
  tibble(
    a = 1:9,
    S_a = c(0.3, 0.7, 0.8, 0.9, 0.9, 0.95, 0.95, 0.9, 0.0),
    M_f = c(0.0, 0.0, 0.5, 0.8, rep(1.0, 4), 0.0),
    M_m = c(0.2, 0.7, rep(1.0, 6), 0.0),
    Fec_f = c(0, 0, 1000, 2000, 3000*(2:5), 0.0),
    Fec_m = c(seq(100, 300, length.out = 8), 0.0)
  )
}
