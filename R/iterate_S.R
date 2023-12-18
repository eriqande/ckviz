#' Iterate age structured survival from random recruitment
#'
#' This is a ridiculously simple function to iterate some age structured survival
#' that assumes a random input to age-0 each year.  It also inserts a little
#' extra randomness in for good measure.
#' @param S the vector of survival probs for surviving
#' to age 1,...,length(S). The last element will be treated
#' as 0.
#' @param T the number of years to simulate forward.
#' @param N0min the mininum N0 each year
#' @param N0max the maximum N0 each year.  (it is drawn from a uniform)
#' @param ASZ_size when setting up the age structure in the first year, the
#' values are drawn from a multinomial of this size
#' @param S_var_denom  random annual variation in survival is drawn from a normal
#' distribution centered on S with standard deviation S/S_var_denom.  This must be
#' large enough that you don't get values outside of 0 or 1.
#' @return returns a matrix that is A rows by T columns.
#' @export
iterate_S <- function(
    S,
    T,
    N0min = 8000,
    N0max = 10000,
    ASZ_size = 500,
    S_var_denom = 30
  ) {
  A <- length(S) - 1
  St <- S[1:A]
  N0 <- runif(T, min = N0min, max = N0max)
  normo <- function(x) {x / sum(x)}

  ret <- matrix(NA, nrow = A, ncol = T)  # make a matrix to return

  # age structure in year one is drawn from a multinomial with size ASZ_size and
  # scaled up a bit.
  ret[,1] <- N0[1] * normo(rmultinom(n = 1, size = ASZ_size, prob = cumprod(St))[,1])

  # for each remaining year, the age structure is found by introducing some error
  # for that year to the age-specific survival rates and then surviving individuals
  # from the previous year (and a year younger) forward to this year.
  for(i in 2:T) {
    ss <- St + rnorm(n = A, sd = St / S_var_denom)
    ret[1,i] <- N0[i] * ss[1]
    ret[-1, i] <- ret[-nrow(ret), i - 1] * ss[-1]
  }
  ret
}
