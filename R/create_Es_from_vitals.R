

#' Make an Es tibble from the vitals
#'
#' The Es tibble has the number of males and females of each age at each time
#' and then it also has colums that give the relative age composition each year,
#' and also the ERRO for each cell from that (using the fecundities).  It also
#' includes the columns that are used to print the mathematical expressions for
#' each.
#' @param vitals  The tibble of vital rates, like that from `example_vitals()`.
#' @param T The number of years of data to have
#' @export
create_Es_from_vitals <- function(
  vitals = example_vitals(),
  T = 15
) {

  A <- nrow(vitals) - 1

  ret <- expand_grid(t = 1:T, a = 1:A) %>%
    mutate(N = sprintf("$N_{s,%d,%d}$", t, a)) %>%
    mutate(  # get the age specific abundances.  Assumed the same for males and females
             # because their survival rates are the same
      Nt = as.vector(iterate_S(vitals$S_a, T))
    ) %>%
    group_by(t) %>%
    mutate(  # get the age specific relative abundances for each year
      R = Nt / sum(Nt),
      Rlab = sprintf("$R_{s,%d,%d}$", t, a)
    ) %>%
    left_join(vitals, by = "a") %>%  # everyting below here is getting the ERRO for males and females.
                                     # these differ for males and females because of different repro schedules.
    mutate(
      Em_str = sprintf("$E_{m,%d,%d}$", t, a),
      Ef_str = sprintf("$E_{f,%d,%d}$", t, a),
    ) %>%
    group_by(t) %>%
    mutate(
      Emt = R * M_m * Fec_m / sum(R * M_m * Fec_m),
      Eft = R * M_f * Fec_f / sum(R * M_f * Fec_f),
      Emt = ifelse(near(Emt, 0), NA, Emt),
      Eft = ifelse(near(Eft, 0), NA, Eft)
    ) %>%
    ungroup()

  ret

}
