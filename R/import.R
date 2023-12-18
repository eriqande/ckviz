#### Import the pipe operator from magrittr ####
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @noRd
NULL


#' @importFrom dplyr bind_rows count filter group_by join_by left_join mutate near pull semi_join ungroup
#' @importFrom ggplot2 .pt aes coord_cartesian geom_hline geom_point geom_segment geom_text geom_tile  geom_vline ggplot guides scale_fill_viridis_c scale_linewidth_continuous theme_void
#' @importFrom latex2exp TeX
#' @importFrom purrr map2
#' @importFrom stats rmultinom rnorm runif
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid unnest
NULL



# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      ".",
      ".data",
      ".fill_flag",
      ":=",
      "Eft",
      "Emt",
      "Fec_f",
      "Fec_m",
      "M_f",
      "M_m",
      "Nt",
      "R",
      "S_a",
      "a",
      "a1",
      "a2",
      "arrow_end",
      "arrow_start",
      "cp_S",
      "n",
      "sp",
      "t1",
      "t2"
    )
  )
}
