#' update an Es tibble to have what it needs to draw survival arrows
#'
#' This is an updated version of cum_surv_probs that lets the user degine
#' start and end points for any of the survival segments.
#'
#' The function checks to make sure they are valid (i.e. that
#' they are within the bounds and have a slope of 1).
#' @param E the tibble of Ns and ERROs, etc.
#' @param P a tibble with t1, a1, t2, a2 giving the start and end
#' points of the segments
#' @examples
#'
#' pts <- tibble::tibble(
#'  t1 = c(5, 5),
#'  a1 = c(1, 2),
#'  t2 = c(11, 11),
#'  a2 = c(7, 8)
#' )
#'
#'
make_survival_segpoints <- function(E, P) {
  max_a <- max(E$a)
  max_t <- max(E$t)

  # catch some simple out of bounds requests here
  stopifnot(all(c(P$a1, P$a2) <= max_a))
  stopifnot(all(c(P$a1, P$a2) >= 1))
  stopifnot(all(c(P$t1, P$t2) <= max_t))
  stopifnot(all(c(P$t1, P$t2) >= 1))

  # catch simple ordering problems here
  stopifnot(all(P$t1 < P$t2))
  stopifnot(all(P$a1 < P$a2))

  # catch if things are not incrementing by 1's
  tmp <- P %>%
    filter(t2 - t1 != a2 - a1)
  if(nrow(tmp) > 0) {
    stop("Age and time combinations not compatible.  t2 - t1 must equal a2 - a1")
  }

  # Finally, we want to throw an error if we have any lines overlapping
  # different cells
  cell_counts_over1 <- P %>%
    mutate(
      t = map2(.x = t1, .y = t2, function(x, y) x:y),
      a = map2(.x = a1, .y = a2, function(x, y) x:y)
    ) %>%
    unnest(cols = c(t, a)) %>%
    count(t, a) %>%
    filter(n > 1)

  if(nrow(cell_counts_over1) > 0) {
    stop("Apparently you have some overlapping segments")
  }

  # now we lapply over the rows of P, and for each, we determine the survival probs.
  # Note that this formulation allows for time-varying age-specific survival rates
  joiner <- lapply(1:nrow(P), function(r) {
    # make a tibble to do a semi_join by:
    sjt <- tibble(
      t = (P$t1[r]):(P$t2[r]),
      a = (P$a1[r]):(P$a2[r])
    )

    # semi-join and get the S_a values and put them back on sjt
    sjt$sp <- E %>%
      semi_join(sjt, by = join_by(t, a)) %>%
      pull(S_a)

    # now, force the first element of sp in that tbble to be a surv_prob of 1 for surviving
    # at a rate of 1.0 to the upper right edge of that first cell
    sjt$sp[1] <- 1

    # then calculate the cumulative survival probs and then make the columns telling
    # us whether the line segment starts or ends.  At least initialize it to FALSE for now
    sjt2 <- sjt %>%
      mutate(
        cp_S = cumprod(sp),
        arrow_start = FALSE,
        arrow_end = FALSE
      )
    # now put TRUEs in the arrow start and end columns as needed
    sjt2$arrow_start[1] <- TRUE
    sjt2$arrow_end[nrow(sjt2)] <- TRUE

    sjt2
  }) %>%
    bind_rows()

  # now, by simply left-joining joiner back onto E, we are returning what we need.
  E %>%
    left_join(joiner, by = join_by(t, a))

}


#' Given an Eft-like tibble, make a plot displaying CKMR-related elements
#'
#' Mostly this is to do a function that lets us quickly plot things with
#' only certain cells filled, etc.
#' @param E a tibble like Es
#' @param cell_fill_var the variable to fill colors by.  Usually will be Emt or Eft
#' of Nt, etc.
#' @param cell_str_var the variable to use for the strings printed in each cell
#' @param min_fill min value in the fill scale
#' @param max_fill max value in the fill scale
#' @param cell_mask a tibble with columns t and a giving values of t and a to
#' have filled with colors (the rest will be clear/white). Values not occurring
#' with E will be ignored.
#' @param survival_endpoints a tibble with t1, a1, t2, a2 giving the start and
#' endpoints of the desired survival lines.
#' @param circ_cells a tibble with columns t and a giving the cells that should
#' have the little sampling circle in them
#' @export
#' @examples
#' t <- 3:6
#' cm <- tibble::tibble(t = t, a = t+1)
#' cm2 <- tidyr::expand_grid(t = c(4, 7), a = 1:8)
#'
plot_ckmr_pictorial <- function(
    E,
    cell_fill_var = "Eft",
    cell_str_var = "Ef_str",
    min_fill = NA,
    max_fill = NA,
    cell_mask = NULL,
    survival_endpoints = NULL,
    circ_cells = NULL
) {


  # get some line positions
  as <- unique(c(E$a - 0.5, E$a + 0.5))
  ts <- unique(c(E$t - 0.5, E$t + 0.5))


  # do the cell masking here:
  if(!is.null(cell_mask)) {
    cm <- cell_mask %>%
      mutate(.fill_flag = 1)

    E2 <- E %>%
      left_join(cm, by = join_by(t, a)) %>%
      mutate("{cell_fill_var}" := ifelse(is.na(.fill_flag), NA, .data[[cell_fill_var]]))
  } else {
    E2 <- E
  }




  # get our base layer down
  g <- ggplot(E2, aes(x = t, y = a)) +
    geom_tile(aes(fill = .data[[cell_fill_var]])) +
    geom_text(
      aes(label = TeX(.data[[cell_str_var]], output = "character")),
      parse = TRUE,
      size = 7/.pt
    ) +
    geom_hline(yintercept = as) +
    geom_vline(xintercept = ts) +
    scale_fill_viridis_c(option = "H", alpha = 0.6, na.value = NA, limits = c(min_fill, max_fill)) +
    theme_void() +
    coord_cartesian(expand = FALSE)


  # now, deal with the survival segments
  if(!is.null(survival_endpoints)) {
    E3 <- make_survival_segpoints(E2, survival_endpoints)

    g <- g +
      scale_linewidth_continuous(limits = c(0, 1), range = c(0, 3)) +
      guides(linewidth = "none") +
      geom_segment(
        data = E3,
        mapping = aes(
          x = t - 0.5 * !arrow_start,
          xend = t + 0.5 * !arrow_end,
          y = a - 0.5 * !arrow_start,
          yend = a + 0.5 * !arrow_end,
          linewidth = cp_S
        ),
        colour = "gray40",
        alpha = 0.5
      )
  }

  if(!is.null(circ_cells)) {
    g <- g +
      geom_point(
        data = circ_cells,
        mapping = aes(x = t + 0.3, y = a + 0.3),
        shape = 21,
        fill = NA,
        stroke = 0.2
      )


  }

  g

}
