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


# here is function to make a squiggly arrow
make_squiggly_arrow_df <- function(x0, y0, x1, y1,
                                   n_cycles = 3,
                                   amplitude = 0.1,
                                   n_points = 500) {
  # Direction vector from start to end
  dx <- x1 - x0
  dy <- y1 - y0
  length <- sqrt(dx^2 + dy^2)

  # Normal (perpendicular) unit vector
  nx <- -dy / length
  ny <- dx / length

  # Parametric position along the main line
  t <- seq(0, 1, length.out = n_points)

  # Add sinusoidal offset in the perpendicular direction
  x <- x0 + dx * t + amplitude * -sin(2 * pi * n_cycles * t) * nx
  y <- y0 + dy * t + amplitude * -sin(2 * pi * n_cycles * t) * ny

  data.frame(x = x, y = y)
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
#' @param dcirc_cells a tibble with columns t and a giving the cells that should
#' have the little double-sampling circle (that denotes sampling a pair from the offspring) in them
#' @param dcirc_small Size of the small circle inside the dcirc.  Default = 1.5
#' @param dcirc_big Size of the big circle inside the dcirc.  Default = 3.0
#' @param sample_squiggle a tibble in which each row represents a single sampled individual.
#' The columns are  b (the birth year), ts (the sampling year), col (the color you want the
#' squiggle to be as a string)
#' @param samp_squig_ystart the y-value at which we place the newborn samples for the squiggles.
#' @param samp_squig_fract the fraction of vertical space that a year takes up in the sample squiggle
#' area.
#' @param squig_width the linewidth of the sample-squiggle path
#' @param aas_text_size the size of the "age at sampling" text if sample squiggle is not null.
#' @param aas_num_size the size of the age at sampling numbers if sample squiggle is not null.
#' @export
#' @examples
#' vitals <- example_vitals()
#' set.seed(5)
#' E <- create_Es_from_vitals(vitals = vitals, T = 15)
#' plot_ckmr_pictorial(E, cell_str_var = "N")
#'
#' ss <- tibble::tibble(b = c(3, 5), ts = c(6, 12), col = c("red", "blue"))
#'
#' plot_ckmr_pictorial(E, cell_str_var = "N", sample_squiggle = ss)
plot_ckmr_pictorial <- function(
    E,
    cell_fill_var = "Eft",
    cell_str_var = "Ef_str",
    min_fill = NA,
    max_fill = NA,
    cell_mask = NULL,
    survival_endpoints = NULL,
    circ_cells = NULL,
    dcirc_cells = NULL,
    dcirc_small = 1.5,
    dcirc_big = 3.0,
    sample_squiggle = NULL,
    samp_squig_ystart = -0.1,
    samp_squig_fract = 0.3,
    squig_width = 0.5,
    aas_text_size = 2.0,
    aas_num_size = 2.0
) {


  # get some line positions
  as <- unique(c(E$a - 0.5, E$a + 0.5))
  ts <- unique(c(E$t - 0.5, E$t + 0.5))

  ts_tib <- tibble(
    ts = ts,
    yhi = max(as),
    ylo = min(as)
  )


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
    geom_segment(data = ts_tib, mapping = aes(x = ts, xend = ts, y = yhi, yend = ylo)) +
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

  if(!is.null(dcirc_cells)) {
    g <- g +
      geom_point(
        data = dcirc_cells,
        mapping = aes(x = t - 0.3, y = a + 0.3),
        shape = 21,
        fill = NA,
        stroke = 0.2,
        size = dcirc_small
      ) +
      geom_point(
        data = dcirc_cells,
        mapping = aes(x = t - 0.3, y = a + 0.3),
        shape = 21,
        fill = NA,
        stroke = 0.2,
        size = dcirc_big
      )
  }

  if(!is.null(sample_squiggle)) {
    # recalculate the positions
    ss2 <- sample_squiggle %>%
      mutate(
        diff = ts - b,
        start_x = b,
        end_x = ts,
        start_y = samp_squig_ystart,
        end_y = start_y - diff * samp_squig_fract
      )
    # this is the furthest down line we need
    max_squig <- max(ss2$diff)

    # get the ts_tib values we need
    tt2 <- ts_tib %>%
      mutate(
        yhi = samp_squig_ystart,
        ylo = yhi - max_squig * samp_squig_fract
      )
    # get a tibble for some annotation text to fit in on the end
    aas_tib <- tibble(
      x1 = max(ts) - 0.5,
      x2 = max(ts),
      y1 = samp_squig_ystart,
      y2 = samp_squig_ystart - max_squig * samp_squig_fract,
      label = "age at sampling"
    )
    aas_nums <- tibble(
      y = samp_squig_ystart - (0:max_squig) * samp_squig_fract
    ) %>%
      mutate(
        x = min(as) + 0.1,
        label = 0:(n() - 1)
      )

    # make a tibble for the squiggly arrows
    arrows_tib <- ss2 %>%
      mutate(data = pmap(
        .l = list(
          x0 = start_x,
          x1 = end_x,
          y0 = start_y,
          y1 = end_y,
          n_cycles = ceiling(sqrt( (end_x - start_x) ^ 2 + (end_y - start_y) ^ 2) * 2)  # two cycles per unit of arrow length
        ),
        .f = make_squiggly_arrow_df
      )
    ) %>%
      unnest(cols = data)


    g <- g +
      geom_hline(yintercept = samp_squig_ystart - (0:max_squig) * samp_squig_fract, linetype = "dotted") +
      geom_segment(data = tt2, mapping = aes(x = ts, xend = ts, y = yhi, yend = ylo), linetype = "dotted") +
      geom_text(data = aas_tib, mapping = aes(x = x1,  y = y1, label = label), angle = -90, hjust = "left", vjust = 0.5, size = aas_text_size) +
      geom_text(data = aas_nums, mapping = aes(x = x, y = y, label = label), hjust = "left", vjust = 0.5, size = aas_num_size) +
      geom_path(
        data = arrows_tib,
        mapping = aes(x = x, y = y, color = col),
        linewidth = squig_width
      ) +
      scale_color_identity() +
      geom_point(
        data = ss2,
        mapping = aes(x = mean(ts), y = end_y - 1.5/diff),  # add an invisible point to expand the plot area
        shape = 1,
        alpha = 0
      )
  }
  g

}
