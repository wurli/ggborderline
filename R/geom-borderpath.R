#' Key glyphs for legends
#'
#' @param data,params,size See `ggplot2::draw_key_path()` for usage
#'
#' @return A gTree object
#' @keywords internal
#'
#' @export
draw_key_borderpath <- function(data, params, size) {

  if (is.null(data$linetype)) {
    data$linetype <- 0
  } else {
    data$linetype[is.na(data$linetype)] <- 0
  }

  grobTree(
    # 'Border' line
    segmentsGrob(
      0.1, 0.5, 0.9, 0.5,
      arrow = params$arrow,
      gp = gpar(
        col = alpha(data$bordercolour %||% data$fill %||% "white", data$alpha),
        fill = alpha(
          params$arrow.fill %||% data$colour %||% data$fill %||% "white",
          data$alpha
        ),
        lwd = (data$linewidth %||% data$size %||% 0.5 + (data$borderwidth %||% 0.5) * 2) * .pt,
        lty = data$linetype %||% 1,
        lineend = "butt"

        # Would be nice but causes issues with overlap
        # lineend = params$lineend %||% "butt"
      )
    ),
    # Normal line
    segmentsGrob(
      0.1, 0.5, 0.9, 0.5,
      arrow = params$arrow,
      gp = gpar(
        col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
        fill = alpha(
          params$arrow.fill %||% data$colour %||% data$fill %||% "black",
          data$alpha
        ),
        lwd = (data$linewidth %||% data$size %||% 0.5) * .pt,
        lty = data$linetype %||% 1,
        lineend = "butt"
        # lineend = params$lineend %||% "butt"
      )
    )
  )
}

#' Connect observations
#'
#' This set of geoms is very similar to `ggplot2::geom_path()`,
#' `ggplot2::geom_line()` and `ggplot2::geom_step()`, with the only difference
#' being that they accept two additional aesthetics, `bordercolour` and
#' `borderwidth`. For additional documentation, please refer to the ggplot2
#' geoms.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param arrow Arrow specification, as created by [grid::arrow()].
#'
#' @return A ggproto layer object
#'
#' @export
#' @examples
#' require(ggplot2)
#'
#' # geom_borderline() adds a border around lines
#' ggplot(economics_long, aes(date, value01, colour = variable)) +
#'   geom_borderline()
#'
#' # You can control the linewidth and colour of the border with the
#' # borderwidth and bordercolour aesthetics:
#' ggplot(economics_long, aes(date, value01, bordercolour = variable)) +
#'   geom_borderline(borderwidth = .4, colour = "white")
#'
#' # The background 'border' part of the geom is always solid, however this
#' # can be used to create some nice effects:
#' x <- seq(0, 4 * pi, length.out = 500)
#' test_data <- data.frame(
#'   x = rep(x, 2), y = c(sin(x), cos(x)),
#'   fun = rep(c("sin", "cos"), each = 500)
#' )
#' ggplot(test_data, aes(x, y, colour = fun)) +
#'   geom_borderline(linewidth = 1, linetype = "dashed", lineend = "round")
geom_borderpath <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            lineend = "butt",
                            linejoin = "round",
                            linemitre = 10,
                            arrow = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBorderpath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggborderline-extensions
#' @export
#' @keywords internal
GeomBorderpath <- ggproto("GeomBorderpath", GeomPath,

  default_aes = aes(
    colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
    bordercolour = "white", borderwidth = NULL, size = NULL
  ),

  handle_na = function(self, data, params) {

    if (!is.null(data$size) && utils::packageVersion("ggplot2") >= "3.4.0") {
      inform("The use of `size` is deprecated, please use `linewidth` instead")
    }

    data$linewidth   <- data$size %||% data$linewidth
    data$borderwidth <- data$borderwidth %||% (data$linewidth * 0.4)

    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    complete <- stats::complete.cases(
      data[c("x", "y", "linewidth", "borderwidth", "colour", "bordercolour", "linetype")]
    )
    kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      cli::cli_warn("Removed {sum(!kept)} row{?s} containing missing values ({.fn {snake_class(self)}}).")
    }

    data
  },

  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {

    if (!anyDuplicated(data$group)) {
      message_wrap("geom_path: Each group consists of only one observation. ",
                   "Do you need to adjust the group aesthetic?")
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- dapply(munched, "group", function(df) {
      linetype <- unique(df$linetype)
      new_data_frame(
        list(
          solid = identical(linetype, 1) || identical(linetype, "solid"),
          constant = nrow(unique(df[, c("alpha", "colour", "linewidth", "linetype")])) == 1
        ),
        n = 1L
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      abort("geom_path: If you are using dotted or dashed lines, colour, linewidth and linetype must be constant over the line")
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start      <- c(TRUE, group_diff)
    end        <- c(group_diff, TRUE)

    if (!constant) {
      gList(
        # 'Border' line
        segmentsGrob(
          munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
          default.units = "native", arrow = arrow,
          gp = gpar(
            col = alpha(munched$bordercolour, munched$alpha)[!end],
            fill = alpha(munched$bordercolour, munched$alpha)[!end],
            lwd = ((munched$linewidth %||% munched$size)[start] + munched$borderwidth[start] * 2) * .pt,
            lty = "solid",
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre
          )
        ),
        # Normal line
        segmentsGrob(
          munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
          default.units = "native", arrow = arrow,
          gp = gpar(
            col = alpha(munched$bordercolour, munched$alpha)[!end],
            fill = alpha(munched$bordercolour, munched$alpha)[!end],
            lwd = (munched$linewidth %||% munched$size)[start] * .pt,
            lty = munched$linetype[!end],
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre
          )
        )
      )
    } else {
      id <- match(munched$group, unique(munched$group))

      out <- lapply(unique(munched$group), function(g) {

        m <- subset(munched, group == g)
        id <- match(m$group, g)

        list(
          # 'Border' line
          polylineGrob(
            m$x, m$y, id = id,
            default.units = "native", arrow = arrow,
            gp = gpar(
              col = alpha(m$bordercolour, m$alpha)[start],
              fill = alpha(m$bordercolour, m$alpha)[start],
              lwd = ((m$linewidth %||% m$size)[start] + m$borderwidth[start] * 2) * .pt,
              lty = "solid",
              lineend = lineend,
              linejoin = linejoin,
              linemitre = linemitre
            )
          ),
          # Normal line
          polylineGrob(
            m$x, m$y, id = id,
            default.units = "native", arrow = arrow,
            gp = gpar(
              col = alpha(m$colour, m$alpha)[start],
              fill = alpha(m$colour, m$alpha)[start],
              lwd = (m$linewidth %||% m$size)[start] * .pt,
              lty = m$linetype[start],
              lineend = lineend,
              linejoin = linejoin,
              linemitre = linemitre
            )
          )
        )
      })

      out <- unlist(out, recursive = FALSE)

      do.call(gList, out)
    }
  },

  draw_key = draw_key_borderpath,

  non_missing_aes = "size",
  rename_size = TRUE

)

#' @export
#' @rdname geom_borderpath
geom_borderline <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            lineend = "butt",
                            linejoin = "round",
                            linemitre = 10,
                            arrow = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBorderline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggborderline-extensions
#' @export
GeomBorderline <- ggproto("GeomBorderline", GeomBorderpath,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)

#' @param direction direction of stairs: 'vh' for vertical then horizontal,
#'   'hv' for horizontal then vertical, or 'mid' for step half-way between
#'   adjacent x-values.
#' @export
#' @rdname geom_borderpath
geom_borderstep <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", direction = "hv",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBorderstep,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      direction = direction,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggborderline-extensions
#' @export
GeomBorderstep <- ggproto("GeomBorderstep", GeomBorderpath,
  draw_panel = function(data, panel_params, coord, direction = "hv") {
    data <- dapply(data, "group", stairstep, direction = direction)
    GeomBorderpath$draw_panel(data, panel_params, coord)
  }
)

#' Scales for borderlines
#'
#' These scales control the linewidth and colour of the borders in borderlines.
#' They work in much the same way as `ggplot2::scale_colour_continuous()`,
#' `ggplot2::scale_linewidth_discrete()`, etc.
#'
#' @param ... Passed to the corresponding ggplot2 scales
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful,
#'   for example, to apply colour settings to the bordercolour and colour
#'   aesthetics at the same time, via aesthetics = c("bordercolour", "colour").
#'
#' @return A ggproto scale object
#'
#' @export
scale_bordercolour_continuous <- function(..., aesthetics = "bordercolour") {
  out <- scale_colour_continuous(...)
  out$aesthetics <- aesthetics
  out
}

#' @rdname scale_bordercolour_continuous
#' @export
scale_bordercolour_discrete <- function(..., aesthetics = "bordercolour") {
  out <- scale_colour_discrete(...)
  out$aesthetics = aesthetics
  out
}

#' @rdname scale_bordercolour_continuous
#' @export
scale_borderwidth_continuous <- function(..., aesthetics = "borderwidth") {

  scale <- if (utils::packageVersion("ggplot2") < "3.4.0") {
    scale_size_continuous
  } else {
    scale_linewidth_continuous
  }

  out <- scale(...)
  out$aesthetics <- aesthetics
  out

}

#' @rdname scale_bordercolour_continuous
#' @export
scale_borderwidth_discrete <- function(..., aesthetics = "borderwidth") {

  scale <- if (utils::packageVersion("ggplot2") < "3.4.0") {
    scale_size_discrete
  } else {
    scale_linewidth_discrete
  }

  out <- scale(...)
  out$aesthetics <- aesthetics
  out

}

set_border_palettes <- function() {
  # Skip in old version
  if (!"element_geom" %in% getNamespaceExports("ggplot2")) {
    return()
  }
  new_pal <- function(inherit) {
    el_def(c("character", "function"), inherit = inherit)
  }

  register_theme_elements(
    palette.bordercolour.continuous = scales::pal_seq_gradient("#132B43", "#56B1F7"),
    palette.bordercolour.discrete = scales::pal_hue(),
    palette.borderwidth.continuous = scales::pal_rescale(c(1, 6)),
    palette.borderwidth.discrete = function(n) seq(2, 6, length.out = n),
    element_tree = list(
      palette.bordercolour.continuous =
        el_def(c("character", "function"), "palette.colour.continuous"),
      palette.bordercolour.discrete =
        el_def(c("character", "function"), "palette.colour.discrete"),
      palette.borderwidth.continuous =
        el_def(c("character", "numeric", "integer", "function"), "palette.linewidth.continuous"),
      palette.borderwidth.discrete =
        el_def(c("character", "numeric", "integer", "function"), "palette.linewidth.discrete")
    )
  )
}

.onLoad <- function(...) {
  set_border_palettes()
}
