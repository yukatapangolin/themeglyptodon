
#' Theme Glyptodon
#'
#' @param base_family
#' @param base_size
#' @param plot_title_family
#' @param plot_title_size
#' @param plot_title_face
#' @param plot_title_margin
#' @param subtitle_family
#' @param subtitle_size
#' @param subtitle_face
#' @param subtitle_margin
#' @param strip_text_family
#' @param strip_text_size
#' @param strip_text_face
#' @param caption_family
#' @param caption_size
#' @param caption_face
#' @param caption_margin
#' @param axis_text_size
#' @param axis_title_family
#' @param axis_title_size
#' @param axis_title_face
#' @param axis_title_just
#' @param plot_margin
#' @param grid_col
#' @param grid
#' @param axis_col
#' @param axis
#' @param ticks
#'
#' @return
#' @importFrom ggplot2 margin theme element_blan element_line element_text rel unit
#' @export
#'
#' @examples
theme_glyptodon <- function (base_family = "Roboto Condensed",
                             base_size = 11.5,

                             plot_title_family = "IBM Plex Sans",
                             plot_title_size = base_size + 10.5,
                             plot_title_face = "bold",
                             plot_title_margin = 5,

                             subtitle_family = "Source Sans Pro",
                             subtitle_size = base_size + 1,
                             subtitle_face = "plain",
                             subtitle_margin = 15,

                             strip_text_family = "Arial",
                             strip_text_size = base_size + 1,
                             strip_text_face = "plain",
                             strip_background_col = "#835C3B",
                             strip_text_col = "#ffffff",
                             strip_tran = FALSE,

                             caption_family = "Roboto",
                             caption_size = base_size - 1,
                             caption_face = "italic",
                             caption_margin = 10,
                             caption_col = "#1c1c1c",

                             axis_text_family = "Goldman Sans Condensed",
                             axis_text_size = base_size + 2.5,
                             axis_title_family = "Arial",
                             axis_title_size = base_size,
                             axis_col = caption_col,
                             axis_title_face = "plain",
                             axis_title_just = "rt",
                             axis_title_col = caption_col,
                             axis = TRUE,

                             plot_margin = margin(20, 20, 20, 20),
                             plot_background = "#eee8d5",
                             legend_position = "top",

                             grid_major_col = "#70602d",
                             grid_minor_col = "#d7c99d",
                             grid = TRUE,


                             ticks = TRUE)
{
  if (identical(strip_tran, TRUE)) {
    strip_background_col <-  "transparent"
    strip_text_face <- "bold"
    strip_text_col <-  "#121212"
  }
  ret <- ggplot2::theme_minimal(base_family = base_family,
                                base_size = base_size)
  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key = element_blank())
  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + theme(panel.grid = element_line(color = grid_major_col,
                                                 size = 0.2))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_major_col,
                                                       size = rel(.25)))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_minor_col,
                                                       linetype = "dotted",
                                                       size = rel(1.25)))
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.y = element_blank())
    }
  }
  else {
    ret <- ret + theme(panel.grid = element_blank())
  }
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line.x = element_line(color = axis_col,
                                                size = 0.3))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      }
      else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col,
                                                      size = 0.3))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      }
      else {
        ret <- ret + theme(axis.line.y = element_line(color = axis_col,
                                                      size = 0.15))
      }
    }
    else {
      ret <- ret + theme(axis.line.x = element_line(color = axis_col,
                                                    size = 0.5))
      #ret <- ret + theme(axis.line.y = element_line(color = axis_col,
      #                                              size = 0.15))
    }
  }
  else {
    ret <- ret + theme(axis.line = element_blank())
  }
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  }
  else {
    ret <- ret + theme(axis.ticks = element_line(size = 0.5))
    ret <- ret + theme(axis.ticks.x = element_line(size = 0.5))
    ret <- ret + theme(axis.ticks.y = element_line(size = 0))
    ret <- ret + theme(axis.ticks.length = grid::unit(4,
                                                      "pt"))
  }
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0,
               l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0,
               l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  ret <- ret + theme(axis.text.x = element_text(family = axis_text_family,
                                                color = axis_col,
                                                size = axis_text_size,
                                                margin = margin(t = 2)))
  ret <- ret + theme(axis.text.y = element_text(family = axis_text_family,
                                                color = axis_col,
                                                size = axis_text_size,
                                                margin = margin(r = 0)))
  ret <- ret + theme(axis.title = element_text(size = axis_title_size,
                                               color = axis_title_col,
                                               family = axis_title_family))
  ret <- ret + theme(axis.title.x = element_text(hjust = xj,
                                                 size = axis_title_size,
                                                 family = axis_title_family,
                                                 face = axis_title_face))
  ret <- ret + theme(axis.title.y = element_text(hjust = yj,
                                                 size = axis_title_size,
                                                 family = axis_title_family,
                                                 face = axis_title_face))
  ret <- ret + theme(axis.title.y.right = element_text(
    hjust = yj,
    size = axis_title_size,
    angle = 90,
    family = axis_title_family,
    face = axis_title_face))
  ret <- ret + theme(strip.text = element_text(
    hjust = 0.5,
    color = strip_text_col,
    size = strip_text_size,
    face = strip_text_face,
    family = strip_text_family),
    strip.background = element_rect(fill = strip_background_col,
                                    colour = NA, linetype = 0))
  ret <- ret + theme(panel.spacing = grid::unit(2, "lines"))
  ret <- ret + theme(plot.title = element_text(
    hjust = 0, size = plot_title_size,
    margin = margin(b = plot_title_margin),
    family = plot_title_family,
    face = plot_title_face))
  ret <- ret + theme(plot.subtitle = element_text(
    hjust = 0,
    size = subtitle_size,
    margin = margin(b = subtitle_margin),
    family = subtitle_family,
    face = subtitle_face))
  ret <- ret + theme(plot.caption = element_text(
    hjust = 1,
    color = caption_col,
    size = caption_size,
    margin = margin(t = caption_margin),
    family = caption_family,
    face = caption_face))
  ret <- ret + theme(plot.margin = plot_margin)
  ret <- ret + theme(rect = element_rect(fill = strip_background_col,
                                         colour = NA, linetype = 1),
                     text = element_text(colour = "#010101"),
                     panel.spacing = unit(0.5, "lines"),
                     legend.title.align = NULL,
                     legend.position = legend_position,
                     legend.direction = NULL,
                     legend.justification = "center",

                     plot.background = element_rect(fill = plot_background,
                                                    colour = NA))
  ret <- ret + theme(plot.caption = element_text(
    margin = margin(t = 25, r = 0, b = 0, l = 0)))
  ret <- ret + theme(
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))
    )
  ret
}
