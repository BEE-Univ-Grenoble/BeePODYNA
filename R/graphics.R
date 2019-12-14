#' @import graphics
#' @import grDevices
NULL

#' plot_population
#'
#' plot one given population and its capacity line.
#'
#' @param x a population object
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for lines.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the population curve, NOT on the capacity line (custom parameter need to be given, as exemple capacity_lty).
#' \itemize{
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{xlab} set to \code{'Time'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Size'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{main} set to use the pop label by default, see \code{\link[graphics]{title}}.
#'   }
#' @param capacity_line set to \code{TRUE} by default, if a dashed line must be drawned to show the capacity.
#' @param capacity_lty set to \code{3} by default.
#' @param text_print set to \code{TRUE} by default, whether there is a text on the capacipty line or not (writting population 'K label' by default).
#' @param add set to \code{FALSE} by default, to add another population plot on a precedent one.
#'
#'
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and \code{\link[graphics]{par}} for plot parameter that were omitted on this documentation
#' \item \code{\link[BeePODYNA]{population}} for every aspect about population and community creations.
#' }
#'
#' @examples
#' data(hudson)
#' hare <- hudson$hare
#' lynx <- hudson$lynx
#'
#' plot(hare,
#'   col = "red", pch = 15, type = "b", capacity_lty = 3,
#'   lty = 2, main = "Hudson data"
#' )
#'
#' plot(lynx,
#'   col = "blue", pch = 15, type = "b", capacity_lty = 3,
#'   add = TRUE, lty = 1, capacity_line = TRUE
#' )
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
plot.population <- function(x,
                            ...,
                            capacity_line = TRUE,
                            capacity_lty = 3,
                            text_print = TRUE,
                            add = FALSE) {
  delta_x <- (max(x$time) - min(x$time)) * 0.1
  delta_y <- (max(x$size) - min(x$size)) * 0.2

  default_par <- list(
    col = 1,
    type = "b",
    xlab = "Time",
    ylab = "Population size",
    main = x$label,
    xlim = c(min(x$time) - delta_x, max(x$time) + delta_x),
    ylim = c(min(x$size) - delta_y, max(x$size) + delta_y)
  )

  call_par <- list(...)

  names_defaut_par <- names(default_par)
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) | is.null(call_par[[names_defaut_par[i]]])) {
      call_par[[names_defaut_par[i]]] <- default_par[[names_defaut_par[i]]]
    }
  }

  if ("log" %in% names(call_par)) {
    call_par$log <- sub("x", "", call_par$log)
    if (regexec("y", call_par$log)[[1]] > 0) {
      if (default_par$ylim[1] < 0) call_par$ylim[1] <- 1
    }
  }

  if (!add) {
    empty_par <- c(list(x = 1, y = 1), call_par)
    empty_par$type <- "n"

    do.call(plot, empty_par)
  }

  line_par <- c(list(x = x$time, y = x$size), call_par)
  line_par$xlim <- NULL
  line_par$xlim <- NULL
  line_par$ylim <- NULL
  line_par$xlab <- NULL
  line_par$ylab <- NULL
  line_par$main <- NULL
  line_par$log <- NULL

  do.call(lines, line_par)

  if (capacity_line) {
    abline(h = x$capacity, lty = capacity_lty, col = call_par$col)
  }

  if (text_print) {
    text(
      x = x$time[1], y = x$capacity,
      paste("K", x$label, sep = " "),
      pos = 3,
      col = call_par$col
    )
  }
}
#' plot.community
#'
#' plot one given community and its capacity line.
#'
#' @param x a community object
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for lines.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the population curve, NOT on the capacity line (custom parameter need to be given, as exemple capacity_lty).
#' \itemize{
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{xlab} set to \code{'Time'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Size'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{main} set to use the pop label by default, see \code{\link[graphics]{title}}.
#'   }
#' @param capacity_line set to \code{TRUE} by default, if a dashed line must be drawned to show the capacity.
#' @param capacity_lty set to \code{3} by default.
#' @param text_print set to \code{TRUE} by default, whether there is a text on the capacipty line or not (writting population 'K label' by default).
#' @param add set to \code{FALSE} by default, to add another population plot on a precedent one.
#'
#' @note For \strong{capacity_line}, \strong{capacity_lty}, \strong{type}, \strong{text_print} and \strong{text_x} parameters, values can be a single value or a vector of value. If the vector length is inferior to the population number in the community, the value or the vector will be copy.
#'
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and \code{\link[graphics]{par}} for plot parameter that were omitted on this documentation
#' \item \code{\link[BeePODYNA]{plot.population}}, \code{\link[BeePODYNA]{population}}, \code{\link[BeePODYNA]{community}} for every aspect about population and community creations.
#' }
#'
#' @import graphics
#'
#' @examples
#' data(hudson)
#' plot(hudson)
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
plot.community <- function(x, ...,
                           capacity_line = TRUE,
                           capacity_lty = 3,
                           text_print = TRUE,
                           add = FALSE) {
  n_pop <- length(x)

  capacity_line <- rep(capacity_line, n_pop)
  capacity_lty <- rep(capacity_lty, n_pop)
  text_print <- rep(text_print, n_pop)

  com_time <- unlist(sapply(x$populations, "[[", 3))
  delta_x <- (max(com_time) - min(com_time))
  if(delta_x == 0) delta_x <- 0.1 else  delta_x <- delta_x * 0.1

  com_size <- unlist(sapply(x$populations, "[[", 2))
  delta_y <- (max(com_size) - min(com_size)) * 0.1
  if(delta_y == 0) delta_y <- 0.1 else  delta_y <- delta_y * 0.1

  default_par <- list(
    col = rep(1, n_pop),
    type = rep("b", n_pop),
    xlab = "Time",
    ylab = "Population size",
    main = x$label,
    xlim = c(min(com_time) - delta_x, max(com_time) + delta_x),
    ylim = c(min(com_size) - delta_y, max(com_size) + delta_y)
  )

  call_par <- list(...)

  names_defaut_par <- names(default_par)
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) | is.null(call_par[[names_defaut_par[i]]])) {
      call_par[[names_defaut_par[i]]] <- default_par[[names_defaut_par[i]]]
    }
  }

  for (i in seq_along(call_par)) {
    if(length(call_par[[i]]) < n_pop) call_par[[i]] <- rep(call_par[[i]], n_pop)
  }

  if ("log" %in% names(call_par)) {
    call_par$log <- paste(sub("x", "", call_par$log), collapse = "")
    if (regexec("y", call_par$log)[[1]] > 0) {
      if (default_par$ylim[1] < 0) call_par$ylim[1] <- 1
    }
  }

  if (!add) {
    empty_par <- c(list(x = 1, y = 1), call_par)
    tmpx <- empty_par$xlim[1:2]
    tmpy <- empty_par$ylim[1:2]
    empty_par <- lapply(empty_par, FUN = function(x) x[1])
    empty_par$xlim <- tmpx
    empty_par$ylim <- tmpy
    empty_par$type <- "n"

    do.call(plot, empty_par)
  }

  for (i in 1:n_pop) {
    line_par <- call_par
    line_par <- lapply(line_par, FUN = function(x) x[i])
    line_par <- c(list(x = x$populations[[i]]$time, y = x$populations[[i]]$size), line_par)
    line_par$xlim <- NULL
    line_par$xlim <- NULL
    line_par$ylim <- NULL
    line_par$xlab <- NULL
    line_par$ylab <- NULL
    line_par$main <- NULL
    line_par$log <- NULL

    do.call(lines, line_par)

    if (capacity_line[i]) {
      abline(h = x$populations[[i]]$capacity, lty = capacity_lty[i], col = call_par$col[i])
    }

    if (text_print[i]) {
      text(
        x = x$populations[[i]]$time[1], y = x$populations[[i]]$capacity,
        paste("K", x$populations[[i]]$label, sep = " "),
        pos = 3,
        col = call_par$col[i]
      )
    }
  }
}


#' com_plot
#'
#' plot a given community of length = 2 with 2 dimmensionnal place.
#'
#' @param x a community object
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as titles, labels and windows limitation.
#' Classical \code{\link[graphics]{graphical parameter}} applies on the population curve, NOT on the capacity line (custom parameter need to be given, as exemple capacity_lty).
#' \itemize{
#'   \item \strong{xlab} set to \code{'Time'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{ylab} set to \code{'Size'} by default, see \code{\link[graphics]{title}}.
#'   \item \strong{main} set to use the pop label by default, see \code{\link[graphics]{title}}.
#'   }
#' @param xpop the population for the x axise
#' @param ypop the population for the y axise
#' @param capacity_line set to \code{TRUE} by default, if a dashed line must be drawned to show the capacity.
#' @param capacity_lty set to \code{3} by default.
#' @param text_print set to \code{TRUE} by default, whether there is a text on the capacipty line or not (writting population 'K label' by default).
#' @param first_p the first point in the time serie to be drawn. All points are drawn by default.
#' @param white the percentage of white for the beginning of the color scale. \code{0.8} by default.
#' @param add set to \code{FALSE} by default, to add another com_plot plot on a precedent one.
#'
#' @note For \strong{capacity_line}, \strong{capacity_lty} and \strong{text_print} parameters, values can be a single value or a vector of value. If the vector length is inferior to the population number in the community, the value or the vector will be copy.
#'
#' @seealso
#' \itemize{
#' \item \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and \code{\link[graphics]{par}} for plot parameter that were omitted on this documentation
#' \item \code{\link[BeePODYNA]{plot.population}}, \code{\link[BeePODYNA]{population}}, \code{\link[BeePODYNA]{community}} for every aspect about population and community creations.
#' }
#'
#' @import graphics
#'
#' @examples
#' data(hudson)
#' com_plot(hudson)
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
com_plot <- function(x, ...,
                     xpop = 1,
                     ypop = 2,
                     capacity_line = c(TRUE, TRUE),
                     capacity_lty = c(3, 3),
                     text_print = c(TRUE, TRUE),
                     first_p = 1,
                     white = 0.8,
                     add = FALSE) {
  n_pop <- length(x)

  capacity_line <- rep(capacity_line, n_pop)
  capacity_lty <- rep(capacity_lty, n_pop)
  text_print <- rep(text_print, n_pop)

  com_x <- x$populations[[xpop]]$size
  delta_x <- (max(com_x) - min(com_x))
  if (delta_x == 0) delta_x <- 0.1 else delta_x <- delta_x * 0.1

  com_y <- x$populations[[ypop]]$size
  delta_y <- (max(com_y) - min(com_y)) * 0.1
  if (delta_y == 0) delta_y <- 0.1 else delta_y <- delta_y * 0.1

  default_par <- list(
    col = rep(1, n_pop),
    type = rep("b", n_pop),
    xlab = x$populations[[xpop]]$label,
    ylab = x$populations[[ypop]]$label,
    main = x$label,
    xlim = c(min(com_x) - delta_x, max(com_x) + delta_x),
    ylim = c(min(com_y) - delta_y, max(com_y) + delta_y)
  )

  call_par <- list(...)

  names_defaut_par <- names(default_par)
  for (i in seq_along(default_par)) {
    if (!names_defaut_par[i] %in% names(call_par) | is.null(call_par[[names_defaut_par[i]]])) {
      call_par[[names_defaut_par[i]]] <- default_par[[names_defaut_par[i]]]
    }
  }

  for (i in seq_along(call_par)) {
    if (length(call_par[[i]]) < n_pop) call_par[[i]] <- rep(call_par[[i]], n_pop)
  }

  if ("log" %in% names(call_par)) {
    tmp_y <- paste(sub("x","", call_par$log), collapse = "")
    if (regexec("y", tmp_y)[[1]] > 0) {
      if (default_par$ylim[1] < 0) call_par$ylim[1] <- 1
    }
    tmp_x <- paste(sub("y","", call_par$log), collapse = "")
    if (regexec("x", tmp_x)[[1]] > 0) {
      if (default_par$xlim[1] < 0) call_par$xlim[1] <- 1
    }
  }

  if (!add) {
    empty_par <- c(list(x = 1, y = 1), call_par)
    tmpx <- empty_par$xlim[1:2]
    tmpy <- empty_par$ylim[1:2]
    empty_par <- lapply(empty_par, FUN = function(x) x[1])
    empty_par$xlim <- tmpx
    empty_par$ylim <- tmpy
    empty_par$type <- "n"

    do.call(plot, empty_par)
  }

  com_time <- 1


  x_size <- x$populations[[xpop]]$size
  y_size <- x$populations[[ypop]]$size
  x_time <- x$populations[[ypop]]$time
  # color gradient, only gray for the moment
  gray <- gray.colors(length(x_size), start = white, end = 0, gamma = 2.2)

  for (i in first_p:length(x_size)) {
    segments(
      x0 = x_size[i], y0 = y_size[i],
      x1 = x_size[i + 1], y1 = y_size[i + 1],
      col = gray[i]
    )
    text(
      x = x_size[i], y = y_size[i],
      label = x_time[i], col = gray[i], pos = 3
    ) # modifier pour dire si on prend le time sur x ou y
  }



  if (capacity_line[1]) {
    abline(v = x$populations[[xpop]]$capacity, lty = capacity_lty[1], col = call_par$col[xpop])
  }

  if (capacity_line[2]) {
    abline(h = x$populations[[ypop]]$capacity, lty = capacity_lty[2], col = call_par$col[ypop])
  }

  if (text_print[1]) {
    text(
      x = x$populations[[xpop]]$capacity, y = call_par$ylim[1],
      paste("K", x$populations[[xpop]]$label, sep = " "),
      pos = 4, srt = 90, col = call_par$col[xpop]
    )
  }

  if (text_print[2]) {
    text(
      x = call_par$xlim[1], y = x$populations[[ypop]]$capacity,
      paste("K", x$populations[[ypop]]$label, sep = " "),
      pos = 4, col = call_par$col[ypop]
    )
  }
}