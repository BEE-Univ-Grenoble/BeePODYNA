#' plot.population
#'
#' plot one given population and its capacity line.
#'
#' @param x a population object
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for lines. Does NOT apply for lines and capacity ablines.
#' \itemize{
#'   \item \strong{capacity_line} set to \code{TRUE} by default, if a dashed line must be drawned to show the capacity.
#'   \item \strong{capacity_lty} set to \code{3} by default.
#'   \item \strong{type} for the size line, set to \code{'b'} by default.
#'   \item \strong{xlab} set to \code{'Time'} by default, see \code{\link[graphics]{title}}
#'   \item \strong{ylab} set to \code{'Size'} by default, see \code{\link[graphics]{title}}
#'   \item \strong{main} set to use the pop label by default, see \code{\link[graphics]{title}}
#'   }
#'
#' @seealso \code{\link[graphics]{plot}}, \code{\link[graphics]{title}} and \code{\link[graphics]{par}} for plot parameter that were omitted on this documentation
#'
#' @import graphics
#'
#' @examples
#' hare = population("hare",30,1.2,33)
#' hare[[2]]=c(4,30,32,33,33)
#' hare[[3]]=c(0,1,2.5,5,8)
#'
#' plot(hare)
#' plot(hare, col = 'red', pch=15, lty = 6, capacity_lty = 2)
#' plot(hare, col = 'red', pch=15, lty = 6, capacity_line = FALSE)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export

# plot_population = function(pop, ...) {
#   UseMethod('plot_population')
# }
#
# # @rdname plot_population
# # @export
# plot_population.default <- function(pop,...) {
#   stop(sprintf("I cannot cast an object of type %s to population object",
#                class(pop)
#   )
#   )
# }
#
# # @rdname plot_population
# # @export
# plot_population.population <- function(x, ...) {
#
#   mc <- match.call()
#
#   # checks for arguments
#   if (!is.population(pop)) {
#     stop("The object must be a population.")
#   }
#   # check for custom graphical parameters
#   if (is.null(mc$capacity_lty)) {
#     capacity_lty <- 3
#   } else {
#     capacity_lty <- eval(mc$capacity_lty)
#   }
#
#   if (is.null(mc$type)) {
#     type <- "b"
#   } else {
#     type <- eval(mc$type)
#   }
#
#   if (is.null(mc$xlab)) {
#     xlab <- "Time"
#   } else {
#     xlab <- eval(mc$xlab)
#   }
#
#   if (is.null(mc$ylab)) {
#     ylab <- "Size"
#   } else {
#     ylab <- eval(mc$ylab)
#   }
#
#   if (is.null(mc$main)) {
#     main <- pop$label
#   } else {
#     main <- eval(mc$main)
#   }
#
#   if (is.null(mc$capacity_line)) {
#     capacity_line <- TRUE
#   } else {
#     capacity_line <- eval(mc$capacity_line)
#   }
#
#   # remove log_pop or not?
#   if (is.null(mc$log_pop)) {
#     log_pop <- FALSE
#   } else {
#     log_pop <- eval(mc$log_pop)
#   }
#   # #checks to remove
#   # if(log_pop){
#   #   pop$size[pop$size<1] = 1 #arbitrory choice
#   #   pop$size = log(pop$size)
#   #   pop$capacity = log(pop$capacity)
#   # }
#
#   # limitations
#   if (is.null(mc$xlim)) {
#     time_min <- min(pop$time)
#     time_max <- max(pop$time)
#     x_dist <- time_max - time_min
#     time_min <- time_min - x_dist * 0.1
#     time_max <- time_max + x_dist * 0.1
#
#     xlim <- c(time_min, time_max)
#   } else {
#     xlim <- eval(mc$xlim)
#     time_min <- xlim[1]
#     time_max <- xlim[2]
#   }
#   if (is.null(mc$ylim)) {
#     pop_min <- min(pop$size)
#     pop_max <- max(pop$size)
#     y_dist <- pop_max - pop_min
#     pop_min <- pop_min - y_dist * 0.1
#     pop_max <- pop_max + y_dist * 0.1
#
#     ylim <- c(pop_min, pop_max)
#   } else {
#     ylim <- eval(mc$ylim)
#     pop_min <- ylim[1]
#     pop_max <- ylim[2]
#   }
#
#   # plotting env
#   localplot <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main,
#                           capacity_lty, capacity_line, log_pop) {
#     plot(x, y, ..., type = "n", xlab = " ", ylab = " ", main = " ")
#   }
#   localplot(x = 1, y = 1, xlim, ylim, ...)
#   # setting titles
#   localTitle <- function(..., type, xlim, ylim, xlab, ylab, main,
#                            capacity_lty, capacity_line, log_pop) title(...)
#   localTitle(main, sub = NULL, xlab, ylab)
#   # plotting the pop itself
#   localLines <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main,
#                            capacity_lty, capacity_line, log_pop) lines(x, y, ...)
#   localLines(pop$time, pop$size, type, ...)
#   # plotting capacity line
#   if (capacity_line) {
#     abline(h = pop$capacity, lty = capacity_lty)
#     text(time_min, pop$capacity, "Capacity", pos = 4)
#   }
# }


#' @rdname plot_population
#' @export
plot.population <- function(x, ...) {

  pop = x
  mc <- match.call()

  # checks for arguments
  if (!is.population(pop)) {
    stop("The object must be a population.")
  }
  # check for custom graphical parameters
  if (is.null(mc$capacity_lty)) {
    capacity_lty <- 3
  } else {
    capacity_lty <- eval(mc$capacity_lty)
  }

  if (is.null(mc$type)) {
    type <- "b"
  } else {
    type <- eval(mc$type)
  }

  if (is.null(mc$xlab)) {
    xlab <- "Time"
  } else {
    xlab <- eval(mc$xlab)
  }

  if (is.null(mc$ylab)) {
    ylab <- "Size"
  } else {
    ylab <- eval(mc$ylab)
  }

  if (is.null(mc$main)) {
    main <- pop$label
  } else {
    main <- eval(mc$main)
  }

  if (is.null(mc$capacity_line)) {
    capacity_line <- TRUE
  } else {
    capacity_line <- eval(mc$capacity_line)
  }

  # remove log_pop or not?
  if (is.null(mc$log_pop)) {
    log_pop <- FALSE
  } else {
    log_pop <- eval(mc$log_pop)
  }

  # #checks to remove
  # if(log_pop){
  #   pop$size[pop$size<1] = 1 #arbitrory choice
  #   pop$size = log(pop$size)
  #   pop$capacity = log(pop$capacity)
  # }

  # limitations
  if (is.null(mc$xlim)) {
    time_min <- min(pop$time)
    time_max <- max(pop$time)
    x_dist <- time_max - time_min
    time_min <- time_min - x_dist * 0.1
    time_max <- time_max + x_dist * 0.1

    xlim <- c(time_min, time_max)
  } else {
    xlim <- eval(mc$xlim)
    time_min <- xlim[1]
    time_max <- xlim[2]
  }
  if (is.null(mc$ylim)) {
    pop_min <- min(pop$size)
    pop_max <- max(pop$size)
    y_dist <- pop_max - pop_min
    pop_min <- pop_min - y_dist * 0.1
    pop_max <- pop_max + y_dist * 0.1

    ylim <- c(pop_min, pop_max)
  } else {
    ylim <- eval(mc$ylim)
    pop_min <- ylim[1]
    pop_max <- ylim[2]
  }

  # plotting env
  localplot <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main,
                          capacity_lty, capacity_line, log_pop) {
    plot(x, y, ..., type = "n", xlab = " ", ylab = " ", main = " ")
  }
  localplot(x = 1, y = 1, xlim, ylim, ...)
  # setting titles
  localTitle <- function(..., type, xlim, ylim, xlab, ylab, main,
                           capacity_lty, capacity_line, log_pop) title(...)
  localTitle(main, sub = NULL, xlab, ylab)
  # plotting the pop itself
  localLines <- function(x, y, ..., type, xlim, ylim, xlab, ylab, main,
                           capacity_lty, capacity_line, log_pop) lines(x, y, ...)
  localLines(pop$time, pop$size, type, ...)

  # plotting capacity line
  if (capacity_line) {
    abline(h = pop$capacity, lty = capacity_lty)
    text(time_min, pop$capacity, "Capacity", pos = 4)
  }
}