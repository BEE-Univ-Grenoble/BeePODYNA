#' plot.population
#'
#' plot one or multiple given populations
#'
#' @param x a population object
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for lines. Does NOT apply for lines and capacity ablines.
#' \itemize{
#'   \item capacity.line TRUE/FALSE if a dashed line must be drawned for each populations
#'   \item log.time TRUE/FALSE if time axis (x axis) must be logarithmic
#'   \item capacity_lty set to 3 by default
#'   \item type for the size line, set to 'b' by default
#'   \item xlab set to 'Time' by default
#'   \item ybal set to 'Size' by default
#'   \item main set to use the pop label by default
#'   \item capacity_line set to TRUE by default
#'   }
#'
#' @seealso \code{\link[graphics]{plot}} and \code{\link[graphics]{par}} for graphical parameter that were omitted on this documentation
#'
#' @import graphics
#'
#' @examples
#' \dontrun{
#' hirsu = population("hirsuta",20,1,100)
#' daonen = population("daonensis",30,1.2,100)
#'
#' daonen[[2]]=c(30,34)
#' daonen[[3]]=c(0,5)
#'
#' liste = list(hirsu,daonen)
#' plot_population(list = liste, log.pop = T, color = c(3,2), xlab = "Time")
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
plot_population = function(x, ...) {
  UseMethod('plot_population')
}

#' @rdname plot_population
#' @export
plot_population.population <- function(pop, ...){

  mc <- match.call()

  #checks for arguments
  if(!is.population(pop)){stop(sprintf("I cannot cast an object of type %s to population object",
                                       class(object)
  ))}
  #check for custom graphical parameters
  if (is.null(mc$capacity_lty)) { capacity_lty = 3
  } else { capacity_lty = eval(mc$capacity_lty) }

  if (is.null(mc$type)) { type = "b"
  } else { type = eval(mc$type) }

  if (is.null(mc$xlab)) { xlab = "Time"
  } else { xlab = eval(mc$xlab)}

  if (is.null(mc$ylab)) { ylab = "Size"
  } else { ylab = eval(mc$ylab)}

  if (is.null(mc$main)) { main = pop$label
  } else { main = eval(mc$main)}

  if (is.null(mc$capacity_line)) { capacity_line = TRUE
  } else { capacity_line = eval(mc$capacity_line) }

  #remove log_pop or not?
  if (is.null(mc$log_pop)) { log_pop = FALSE
  } else { log_pop = eval(mc$log_pop) }

  # #checks to remove
  # if(log_pop){
  #   pop$size[pop$size<1] = 1 #arbitrory choice
  #   pop$size = log(pop$size)
  #   pop$capacity = log(pop$capacity)
  # }

  #limitations
  if (is.null(mc$xlim)) {
    time_min <- min(pop$time)
    time_max <- max(pop$time)
    x_dist <- time_max - time_min
    time_min <- time_min - x_dist * 0.1
    time_max <- time_max + x_dist * 0.1

    xlim = c(time_min,time_max)
  } else {
    xlim = eval(mc$xlim)
    time_min <- xlim[1]
    time_max <- xlim[2]
  }
  if (is.null(mc$ylim)) {
    pop_min <- min(pop$size)
    pop_max <- max(pop$size)
    y_dist <- pop_max - pop_min
    pop_min <- pop_min - y_dist * 0.1
    pop_max <- pop_max + y_dist * 0.1

    ylim = c(pop_min,pop_max)
  } else {
    ylim = eval(mc$ylim)
    pop_min <- ylim[1]
    pop_max <- ylim[2]
  }

  #plotting env
  localplot <- function(x,y, ..., type, xlim, ylim, xlab, ylab,main,
                        capacity_lty, capacity_line, log_pop) {
    plot(x,y ,... , type ="n", xlab = " ", ylab = " ", main = " ")
  }
  localplot(x = 1, y=1, xlim, ylim, ...)
  #setting titles
  localTitle <- function( ..., type, xlim, ylim, xlab, ylab,main,
                          capacity_lty, capacity_line, log_pop) title(...)
  localTitle(main,sub = NULL, xlab, ylab)
  #plotting the pop itself
  localLines <- function(x,y,...,type, xlim,ylim, xlab, ylab, main,
                         capacity_lty, capacity_line, log_pop) lines(x,y, ...)
  localLines(pop$time,pop$size, type , ...)

  #plotting capacity line
  if(capacity_line) {
    abline(h=pop$capacity, lty = capacity_lty)
    text(time_min,pop$capacity,"Capacity",pos=4)
  }
}


#' @rdname plot_population
#' @export
plot.population <- function(pop, ...){

  mc <- match.call()

  #checks for arguments
  if(!is.population(pop)){stop(sprintf("I cannot cast an object of type %s to population object",
                                       class(object)
  ))}
  #check for custom graphical parameters
  if (is.null(mc$capacity_lty)) { capacity_lty = 3
  } else { capacity_lty = eval(mc$capacity_lty) }

  if (is.null(mc$type)) { type = "b"
  } else { type = eval(mc$type) }

  if (is.null(mc$xlab)) { xlab = "Time"
  } else { xlab = eval(mc$xlab)}

  if (is.null(mc$ylab)) { ylab = "Size"
  } else { ylab = eval(mc$ylab)}

  if (is.null(mc$main)) { main = pop$label
  } else { main = eval(mc$main)}

  if (is.null(mc$capacity_line)) { capacity_line = TRUE
  } else { capacity_line = eval(mc$capacity_line) }

  #remove log_pop or not?
  if (is.null(mc$log_pop)) { log_pop = FALSE
  } else { log_pop = eval(mc$log_pop) }

  # #checks to remove
  # if(log_pop){
  #   pop$size[pop$size<1] = 1 #arbitrory choice
  #   pop$size = log(pop$size)
  #   pop$capacity = log(pop$capacity)
  # }

  #limitations
  if (is.null(mc$xlim)) {
    time_min <- min(pop$time)
    time_max <- max(pop$time)
    x_dist <- time_max - time_min
    time_min <- time_min - x_dist * 0.1
    time_max <- time_max + x_dist * 0.1

    xlim = c(time_min,time_max)
  } else {
    xlim = eval(mc$xlim)
    time_min <- xlim[1]
    time_max <- xlim[2]
  }
  if (is.null(mc$ylim)) {
    pop_min <- min(pop$size)
    pop_max <- max(pop$size)
    y_dist <- pop_max - pop_min
    pop_min <- pop_min - y_dist * 0.1
    pop_max <- pop_max + y_dist * 0.1

    ylim = c(pop_min,pop_max)
  } else {
    ylim = eval(mc$ylim)
    pop_min <- ylim[1]
    pop_max <- ylim[2]
  }

  #plotting env
  localplot <- function(x,y, ..., type, xlim, ylim, xlab, ylab,main,
                        capacity_lty, capacity_line, log_pop) {
    plot(x,y ,... , type ="n", xlab = " ", ylab = " ", main = " ")
  }
  localplot(x = 1, y=1, xlim, ylim, ...)
  #setting titles
  localTitle <- function( ..., type, xlim, ylim, xlab, ylab,main,
                          capacity_lty, capacity_line, log_pop) title(...)
  localTitle(main,sub = NULL, xlab, ylab)
  #plotting the pop itself
  localLines <- function(x,y,...,type, xlim,ylim, xlab, ylab, main,
                         capacity_lty, capacity_line, log_pop) lines(x,y, ...)
  localLines(pop$time,pop$size, type , ...)

  #plotting capacity line
  if(capacity_line) {
    abline(h=pop$capacity, lty = capacity_lty)
    text(time_min,pop$capacity,"Capacity",pos=4)
  }
}



