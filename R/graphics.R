#' plot.population
#'
#' plot one or multiple given populations
#'
#' @param list a list of populations objects
#' @param capacity.line TRUE/FALSE if a dashed line must be drawned for each populations
#' @param log.time TRUE/FALSE if time axis (x axis) must be logarithmic
#' @param log.pop TRUE/FALSE if population axis (y axis) must be logarithmic
#' @param color a color vector for each population
#' @param ... every argument for the \code{\link[graphics]{plot}} function such as graphical parameters for labels. Does NOT apply for lines and capacity ablines.
#'
#' @seealso \code{\link[graphics]{plot}} and \code{\link[graphics]{par}} for graphical parameter that were omitted on this documentation
#'
#' @import graphics
#'
#' @examples hirsu = population("hirsuta",20,1,100)
#' daonen = population("daonensis",30,1.2,100)
#'
#' daonen[[2]]=c(30,34)
#' daonen[[3]]=c(0,5)
#'
#' liste = list(hirsu,daonen)
#' plot_population(list = liste, log.pop = T, color = c(3,2), xlab = "Time")
#'
#' @author Jaunatre Maxime
#'
#' @export
plot.population = function(pop,
                           capacity.line = F,
                           log.time = F,
                           log.pop = F,
                           color = c("black","red"),
                           ...) {
  cat('function not working at this moment')
  #UseMethod('plot_population',pop)
}



