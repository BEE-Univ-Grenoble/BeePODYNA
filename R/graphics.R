#' @import graphics

#' plot_population
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
#' @examples
#' hirsu = population("hirsuta",20,1,100)
#' daonen = population("daonensis",30,1.2,100)
#'
#' daonen$size <- c(30,34)
#' daonen$time <- c(0,5)
#'
#' liste = list(hirsu,daonen)
#' plot_population(list = liste, log.pop = T, color = c(3,2), xlab = "Time")
#'
#' @seealso \code{\link[graphics]{plot}}
#'
#' @author Jaunatre Maxime
#'
#' @export
plot_population = function(list = population("Primula",1,1,100),
                           capacity.line = F,
                           log.time = F,
                           log.pop = F,
                           color = c("black","red"),
                           ...) {
# check each parameter
  if( !is.logical(capacity.line)){
    stop( paste("capacity.line must be a single logical value"))
  }

  # if( !is.logical(log.time)){
  #   stop( paste("log.tome must be a single logical value"))
  # }

  if( !is.logical(log.pop)){
    stop( paste("log.pop must be a single logical value"))
  }

#### color ####
# how to check for a color? can be numeric, character etc...???
  # if( !is.character(color)){
  #   stop( paste("color must be a vector of character strings, with colors for a plot."))
  # }

# check each population
  # check.pop = rep( F, length( list ) )
  # # false.pop = c()
  # for(i in 1 : length( list )){
  #   check.pop[i] = is.population( list[[i]] )
  #   # if(check.pop[i] == F) { false.pop = c( false.pop, i ) }
  # }
  #
  # if( sum(check.pop -1) < 0){
  # # if( length(false.pop) > 0 ){
  #   stop( paste("The list must contain only populations"))
  # }

# log plot

  if(!log.time | !log.pop){
    for(i in 1:length(list)){
      if(log.pop){
        list[[i]][[2]] = log(list[[i]][[2]])
        list[[i]][[4]] = log(list[[i]][[4]])
      }else{}

      ####log ####
      #BESOIN DE COMMENCER A T = 1!!! ou alors pas de log en temps
      # if(log.time){
      #   list[[i]][[3]] = log(list[[i]][[3]])
      # }else{}
    }
  }


# set graphical parameters
  pop.min = min( unlist( sapply( list, "[[", 2 )))
  pop.max = max( unlist( sapply( list, "[[", 2 )))
  y.dist = pop.max - pop.min
  pop.min = pop.min - y.dist * 0.1
  pop.max = pop.max + y.dist * 0.1

  time.min = min( unlist( sapply( list, "[[", 3 )))
  time.max = max( unlist( sapply( list, "[[", 3 )))
  x.dist = time.max - time.min
  time.min = time.min - x.dist * 0.1
  time.max = time.max + x.dist * 0.1

  plot(x= 1, y =1, type = "n",
       xlim = c(time.min,time.max),
       ylim = c(pop.min,pop.max),
       ... = ...)

# plot ablines
  if(capacity.line){
    print("plotting capacity lines")
    capacity = sapply( liste, "[[", 5 )

    for(i in 1 : length(capacity) ){
      abline(h = capacity[i], col = color[i], lty = 4)
    }
  }

# plot populations lignes
  for(i in 1 : length(list) ){
    print(i)
    lines(x= list[[i]][[3]], y = list[[i]][[2]],
          col = color[i], type = 'b')
  }

}