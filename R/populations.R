#' population
#'
#' @export
population <- function(label,
                       initial_size,
                       growth_rate,
                       capacity    = Inf) {


  if (!is.character(label) ||
      length(label) > 1 )
    stop("Label must be a single charactere string")

  if (!is.numeric(initial_size) ||
      length(initial_size) > 1  ||
      initial_size < 0
     )
    stop("Population size must be a single positive number")

  if (!is.numeric(growth_rate) ||
      length(growth_rate) > 1 )
    stop("Growth rate size must be a single number")

  if (!is.numeric(capacity) ||
      length(capacity) > 1  ||
      capacity <= 0)
    stop("Capacity size must be a single strictly positive number")

  structure(list(label = label,
            size  = initial_size,
            time  = 0.0,
            growth_rate = growth_rate,
            capacity = capacity),
            class = "population"
            )
}

names.population <- function(pop) {
  pop$label
}




growth_rate <- function(birth_rate,death_rate) {

}

