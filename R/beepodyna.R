#' BeePODYNA
#'
#' A package to study population dynamic.
#'
#' @docType package
#' @name beepodyna
NULL



#' @export
beepodyna <- function(community,
                      interactions = interactions(length(community)),
                      functions = identity,
                      verbose = TRUE) {

  ### check the class of the parameters

  if (class(community) != "community") {
    stop("The community given is not of the class community.")
  }
  if (class(interactions) != "interactions") {
    stop("The interactions matrix given is not of the class interactions")
  }
  if (class(functions) != "function" && class(functions) != "list") {
    stop("The functions given are not a single function or a list.")
  }
  if (class(functions == "list")) {
    for (l in 1:length(functions)) {
      if (class(functions[[l]]) != "function") {
        stop(sprintf("The %d element of the functions list in not a function."))
      }
    }
  }

  ### check the length of the parameters

  nb_pop <- length(community)

  if (dim(interactions)[1] != nb_pop) {
    stop("All the parameters haven't the same size.")
  }

  if (length(functions) > nb_pop) {
    if (verbose) {
      warning("The functions list is too long comparing to the number of populations. The last functions of the list are not used.")
    }
    functions <- functions[1:nb_pop]
  }

  if (length(functions) < nb_pop) {
    if (verbose) {
      warning("The functions list has been repeted since its length doesn't match the number of population.")
    }
    functions <- rep(list(functions), ceiling(nb_pop / length(functions)))[1:nb_pop]
  }

  ### Create the beepodyna object

  beepodyna <- list(
    community = community,
    interactions = interactions,
    functions = functions
  )
  class(beepodyna) <- "beepodyna"
  return(beepodyna)

  #pb length(hudson) !!!
}