#' community
#'
#' Create a community with giving its name and a minimum of one population.
#'
#' @usage
#' community(label, population, ...)
#'
#' @param label the name of the community (character string)
#' @param population a population object
#' @param ... additionnal populations objects. All other objects will be rejected and a warning will be print.
#'
#' @details There is no need to put a name for additionnal populations, because each element of community will be named after the label which is in every population object you provide.
#'
#' @seealso \code{\link[BeePODYNA]{population}} to see how to make an object of class population.
#'
#' @examples
#'hare = population("hirsuta",30,2,80)
#'lynx = population("daonensis",4,1.2,60)
#'
#'hudson = community('hudson',hare,lynx)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
community <- function(label,
                      population,
                      ...) {
  # checking entry
  if (!is.character(label) ||
    length(label) > 1) {
    stop("Label must be a single charactere string")
  }

  if (!is.population(population)) {
    stop("population must be an object of classe population")
  }

  # supplement populations
  liste <- list(...)

  if (length(liste) > 0) {
    not_pop_label <- FALSE
    pop_id <- c()
    pop_name <- population$label

    # checking supplement populations class
    for (i in 1:length(liste)) {
      if (!is.population(liste[[i]])) {
        not_pop_label <- TRUE
      } else {
        pop_id <- c(pop_id, i)
        pop_name <- c(pop_name, liste[[i]]$label)
      }
    }

    # not all supplement populations are populations objects
    if (not_pop_label == TRUE) {
      warning(sprintf(
        "Check other objects class, only '%s' objects are in '%s' community because of their population class",
        paste(pop_name, collapse = ", "), label
      ))
    }

    # building the community object (multiple pop)
    begin <- list(
      label = label,
      population = population
    )
    end <- liste[pop_id]

    community <- structure(c(begin, end),
      class = "community"
    )
  } else {
    #building the community object (1 pop)
    community <- structure(list(
      label = label,
      population = population
    ),
    class = "community"
    )
  }
  #renaming the element of the community with populations labels
  names(community) <- sapply( community, "[[", 1 )

  return(community)
}


#' is.community
#'
#' Check if the object is a community or not.
#'
#' @usage
#' is.community(x)
#'
#' @param x the object which must be a community to validate the condition
#'
#' @return a logical "TRUE" or "FALSE"
#'
#' @seealso \code{\link[BeePODYNA]{community}} to see how to make an object of class community.
#'
#' @examples
#'hare = population("hirsuta",30,2,80)
#'lynx = population("daonensis",4,1.2,60)
#'
#'hudson = community('hudson',hare,lynx)
#'is.community(hudson)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
is.community <- function(x){
  if (class(x) == "community"){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}