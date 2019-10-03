#' community
#'
#' Create a community with giving its name and a minimum of one population.
#'
#' @usage
#' community(label = "label", population = first_pop)
#' community(label = "label", population = first_pop , pop2 = second_pop)
#' community(label = "label", population = first_pop , second_pop)
#'
#' @param label the name of the community (character string)
#' @param population a population object
#' @param ... additionnal populations objects. All other objects will be rejected and a warning will be print.
#'
#' @details There is no need to put a name as shown in the first line of \code{Usage}, because each element of community will be named after the label which is in every population object you provide.
#'
#' @seealso \code{\link[BeePODYNA]{population}} to see how to make an object of class population.
#'
#' @examples
#'hirsu = population("hirsuta",20,1,100)
#'daonen = population("daonensis",30,1.2,100)
#'
#'daonen[[2]]=c(30,34)
#'daonen[[3]]=c(0,5)
#'
#'alpha = community('alpha',hirsu,daonen)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
community <- function(label,
                      population,
                      ...) {
  if (!is.character(label) ||
    length(label) > 1) {
    stop("Label must be a single charactere string")
  }

  if (!is.population(population)) {
    stop("population must be an object of classe population")
  }

  liste <- list(...)

  if (length(liste) > 0) {
    not_pop_label <- FALSE
    pop_id <- c()
    pop_name <- population$label

    for (i in 1:length(liste)) {
      if (!is.population(liste[[i]])) {
        not_pop_label <- TRUE
      } else {
        pop_id <- c(pop_id, i)
        pop_name <- c(pop_name, liste[[i]]$label)
      }
    }

    if (not_pop_label == TRUE) {
      warning(sprintf(
        "Check other objects class, only '%s' objects are in '%s' community because of their population class",
        paste(pop_name, collapse = ", "), label
      ))
    }
    begin <- list(
      label = label,
      population = population
    )
    end <- liste[pop_id]

    community <- structure(c(begin, end),
      class = "community"
    )
  } else {
    community <- structure(list(
      label = label,
      population = population
    ),
    class = "community"
    )
  }

  names(community)[2] <- community[[2]][[1]]

  return(community)
}


#' is.community
#'
#' Check if the object is a community or not.
#'
#' @usage
#' community(label = "label", population = first_pop)
#' community(label = "label", population = first_pop , pop2 = second_pop)
#' community(label = "label", population = first_pop , second_pop)
#'
#' @param label the name of the community (character string)
#' @param population a population object
#' @param ... additionnal populations objects. All other objects will be rejected and a warning will be print.
#'
#' @details There is no need to put a name as shown in the first line of \code{Usage}, because each element of community will be named after the label which is in every population object you provide.
#'
#' @seealso \code{\link[BeePODYNA]{population}} to see how to make an object of class population.
#'
#' @examples
#'hirsu = population("hirsuta",20,1,100)
#'daonen = population("daonensis",30,1.2,100)
#'
#'daonen[[2]]=c(30,34)
#'daonen[[3]]=c(0,5)
#'
#'alpha = community('alpha',hirsu,daonen)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
#' Is.Community
#' The function determinines if the object is a population or not
#' @param x the object which must be a community to validate the condition
#' @return a logical "TRUE" or "FALSE"
#' @example
#' @author Jaunatre Maxime
#' @export
is.community <- function(x){
  if (class(x) == "community"){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}