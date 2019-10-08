#' mat_interaction
#'
#' \code{mat_interaction} creates an interactions matrix object defining the positive and negative interactions between populations.
#' The interaction is not assumed symetrical, so a population can have a different effect on a population than this latest has on the first one.
#' The interaction of a population on itself is equal to 0.
#' If no interaction vector is given, the default values are 0.
#'
#' @param nb_pop is the number of populations in the model. The matrix interaction with only one population is set to 0.
#' @param interactions is a vector of length \code{nb_pop*(nb_pop-1)} giving the interaction of each population on the other one.
#' Each interaction is a decimal ranging between -1 and 1. A positive value means a positive impact (facilitation) while a negative value means a negative impact (predation, competition).
#' The vector starts with the vector of the interactions of the first population on the other ones (ranging from 2 to the last one, not including itself),
#' and then the vector of the interactions of the second population on the others...
#'
#' @examples
#'   mat_interaction(nb_pop=3, interactions=c(0.2,-0.5, 0.1,0.2,0.3,0.8))
#'   mat_interaction(nb_pop=2)
#'   mat_interaction(nb_pop=1)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @import R.utils
#'
#' @export
mat_interaction <- function(nb_pop,
                            interactions = rep(0, nb_pop * (nb_pop - 1))) {
  if (nb_pop == 1) {
    warning("As there is only one population, the interaction matrix has only one element equal to 0.")
    interactions <- matrix(0)
    rownames(interactions) <- "Pop_1"
    colnames(interactions) <- rownames(interactions)
    class(interactions) <- "interaction"
  }

  else if (!is.numeric(nb_pop) ||
    as.integer(nb_pop) != nb_pop ||
    nb_pop < 2) {
    stop("Nb_pop must be an integer superior to 1.")
  } else if (!is.vector(interactions) ||
    !is.numeric(interactions) ||
    length(interactions) != nb_pop * (nb_pop - 1) ||
    sum(as.integer(interactions < -1)) > 0 ||
    sum(as.integer(interactions > 1)) > 0
  ) {
    stop("Interactions vector must be fill with decimals ranging between -1 and 1, and is length must be equal to nb_pop*(nb_pop-1).")
  } else {
    interactions <- insert(interactions, c(0:(nb_pop - 1)) * nb_pop + 1, 0)
    dim(interactions) <- c(nb_pop, nb_pop)
    interactions <- t(interactions)
    rownames(interactions) <- paste0("Pop_", c(1:nb_pop))
    colnames(interactions) <- rownames(interactions)
    class(interactions) <- "interaction"
  }
  return(interactions)
}


#' is.interaction
#'
#' \code{is.interaction} returns \code{TRUE} if x is an interaction matrix of the class 'interaction'.
#' It returns \code{FALSE} otherwise.
#' To create such an object you can use the \code{\link[BeePODYNA]{mat_interaction}} function.
#'
#' @param x is an \code{R} object.
#'
#' @examples
#'   is.interaction(3)
#'   x=mat_interaction(nb_pop=3, interactions=c(0.2,-0.5, 0.1,0.2,0.3,0.8))
#'   is.interaction(x)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
is.interaction <- function(x){
  if (class(x) == "interaction"){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}


#' as.interaction
#'
#' \code{as.interaction} transforms a vector in an interactions matrix object defining the positive and negative interactions between populations.
#' The interaction is not assumed symetrical, so a population can have a different effect on a population than this latest has on the first one.
#' The interaction of a population on itself is equal to 0. If no interaction vector is given, the default values are 0.
#' See \code{\link[BeePODYNA]{mat_interaction}} function to create a new interaction matrix from scratch.
#' If the object is of the type \code{numeric}, it should be a vector of numeric of length \code{nb_pop*(nb_pop-1)} giving the interaction of each population on the other one.
#' Each interaction is a decimal ranging between -1 and 1. A positive value means a positive impact like facilitation while a negative value means a negative impact like predation, competition.
#' The vector starts with the vector of the interactions of the first population on the other ones ranging from 2 to the last one, not including itself,
#' and then the vector of the interactions of the second population on the others...
#'
#' @param object is an \code{R} object to transform into an interaction matrix.
#'
#' @examples
#'   as.interaction(c(0.2,-0.5, 0.1,0.2,0.3,0.8))
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @import R.utils
#'
#' @export
as_interaction <- function(object) {
  UseMethod("as_interaction")
}

#' @rdname as_interaction
#' @export
as.interaction <- as_interaction

#' @rdname as_interaction
#' @export
as_interaction.default <- function(object) {
  stop(sprintf("I cannot cast an object of type %s to interaction object", class(object)))
}

#' @rdname as_interaction
#' @export
as_interaction.interaction <- function(object) {
  object
}

#' @rdname as_interaction
#' @export
as_interaction.numeric <- function(object) {
  if (!is.numeric(object) ||
    sum(as.integer(object < -1)) > 0 ||
    sum(as.integer(object > 1)) > 0) {
    stop("Interactions vector must be fill with decimals ranging between -1 and 1, and is length must be equal to nb_pop*(nb_pop-1).")
  }

  nb_pop <- (1 + sqrt(1 + 4 * length(object))) / 2

  if (as.integer(nb_pop) != nb_pop ||
    nb_pop < 2) {
    stop("Interactions vector must be fill with decimals ranging between -1 and 1, and is length must be equal to nb_pop*(nb_pop-1).")
  }

  object <- insert(object, c(0:(nb_pop - 1)) * nb_pop + 1, 0)
  dim(object) <- c(nb_pop, nb_pop)
  object <- t(object)
  rownames(object) <- paste0("Pop_", c(1:nb_pop))
  colnames(object) <- rownames(object)
  class(object) <- "interaction"
  return(object)
}