library(R.utils)

#' 'mat-interaction' creates an interactions matrix object
#'
#' Creates an interactions matrix object defining the positive and negative interactions between populations.
#' The interaction is not assumed symetrical, so a population can have a different effect on a population than this latest has on the first one.
#' The interaction of a population on itself is equal to 0.
#'
#' @param nb_pop is the number of populations in the model.
#' @param interactions is a vector of length nb_pop*(nb_pop-1) giving the interaction of each population on the other one.
#' Each interaction is a decimal ranging between -1 and 1. A positive value means a positive impact (facilitation) while a negative value means a negative impact (predation, competition).
#' The vector starts with the vector of the interactions of the first population on the other ones (ranging from 2 to the last one, not including itself),
#' and then the vector of the interactions of the second population on the others...
#'
#' @examples
#'   mat_interaction(nb_pop=3, interactions=c(0.2,-0.5, 0.1,0.2,0.3,0.8))
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
mat_interaction <- function(nb_pop,
                            interactions) {

  if (!is.numeric(nb_pop) ||
      as.integer(nb_pop)!=nb_pop ||
      nb_pop < 2 )
    stop("Nb_pop must be an integer superior to 1.")

  if (!is.vector(interactions) ||
      !is.numeric(interactions) ||
      length(interactions)!=nb_pop*(nb_pop-1) ||
      sum(as.integer(interactions< -1))>0 ||
      sum(as.integer(interactions>1))>0
  )
    stop("Interactions vector must be fill with decimals ranging between -1 and 1, and is length must be equal to nb_pop*(nb_pop-1).")

  interactions=insert(interactions,c(0:(nb_pop-1))*nb_pop+1,0)
  dim(interactions)=c(nb_pop,nb_pop)
  interactions=t(interactions)
  rownames(interactions)=paste0("Pop_",c(1:nb_pop))
  colnames(interactions)=rownames(interactions)
  class(interactions)="interaction"
  return(interactions)
}
