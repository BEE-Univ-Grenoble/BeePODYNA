#' Lynx and Hare Pelts in Canada
#'
#' A dataset containing a canadian community of hares and lynxs, represented by the number (in thousands) of pelt.
#' It represent a classical exemple in dynamics model, and was first analyzed by biologist Charles Gordon Hewitt.
#'
#' Howard (2009) provides numerical data for the number of pelts collected by the Hudson’s Bay Company
#' in the years 1900-1920, which we have included in comma-separated value (CSV) form in the source
#' repository with the case study.
#'
#' Ref : Howard, P. (2009). Modeling basics. Lecture Notes for Math 442, Texas A&M University.
#'
#' @format A Beepodyna object with 1 community of 2 populations:
#' \describe{
#'   \item{hudson}{the label of the community}
#'   \item{interaction}{matrix of interaction, here 0 interaction}
#'   \item{populations}{the list of two populations}
#'   \itemize{
#'   \item{hare : the population of hares, interpolated from the number of pelts}
#'   \item{lynx : the population of hares, interpolated from the number of pelts}
#'   }
#'
#' }
#'
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#'
#' @source \url{https://mc-stan.org/users/documentation/case-studies/lotka-volterra-predator-prey.html#data-lynx-and-hare-pelts-in-canada}
"beepod_hudson"

#' hudson
#'
#' Community hudson
#' @examples
#' data(hudson)
#' plot(hudson)
#' summary(hudson)
#' @seealso
#' \code{\link[BeePODYNA]{beepod_hudson}} for description of the dataset.
#'
"hudson"

#' Data Sweden : 3 populations in iteraction in Sweden
#'
#' 3 populations in interactions have been reported between red foxes its major prey voles and
#' most rarely grouve if voles are not available. Here is a classical case of bottom-up control.
#'
#' Red fox population is characterized by low growth rate and a little capacity.
#' Gouse population is characterized by a medium growth rate and a medium capacity.
#' Vole population is characterized by a high growth rate and a high capacity.
#'
#' @format A Beepodyna object of 1 community with 3 populations:
#' \describe{
#'   \item{Sweden}{the label of the community}
#'   \item{interaction}{the matrix of interaction of the 3 populations, here 0 interaction}
#'  \item{populations}{the list of two populations}
#'    \itemize{
#'    \item{red_fox : the population of red, a list of : growth rate, biotic capacity,initial size}
#'    \item{gouse : the population of gouse, a list of : growth rate, biotic capacity,initial size}
#'    \item{vole : the population of vole, a list of : growth rate, biotic capacity,initial size}
#'   }
#'
#' }
#' @source \url{https://www.nature.com/scitable/knowledge/library/dynamics-of-predation-13229468/}
#'
#' @author Leroy Martial <martial.leroy@etu.univ-grenoble-alpes.fr>
"beepod_sweden"

#' sweden
#'
#' Community sweden
#' @examples
#' data(sweden)
#' plot(sweden)
#' summary(sweden)
#' @seealso
#' \code{\link[BeePODYNA]{beepod_sweden}} for description of the dataset.
"sweden"