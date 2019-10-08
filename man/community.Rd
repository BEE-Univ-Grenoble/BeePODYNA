% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/community.R
\name{community}
\alias{community}
\title{community}
\usage{
community(label, population, ...)
}
\arguments{
\item{label}{the name of the community (character string)}

\item{population}{a population object}

\item{...}{additionnal populations objects. All other objects will be rejected and a warning will be print.}
}
\description{
Create a community with giving its name and a minimum of one population.
}
\details{
There is no need to put a name for additionnal populations, because each element of community will be named after the label which is in every population object you provide.
}
\examples{
hare = population("hirsuta",30,2,80)
lynx = population("daonensis",4,1.2,60)

hudson = community('hudson',hare,lynx)

}
\seealso{
\code{\link[BeePODYNA]{population}} to see how to make an object of class population.
}
\author{
Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
}