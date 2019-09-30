#' Population summaries and trends
#'
#' The *summary.pop* function will allow any user of the BeePODYNA package to easily read
#' trends and characteristics (label, initial size, growth rate) of one or several
#' population-class objects. When dealing with a list of population-type objects, il will
#' recursively print the summaries for each of them.
#'
#' @param pop an object of class "population"
#' @param d.printing the number of decimal digits to be printed in statistics
#'
#' @examples
#' # Default usage
#'   summary.population(pop)
#'
#'#' # You might want to modify digits' printing settings
#'  summary.population(pop),11)
#'  summary.population(pop),getOption("digits")+2)
#' @export
summary.population <- function(pop,
                               d.printing = getOption("digits")){

    if(is.population(pop)){

      summary = pop

      #[[1]] label
      label = pop$label
      cat("[[1]] - Population label:  ", toString(label, width = 50), "\n", "\n")

      #[[2]] size
      cat("[[2]] - Population size:  ", "\n")

      if(length(pop$size)>1){

        sizes = c(pop$size[length(pop$size)], pop$size[length(pop$size)-1], pop$size[1])

        dataf = data.frame("Generations" = c("current", "previous generation", "initial"),
                           "Size" = sizes, stringsAsFactors = F)

      }else{

        dataf = data.frame("Generations" = "current", "Size" = pop$size, stringsAsFactors = F)
      }

      summary[[2]] = dataf; names(summary[[2]] == "Sizes")

      print(dataf, justify = "none", right= F, row.names = c("n", "n-1", "n0"))

      #[[3]] growth rates







    }else stop("Object to summarize is not a 'population' class object")



    return(summary)

  }



}

