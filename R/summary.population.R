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
#'  summary.population(pop,11)
#'  summary.population(pop,getOption("digits")+2)
#'
#'  # Several generations and population lists
#'  pop1 = population("eva", 1, 0.8, 500)
#'  pop2 = population("teo", 1, 0.7, 850)
#'  pop2$size = c(1,50,750)
#'
#'  pops = list(pop1, pop2)
#'
#'  summary.population(pop2)
#'  summary.population(pops)
#'
#'
#'
#' @export

summary.population <- function(populations,
                               d.printing = getOption("digits")){

  if( !is.numeric(d.printing)||!is.integer(d.printing) ) stop ("'d.printing' parameter must be an integer number")

  if(is.list(pop)){

    summaries = list()
    for (i in 1:length(pop)){

      pop = populations[[i]]

      if(is.population(pop)){

        summaries[[i]] = pop


        #[[1]] Label
        label = pop$label; names(summaries[[i]]) = label
        cat("[[1]] - Population label:  ", toString(label, width = 50), "\n", "\n")



        #[[2]] Size(s)
        cat("[[2]] - Population size:  ", "\n")

        if(length(pop$size)>1){

          sizes = c(pop$size[length(pop$size)], pop$size[length(pop$size)-1], pop$size[1])

          data.s = data.frame("Generations" = c("current", "previous generation", "initial"),
                              "Size" = sizes, stringsAsFactors = F)

          print(data.s, justify = "none", right= F, row.names = c("[n]", "[n-1]", "[n0]"))

        }else{

          data.s = data.frame("Generations" = "current", "Size" = pop$size, stringsAsFactors = F)
          print(data.s, justify = "none", right= F, row.names = c("[n]"))
        }

        summaries[[2]] = data.s #; names(summary[2]) = "Sizes"
        cat("\n", "\n")

        #[[3]] Generations
        cat("[[3]] - Generations:  ", "\n")
        cat("This population has subsisted for ", pop$time[length(pop$time)], " generations.")

        cat("\n", "\n")


        #[[4]] Growth rate(s)
        cat("[[4]] - Growth rates:  ", "\n")

        if(length(pop$size)>1){

          gr.current = as.numeric(sizes[length(sizes)]/sizes[length(sizes)-1])
          gr.previous = as.numeric(sizes[length(sizes)-1]/pop$size[length(pop$size)-2])
          gr.initial = pop$growth_rate[1]

          rates = c(gr.current, gr.previous,gr.initial)

          data.r = data.frame("Generations" = c("current", "previous generation", "initial"),
                              "Rates" = rates, stringsAsFactors = F)

          print(data.r,
                justify = "none",
                right= F,
                row.names = c("[n]", "[n-1]", "[n0]"),
                digits = d.printing)


        }else{

          data.r = data.frame("Generations" = "current", "Rate" = pop$growth_rate, stringsAsFactors = F)
          print(data.r, justify = "none", right= F, row.names = c("[n]"))
        }

        summaries[[4]] = data.r #; names(summary[3]) = "Rates"
        cat("\n", "\n")



        #[[4]] Biotic capacity
        cat("[[4]] - Biotic capacity:  ", "\n")

        if(is.infinite(pop$capacity)){

          cat("\t", "No biotic capacity was specified.", "\n")
        } else{

          N_K = pop$size[length(pop$size)]/pop$capacity*100

          cat("Biotic capacity is = ", pop$capacity, "\n")
          cat("Population has reached the ", format(N_K, digits = 2), "%  of it at generation 'n'.", "\n")
        }


        cat("\n", "\n")




      }else{

        stop("Your 'pop' argument does not contain a 'population' class object or a list of 'population' objects")
      }

    } # end of "for" loop


  } else{

    stop("Your 'pop' argument does not contain a 'population' class object or a list of 'population' objects")
  }

  return(summaries)

}
