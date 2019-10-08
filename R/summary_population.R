#' Population summaries and trends
#'#' @author Julia Guerra <jguerra_1995@hotmail.es>
#'
#'
#' This function will allow the user to easily read
#' trends and characteristics (label, population size(s), growth rate(s)...) of one or several
#' population-class objects. When dealing with a list of population-type objects, il will
#' recursively print the summaries for each one of them.
#'
#' @param pop an object of class "population"
#' @param d.printing the number of decimal digits to be printed in statistics
#'
#' @examples
#'# Default usage
#' pop = population("mypop", 1, 0.8, 500)
#' summary(pop)
#'
#'
#'
#'# Several generations and population lists
#' pop1 = population("eva", 1, 0.8, 500)
#' pop2 = population("teo", 1, 0.7, 850)
#' pop2$size = c(1,50,750)
#'
#' pops = community('pops', pop1, pop2)
#'
#' summary(pop2)
#' summary(pops)
#'
#'
#'
#'# In case you want to modify digits' printing settings
#'
#' summary(pop,11)
#' summary(pop,getOption("digits")+2)
#'
#'
#' @export
summary.population <- function(populations, d.printing = getOption("digits")){

  if(!is.numeric(d.printing)||!is.integer(d.printing) ) stop ("'d.printing' parameter must be an integer number")

  if(!is.community(populations) && !is.population(populations) ) stop ("Your 'pop' argument does not contain a 'population' class object or a list of 'population' objects")

  if(is.community(populations)){

    summaries = as.list(2:length(populations))
    pop = populations[[2]] # # # NEEDS CHECK HERE
#  how to refer to the elements other than the "community" label ; that is community[[1]] ????
  }  else {

    pop = populations

    }


  end = F
  i = 1

  while(end == F) {

    summaries_sub = pop

    #[[1]] Label
    label = pop$label; summaries_sub[[1]] = label
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

    summaries_sub[[2]] = data.s #; names(summary[2]) = "Sizes"
    cat("\n", "\n")

    #[[3]] Generations
    cat("[[3]] - Generations:  ", "\n")
    cat("This population has subsisted for ", pop$time[length(pop$time)], " generations.")

    cat("\n", "\n")


    #[[4]] Growth rate(s)
    cat("[[4]] - Growth rates:  ", "\n")

    if(length(pop$size)>1){

      gr.current = as.numeric(sizes[length(sizes)]/sizes[length(sizes)-1])

      if(length(pop$size) > 2 ){
        gr.previous =as.numeric(sizes[length(sizes)-1])/as.numeric(pop$size[length(pop$size)-2])
      } else {
        gr.previous = gr.current
      }

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

    summaries_sub[[4]] = data.r #; names(summary[3]) = "Rates"
    cat("\n", "\n")



    #[[4]] Biotic capacity
    cat("[[5]] - Biotic capacity:  ", "\n")

    if(is.infinite(pop$capacity)){

      cat("\t", "No biotic capacity was specified.", "\n")

    } else{

      N_K = pop$size[length(pop$size)]/pop$capacity*100

      cat("Biotic capacity is = ", pop$capacity, "\n")
      cat("Population has reached the ", format(N_K, digits = 2), "%  of it at generation 'n'.", "\n")
    }


    cat("\n")
    cat(rep("-", getOption("width")))
    cat("\n")



    if(is.population(populations)) { # this check might not be necessary ?

      summaries = summaries_sub
      end = T

    }else{

      if(is.community(populations)){

        if(i == length(populations)){
          end = T

        }else{

          summaries[[i]] = summaries_sub
          end = F

          if(i+1 != length(populations)){
            i = i+1
            pop = populations[[i+1]]  # MIGHT NEED CHECK HERE AS WELL IF YOU CHANGE LINE 49

          }  else {

            end = T}


          }
      }
    }
}

  invisible(summaries)
}

