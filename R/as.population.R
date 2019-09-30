#' The fonction *as.population* takes as argument a list
#' containing the population caracteristics (population,
#' number at initial time, growth rates or birth rate and
#' death rate) or a poputaion monitored throught time.
#' And return the inoramtion as a list duable by BeePODyna package

#' @param object if it is list of populations objects (information on the population)
#' must be in the same semantic as population. It could be
#' a dataframe containing the evolution of the population throught time.

#' @return If *object* is a list, the function will test if it match with the
#'  the requirements of a class *population* object.
#'  If *object* is a data frame the function will determine the growth rate,
#'  and form a list of class *population* containing the parameter of the population*.

#' @export

as.population = function(object){

  object = object

# Check list element
  if(class(object) == "list"){
    check.pop = is.population(object)

    if(check.pop == TRUE){

       print("The object ",object," is already a population")
      return(object)
    }
    if(check.pop == FALSE){

      population(object)


    }

  }

# If object is a data frame

   df.object = as.data.frame(object)
  colnames(df.object) = c("time","pop") # /!\ DF must be in the right order

## Find the intial population :

  init = df.object$pop[which(df.object$time == 0)]

## Find the growth rate

  ### (too?) simple way :
  gr.tempo = numeric(length(df.object$time))

  for (i in c(1:length(df.object$time)-1)) {

    gr.tempo[i] = (object$pop[i+1]-object$pop[i])/(df.object$time[i+1]-df.object$time[i])

  }

  approx.gr = mean(gr.tempo)

  df.GR = as.data.frame(cbind(df.object$time,df.object$pop,gr.tempo)) #make a DF with the time and the growth rate
  colnames(df.GR) = c("time","pop","growth_rate")

  ### maybe try to fit a model on the data and extract the GR,

## Find the limit capacity : (where pop is stable ie. GR = 0)

  for (i in length(df.GR$time)) {

    if(sum(df.GR$growth_rates[i],df.GR$growth_rates[i+1]) == 0 ){

      K = df.GR$pop[i]
    }

     }

  }




