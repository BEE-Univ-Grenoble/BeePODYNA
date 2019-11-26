#'Compute one time-step of  evolution of the populations contained in a beepodyna object
#'
#'@param beepodyna an object of class beepodyna
#'
#'@return A beepodyna object, every populations in it will be incremented with one more size value (calculated with the functions and interaction matrix) and time
#'
#'@examples
#'   data(hudson)
#'   beepop=beepodyna(label="model_1",
#'   community=hudson,
#'   interactions=interactions(2),
#'   list(exponential_growth,exponential_growth),
#'   verbose = FALSE
#'   )
#'   bepop1=simulate_onestep(beepop)
#'
#' @export
simulate_onestep <- function(beepodyna) {
  npop <- length(beepodyna)

  for (i in seq_len(npop)) {
    beepodyna$community <- beepodyna$functions[[i]](i,
                                                    beepodyna$community,
                                                    beepodyna$interactions)
  }

  beepodyna
}

#' Function describing an exponential growth, has to bee included in a beepodyda object when creating it
#'
#' @param pop index of the population that will be affected by the function, or the name of the pop
#' @param community a `community` object created with community()
#' @param interactions an `interactions` object created with interactions()
#'
#' @return The function returns a community similare to the one given in `community` but the population selected with `pop` will have one more time index and size (calculed with its growth rate, according to an exponential growth)
#'
#' @examples
#'     data(hudson)
#'     exponential_growth(1,hudson,interactions(2))
#'
#' @export
exponential_growth <- function(pop, community, interactions) {
  target_pop <- community$populations[[pop]]
  last_time  <- target_pop$time[length(target_pop$time)]
  last_size  <- target_pop$size[length(target_pop$size)]

  new_size <- last_size * (1 + target_pop$growth_rate)
  new_time <- last_time + 1

  target_pop$size <- append(target_pop$size, new_size)
  target_pop$time <- append(target_pop$time, new_time)

  community$populations[[pop]] <- target_pop

  community
}

#'Compute n times steps of evolution of the populations contained in a beepodyna object
#'
#'@param beepodyna an object of class beepodyna
#'@param n The number of times steps to be computed
#'
#'@return A beepodyna object, every populations in it will be incremented with n more size values (calculated with the functions and interaction object) and time values
#'
#'@examples
#'    data(hudson)
#'    beepop=beepodyna(label="model_1",
#'    community=hudson,
#'    interactions=interactions(2),
#'    list(exponential_growth,exponential_growth),
#'    verbose = FALSE
#'     )
#'     bepop10=simulate_n_pop_dynamic(beepop,10)
#'
#' @export
simulate_n_pop_dynamic <- function(beepodyna,n) {
  for (i in seq_len(n))
    beepodyna <- simulate_onestep(beepodyna)

  beepodyna
}

#' Function describing a logistical growth, has to bee included in a beepodyda object when creating it
#'
#' @param pop index of the population that will be affected by the function, or the name of the pop
#' @param community a `community` object created with community()
#' @param interactions an `interactions` object created with interactions()
#'
#' @return The function returns a community similare to the one given in `community` but the population selected with `pop` will have one more time index and size (calculed with its growth rate and capacity, according to a logistical growth)
#'
#' @examples
#'     data(hudson)
#'     exponential_growth(1,hudson,interactions(2))
#'
#' @export
logistic_growth <- function(pop, community, interactions) {
  target_pop <- community$populations[[pop]]
  last_time  <- target_pop$time[length(target_pop$time)]
  last_size  <- target_pop$size[length(target_pop$size)]

  new_size <- last_size + last_size * target_pop$growth_rate * (1 - last_size / target_pop$capacity)
  new_time <- last_time + 1

  target_pop$size <- append(target_pop$size, new_size)
  target_pop$time <- append(target_pop$time, new_time)

  community$populations[[pop]] <- target_pop

  community
}