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

#' @export
simulate_n_pop_dynamic <- function(beepodyna,n) {
  for (i in seq_len(n))
    beepodyna <- simulate_onestep(beepodyna)

  beepodyna
}