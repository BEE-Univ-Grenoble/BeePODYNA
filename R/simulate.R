simulate_onestep <- function(beepodyna) {
  # récuperer le nombre d'espèce dans la communauté
  # Iterer sur toutes les espèces
    beepodyna$community <- beepodyna$functions[[i]](i,
                                                    beepodyna$community,
                                                    beepodyna$interactions)

  # retourne beepodina
}

exponential_growth <- function(species, community, interactions) {
  # recuperer l'espece cible
    # l'effectif pour cette espece
    # calculer le nouvelle effect : effiectif courant * growth_rate
    # effectif ranger le nouvel effectif

  # retourne la communauté
}

simulate_n_pop_dynamic <- function(beepodyna,n) {
  # appel n  fois  simulate_onestep
}