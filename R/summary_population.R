#' Populations summaries and trends
#'#' @author Julia Guerra <jguerra_1995@hotmail.es>
#'
#'
#' This function will allow the user to easily read
#' trends and characteristics (label, population size(s), growth rate(s)...) of a
#' population-class object.
#'
#' @usage
#' summary(population, d.print)
#'
#' @param population an object of class "population"
#' @param d.print the number of decimal digits to be printed in statistics
#'
#' @return
#' A list containing calculated summaries for a population. See 'examples'.
#'
#' @examples
#' # Default usage
#' pop1 = population("mypop", 1, 0.8, 500)
#' summary(pop1)
#'
#'
#'
#'
#' # long history populations
#' data(hudson)
#' pop1 = hudson$hare
#' sumrs1 = summary(pop1)   #for saving summaries list
#'
#' sumrs1$Rates_of_change        # taking a look into last growth tendencies
#'
#''
#'
#' # In case you want to modify digits printing settings
#'
#' summary(pop1,11)
#' summary(pop1,getOption("digits")+2)
#'
#' @rdname summary
#' @export

summary.population <- function(populations,
                               d.print = as.numeric(getOption("digits"))) {
  if (!is.numeric(d.print)) stop("'d.print' parameter must be an integer number")

  if (!is.integer(d.print)) d.print <- as.numeric(d.print)

  if(!is_population(populations)) stop("Your 'pop' argument does not contain a 'population' class object ")


  pop <- populations

  summaries_sub <- pop # creating sub-object for saving data; will work for each population

  # [[1]] Label - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  label <- pop$label
  summaries_sub[[1]] <- label
  cat(
    "[[1]] - Population label:  ",
    toString(label, width = 50), "\n", "\n"
  )



  # [[2]] Size(s) - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[2]] - Population size:  ", "\n")
  gens <- as.numeric(length(pop$size)) # number of generations
  sizes <- rev(pop$size) # printing order

  ##### Only one time step .......................................
  if (gens == 1) { # current generation is initial generation

    # dataframe creation
    data.s <- data.frame(
      "Generations" = "n0",
      "Size" = sizes, stringsAsFactors = F
    )
  }


  ##### Only two time steps .......................................
  if (gens == 2) {

    # dataframe creation
    data.s <- data.frame(
      "Generations" =
        c("n", "n0"),
      "Size" = sizes, stringsAsFactors = F
    )
  }


  ##### More than two time steps .......................................
  if (gens > 2) {


    # creating prints for generations from current generation n to n-5 (if exists) and n0

    if (gens >= 7) {
      nrep <- 5 # for generation indexing
      nsize <- sizes[c(1:6, length(sizes))] # for population sizes
    } else {
      nrep <- gens - 2 # for generation indexing
      nsize <- sizes # for population sizes
    }


    names.all.s <- mapply(paste,
                          rep("n-", nrep),
                          1:nrep,
                          sep = ""
    )

    # dataframe creation
    data.s <- data.frame(
      "Generations" = c("n", names.all.s, "n0"),
      "Size" = nsize,
      stringsAsFactors = F
    )
  }

  ## ....... printing results #
  print(data.s,
        justify = "none",
        right = T,
        row.names = rep("#", nrow(data.s))
  )

  # saving object
  summaries_sub[[2]] <- data.s
  cat("\n", "\n")



  # [[3]] Nb of time steps passed by = nb of generations - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[3]] - Generations:  ", "\n")
  cat(
    "This population has subsisted for ",
    length(pop$time), " generations."
  )

  cat("\n", "\n")


  # [[4]] Growth rate(s) - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[4]] - Rates of population change between generations:  ", "\n")

  ##### Only one time step .......................................
  if(gens == 1){

    # dataframe creation
    data.ch = data.frame(
      "Generations" = "n0",
      "Rates of change" = "-",
      "Sizes" = pop$size,
      stringsAsFactors = F)

  }

  ##### Only two time steps .......................................
  if (gens == 2) {
    names.ch <- c("n1 over [ n0 ]" , "[ n0 ]") # for indexing
    ch <- as.numeric(sizes[1] / sizes[2])

    # dataframe creation
    data.ch <- data.frame(
      "Generations" = names.ch,
      "Rates of change" = c("-", ch),
      "Sizes" = c(nsize[1:2]),
      stringsAsFactors = F)
  }

  ##### More than two time steps .......................................
  if (gens > 2) {

    toto <- 1:(length(names.all.s)-1) # indexing
    for (e in 1:length(toto)) {
      if(!is.na(names.all.s[e+1])){
        toto[e] <- paste(names.all.s[e], "over [", names.all.s[e + 1], "]")
      } else{ toto[e] = NA}}

    names.ch <- c( "[ n ]", "n over [ n-1 ]", toto[!is.na(toto)], "(n1 over [ n0 ])"  )

    ch <- nsize[1:(length(nsize) - 1)] # calculating rates of change
    for (i in 1:(length(ch) - 1)) {
      ch[i] <- as.numeric(ch[i] / ch[i + 1])
    }



    if(gens >= 7){

      ch <- c(ch[1:5], as.numeric(sizes[length(sizes) - 1] / sizes[length(sizes)])) # including n0

      data.ch <- data.frame(
        "Generations" = names.ch,
        "Rates of change" = c("-", ch),
        "Sizes" = c(nsize[1:5], "...", nsize[length(nsize)]),
        stringsAsFactors = F)

    }else{

      ch <- c(ch[1:(length(ch)-1)], as.numeric(sizes[length(sizes) - 1] / sizes[length(sizes)])) # including n0

      data.ch <- data.frame(
        "Generations" = names.ch,
        "Rates of change" = c("-", ch),
        "Sizes" = c(nsize[1:3], "...", nsize[length(nsize)]),
        stringsAsFactors = F)
    }

  }

  ## ....... printing results #
  print(data.ch,
        justify = "none",
        right = T,
        row.names = rep("#", nrow(data.ch)),
        digits = d.print
  )


  # saving object
  summaries_sub[[4]] <- data.ch
  names(summaries_sub)[4] <- "Rates_of_change"
  cat("\n", "\n")



  # [[4]] Biotic capacity - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[5]] - Biotic capacity:  ", "\n")

  if (is.infinite(pop$capacity)) {
    cat("\t", "No biotic capacity was specified.", "\n")
  } else {
    N_K <- pop$size[length(pop$size)] / pop$capacity * 100

    cat("Biotic capacity is = ", pop$capacity, "individuals", "\n")
    cat(
      "Population has reached ",
      format(N_K, digits = 2), "%  of it at generation 'n'.", "\n"
    )
  }


  cat("\n")
  cat(rep("-", getOption("width")))
  cat("\n")



  invisible(summaries_sub)
}

#' Communities summaries and trends
#'#' @author Julia Guerra <jguerra_1995@hotmail.es>
#'
#'
#' This function will allow the user to easily read
#' trends and characteristics (label, population size(s), growth rate(s)...) of one
#' community-class object.
#'
#' @usage
#' summary(community, d.print)
#'
#' @param community an object of class "community"
#' @param d.print the number of decimal digits to be printed in statistics
#'
#' @return
#' A list containing calculated summaries for a community. See 'examples'.
#'
#' @examples
#' # Default usage
#' pop1 = population("mypop", 1, 0.8, 500)
#' comm1 = community("toto", pop1,pop1)
#' summary(comm1)
#'
#'
#' # long history community
#' data(hudson)
#' sumrsc = summary(hudson)
#'
#' sumrsc$hare$Rates_of_change   # same for community object
#'
#''
#'
#'# In case you want to modify digits printing settings
#'
#' summary(hudson,11)
#'
#' @rdname summary
#' @export

summary.community <- function(community,d.print = as.numeric(getOption("digits"))) {

  if (!is.numeric(d.print)) stop("'d.print' parameter must be an integer number")

  if (!is.integer(d.print)) d.print <- as.numeric(d.print)

  if (!is_community(community)) stop("Your 'community' argument does not contain a 'community' class object")

  subpops <- community$populations
  summaries_sub <- subpops[[1]] # creating sub-object for saving all data


  # [[1]] Label - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  label <- community$label
  summaries_sub[[1]] <- label
  cat(
    "[[1]] - Community label:  ",
    toString(label, width = 50), "\n", "\n"
  )



  # [[2]] Size(s) - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[2]] - Population size:  ", "\n")

  gens <- 1:length(subpops)
  sizes <- as.list(1:length(subpops))
  names <- names(subpops)

  for(i in 1:length(subpops)){
    gens[i] <- as.numeric(length(subpops[[i]][["size"]])) # number of generations
    sizes[[i]] <- subpops[[i]][["size"]]
  }

  gens1 <- gens[1] # is this risky or not ????????


  ##### Only one time step .......................................
  if (gens1 == 1) { # current generation is initial generation



    # dataframe creation ----
    data.s <- as.data.frame(matrix(nrow = 1, ncol = (1 + length(sizes)),
                                   dimnames = list(c(), c("Generations", names(subpops)))))

    data.s[,1] <- c("[ n0 ]")
    data.s[,2:ncol(data.s)] <- as.vector(lapply(X = sizes, FUN = '[') ) #            TRY THIS !!!!!!!!!!!!!!!!!!!!!

  }


  ##### Only two time steps .......................................
  if (gens1 == 2) {

    # dataframe creation ----
    data.s <- as.data.frame(matrix(nrow = 2, ncol = (1 + length(sizes)),
                                   dimnames = list(c(), c("Generations", names(subpops)))))

    data.s[,1] <- c("n","[ n0 ]")
    data.s[,2:ncol(data.s)] <- as.vector(lapply(X = sizes, FUN = '[') ) #            TRY THIS !!!!!!!!!!!!!!!!!!!!!

  }


  ##### More than two time steps .......................................
  if (gens1 > 2) {


    # creating prints for generations from current generation n to n-5 (if exists) and n0

    if (gens1 >= 7) {
      nrep <- 5 # for generation indexing
      nsize = lapply(X = sizes, FUN = function(list){ list[c(1:6, length(list))]}) # for population sizes
      genslabel <- subpops[[1]][["time"]][c(1:6, length(subpops[[1]][["time"]]))] # for time indexing

      names.all.s <- mapply(paste,
                            rep("[ n-", nrep),
                            1:nrep, " ]",
                            sep = ""
      )

      # dataframe creation ----
      data.s <- as.data.frame(matrix(nrow = 7, ncol = (1 + length(sizes)),
                                     dimnames = list(c(), c("Generations", names(subpops)))))


    } else {

      nrep <- gens1 - 2 # for generation indexing
      nsize <- lapply(X = sizes, FUN = function(list){ list[]})
      genslabel <- subpops[[1]][["time"]] # for time indexing


      names.all.s <- mapply(paste,
                            rep("[ n-", nrep),
                            1:nrep, " ]",
                            sep = "" )

      # dataframe creation ----
      data.s <- as.data.frame(matrix(nrow = gens1, ncol = (1 + length(sizes)),
                                     dimnames = list(c(), c("Generations", names(subpops)))))
    }


    data.s[,1] <- c("[ n ]", names.all.s, "[ n0 ]")
    data.s[,2:ncol(data.s)] <-  as.vector(lapply(X = nsize, FUN = '[') )
  }

  ## ....... printing results #
  print(data.s,
        justify = "none",
        right = T,
        row.names = rep("#",nrow(data.s)))


  # saving object
  summaries_sub[[2]] <- data.s
  cat("\n", "\n")


  # [[3]] Nb of time steps passed by = nb of generations - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[3]] - Generations:  ", "\n")
  cat(
    "This community has subsisted for ",
    gens1, " generations."
  )

  cat("\n", "\n")


  # [[4]] Growth rate(s) - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[4]] - Rates of population change between generations:  ", "\n")

  ##### Only one time step .......................................
  if(gens1 == 1){


    # dataframe creation ----
    data.ch <- as.data.frame(matrix(nrow = 1, ncol = (2 + length(sizes)),
                                    dimnames = list(c(), c("Generations", "Rates of change for...", names(subpops)))))

    data.ch[,1] <- subpops[[1]]$time
    data.ch[,2] <- c("n0")
    data.ch[,3:ncol(data.ch)] <- as.vector(lapply(X = sizes, FUN = '[') )


  }

  ##### Only two time steps .......................................
  if (gens1 == 2) {

    names.ch <- c("n1 over [ n0 ]" , "[ n0 ]") # for indexing

    data.ch <- as.data.frame(matrix(nrow = 2, ncol = (2 + length(sizes)),
                                    dimnames = list(c(), c("Generations", "Rates of change for...", names(subpops)))))


    rates_c1 = function(v) {
      ratesv = c()
      for (i in 1:length(v)){
        ratesv[i] = v[i]/v[i+1]
      }
      return(ratesv)}


    data.ch[,1] <- rev(subpops[[1]]$time)
    data.ch[,2]<- names.ch
    data.ch[3:ncol(data.ch)] <- lapply(X = sizes, FUN = rates_c1)



  }

  ##### More than two time steps .......................................
  if (gens1 > 2) {

    toto <- 1:(length(names.all.s)-1) # indexing
    for (e in 1:length(toto)) {
      if(!is.na(names.all.s[e+1])){
        toto[e] <- paste(names.all.s[e], "over [", names.all.s[e + 1], "]")
      } else{ toto[e] = NA}
    }

    names.ch <- c( "[ n ]", "n over [ n-1 ]", toto[!is.na(toto)], "(n1 over [ n0 ])"  )



    if(gens1 >= 7){

      data.ch <- as.data.frame(matrix(nrow = 7, ncol = (2 + length(sizes)),
                                      dimnames = list(c(), c("Generations", "Rates of change for...", names(subpops)))))

      # for filling column of "rates"
      rates_c2 = function(v) {
        ratesv = c(1:7)
        for (i in 1:5){
          ratesv[i+1] = v[i]/v[i+1]
        }
        ratesv[1] = NA
        ratesv[7] = v[length(v)-1]/v[length(v)]
        return(ratesv)
      }

      timing = rev(subpops[[1]]$time)
      data.ch[,1] <-timing[c(1:6,length(timing))]
      data.ch[,2]<- names.ch
      data.ch[,3:ncol(data.ch)] <-  lapply(X = sizes, FUN = rates_c2)



    }else{

      data.ch <- as.data.frame(matrix(nrow = gens1, ncol = (2 + length(sizes)),
                                      dimnames = list(c(), c("Generations", "Rates of change for...", names(subpops)))))
      rates_c3 = function(v,n) {
        ratesv = c(1:n)
        for (i in 1:(n-1)){
          ratesv[i+1] = v[i]/v[i+1]
        }
        ratesv[1] = NA
        ratesv[n] = v[length(v)-1]/v[length(v)]
        return(ratesv)
      }

      timing = rev(subpops[[1]]$time)
      data.ch[,1] <-timing[c(1:(gens1-1),length(timing))]
      data.ch[,2]<- names.ch
      data.ch[,3:ncol(data.ch)] <-  lapply(X = sizes, FUN = rates_c3, n = gens1)

    }

  }

  ## ....... printing results #
  print(data.ch,
        justify = "none",
        right = T,
        row.names = rep("#", nrow(data.ch)),
        digits = d.print
  )


  # saving object
  summaries_sub[[4]] <- data.ch
  names(summaries_sub)[4] <- "Rates_of_change"
  cat("\n", "\n")



  # [[4]] Biotic capacity - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("[[5]] - Biotic capacity:  ", "\n")


  data.cap <- as.data.frame(matrix(nrow = 1, ncol = (1 + length(sizes)),
                                   dimnames = list(c(), c("Reached biotic capacity at [ n ]   ", names(subpops)))))

  capacity_counting = function(list) {
    propr = c()

    if (!is.numeric(list[["capacity"]])  ) {
      cat("\t", "No biotic capacity was specified for population ", list[["label"]], "\n")
      propr = NA
    } else {
      propr <- paste0((list[["size"]][length(list[["size"]])] / list[["capacity"]] ) * 100, " %")
    }

    return(propr)
  }

  data.cap[,1] <- " "
  data.cap[,2:ncol(data.cap)]<- lapply(X = subpops, FUN = capacity_counting)

  print(data.cap,
        justify = "none",
        right = T,
        row.names = rep("#", nrow(data.cap)),
        digits = 3
  )

  cat("\n")
  cat(rep("-", getOption("width")))
  cat("\n")



  invisible(summaries_sub)
}
