
# Hudson data preparation
library(BeePODYNA)
#file at : https://github.com/stan-dev/example-models/blob/master/knitr/lotka-volterra/hudson-bay-lynx-hare.csv
Hudson <- read.csv("raw_data/hudson-bay-lynx-hare.csv",
           comment.char="#")


lynx = population("lynx",4,1,60)
hare = population("hare",30,2,80)
lynx[[2]]=Hudson$Lynx
hare[[2]]=Hudson$Hare
lynx[[3]] = hare[[3]] = Hudson$Year
hudson = community('Hudson',hare,lynx)

usethis::use_data(hudson, overwrite = TRUE)

# Sweden data preparation
# further readings https://www.nature.com/scitable/knowledge/library/dynamics-of-predation-13229468/

red_fox = population("red_fox",10,1.2,50)
vole = population("vole",100,2,200)
gouse = population("Gouse",30,1.5,80)
Sweden = community('Sweden',red_fox,vole,gouse)

usethis::use_data(Sweden, overwrite = TRUE)

# Data beepodyna
beepod_sweden = beepodyna(label="beepod_sweden",
                     community = Sweden,
                   interactions=interactions(3),
                     c(exponential_growth,exponential_growth,exponential_growth),
                   verbose = FALSE)
usethis::use_data(beepod_sweden, overwrite = TRUE)

beepod_hudson = beepodyna(label="beepod_hudson",
                          community = hudson,
                          interactions=interactions(2),
                          c(exponential_growth,exponential_growth),
                          verbose = FALSE)

usethis::use_data(beepod_hudson, overwrite = TRUE)

#data("hudson")
#plot(hudson)

#ou alors utiliser le package IBM pour produire les datas!!!
#
# library(ibm)
#
# set.seed(880820)
#
# par = list(alpha = 5e-4,
#            beta = 5e-4,
#            r = 0.1,
#            m = 0.05,
#            D = list(N = 8e-5, P = 8e-5),
#            L = list(N = 0.2, P = 0.2))
#
# N0 = with(par, m / (2 * beta * L$P))
# P0 = with(par, m / (2 * alpha * L$N))
#
# par$initial = list(N = round(N0), P = round(P0))
#
# sim = localLotkaVolterra(par,
#                          T = 1000,
#                          replicate = 1,
#                          maxpop = 1e4)
#
# plot(sim)
