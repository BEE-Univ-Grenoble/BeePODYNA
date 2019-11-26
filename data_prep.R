
# Hudson data preparation
library(BeePODYNA)
#file at : https://github.com/stan-dev/example-models/blob/master/knitr/lotka-volterra/hudson-bay-lynx-hare.csv
Hudson <-
  read.csv("raw_data/hudson-bay-lynx-hare.csv",
           comment.char="#")

lynx = population("lynx",4,1,60)
hare = population("hare",30,2,80)
lynx[[2]]=Hudson$Lynx
hare[[2]]=Hudson$Hare
lynx[[3]] = hare[[3]] = Hudson$Year
hudson = community('Hudson',hare,lynx)

usethis::use_data(hudson, overwrite = TRUE)
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
