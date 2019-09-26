# To determinate if the entity is a population or not 

is.population  = function(population)
  if (population$class = "population"){
    print("TRUE")
  }
 else {
  print("FALSE")
  }
}