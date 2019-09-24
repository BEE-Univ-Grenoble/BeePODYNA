# BeePODYNA

## Aims

## Models

- 1D
  + Linéaire
  + non linéaire
- 2D
  + Linéaire
  + non linéaire
  
### Description d'une population

Une population est définie par :

- Un effectif variable au cours du temps
- Une serie de paramètres constants:
  + Taux de natalité
  + Taux de mortalité
  + Capacité biotique
  
#### Fonctions utilitaires

- is.population
- growth_rate_estimate
- as.population (à partir d'une liste, à partir d'une population)
- Récuperation des attibuts:
  + size (taille de la pop et temps associé) 
  + capacity 
  + growth_rate ()
- summary (julia)
- plot (maxime)

### Description d'un modèle

IMPLEMENTER LA FONCTION dynapop (modèle de dynamic des pop)

- Une liste de populations
- Une matrice d'interactions
- Une liste de meme longueur d'equations (fonction)
  + Arguments :
    + Ensemble des populations
    + La matrice d'interaction
  + Retourne :
    Le nouvel effectif de la population

## Functionalities

- Approche eulèrienne
- Représentations graphiques
- Résolution
- simulation
- Fonction d'analyse automatique

Regarder shiny pour faire une interface interactive
