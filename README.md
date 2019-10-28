# Random-Forest-Implementation
Projet d'études pour le cours Advanced Supervised Learning du Master 2 Data Mining de l'Université Lumière Lyon 2 (Sujet à finalité “algorithmique”).

## Présentation
Ce projet consiste en l’étude approdondie des arbres de décision et de leur extension par le biais des forêts aléatoires. Les objectifs sont :

* Implémenter le pseudo-code des arbres décisionnels donné en cours. On attend donc deux fonctions ArbreGeneration et DivisionAttribut. Une seul des deux problèmes, régression ou catégorisation, pourra être considéré.
* Implémenter une fonction RandomForest qui utilise les fonctions précédentes et met en oeuvre les ré-échantillonage sur les individus et sur les variables tels que décrit dans le pseudo-code du cours.
* Présenter dans le rapport un texte synthétique sur les fondements et propriétés des forêts aléatoires. Vous pourrez vous inspirer du cours mais toute autre référence pourra être utilisée pour enrichir le propos.
* Montrer aux travers d’expériences la supériorité des forêts aléatoires sur les arbres de décision permettant ainsi de valider empiriquement les propriétés théoriques discutés en cours sur les méthodes de ré-échantillonage.

Quelques références à titre indicatif que vous compléterez à votre guise et selon les besoins de votre projet : [Breiman, 2001, Rokach, 2010, Breiman, 1996].

## Organisation

### Arbres décisionnels

#### ArbreGeneration

#### DivisionAttribut

##### Régression ou Catégorisation ?

### RandomForest

## entropy ####
exemple avec le dataset myocarde :

myocarde=read.table("http://freakonometrics.free.fr/myocarde.csv", head=TRUE, sep=";")
y=myocarde$PRONO classe=(myocarde[,3]< 19)

entropy <-  function(y,classe){
  T. = table(y,classe)
  nx = apply(T.,2,sum)
  n. = sum(T.)
  pxy = T./matrix(rep(nx,each=2),nrow=2)
  omega = matrix(rep(nx,each=2),nrow=2)/n
  g  = sum(omega*pxy*log(pxy))
  return(g)}
