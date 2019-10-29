decisionTree <- function(attribute, value, childL=NULL, childR=NULL) {
  tree <- list(
    attribute=attribute,
    value=value,
    childL=childL,
    childR=childR
  )
  class(tree) = "decisionTree"
  return(tree)
}

isLeaf <- function(obj) {
  UseMethod("isLeaf", obj)
}

isLeaf.default <- function(obj) {
  print("default function")
}

isLeaf.decisionTree <- function(tree) {
  return(is.null(tree$childL) & is.null(tree$childR))
}


