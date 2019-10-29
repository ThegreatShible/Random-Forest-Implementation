# Returns only a node made of the separation attribute and separation value
decisionNode <- function(attribute=NA, value=NA, class=NA, children=NA) {
  node <- list(
    attribute=attribute, # attribute we check
    value=value, # value(s) to compare to to choose the branch
    class=class, # class prediction if we are done
    children=children
  )
  class(node) = "decisionTree"
  return(node)
}

# Returns entropy of a given dataset and majority class
entropy <- function(Y) {
  nbElements = length(Y)
  value = 0
  max = 0
  majority = NULL
  for (l in levels(as.factor(Y))) {
    ratio = length(Y[Y == l]) / nbElements
    value = value - ratio*log(ratio)
    if (ratio > max) {
      majority = l
    }
  }
  return(list(
    value=value,
    majorityClass=majority
  ))
}

filter <- function(X, condition) {
  
  for (i in (1:nrow(X))) {
    
  }
}

# Returns a vector list of size (length(j) + 1) containing
# different subsets of X divided depending on their values
# for the attribute
divideDataset <- function(X, attribute, values) {
  attr = X[,attribute]
  res = list()
  lenValues = length(values)
  res[[1]] = X[attr < values[1],]
  for (i in (2:lenValues)) {
    res[[i]] = X[values[i-1] <= attr & attr < values[i],]
  }
  res[[lenValues+1]] = X[attr >= values[lenValues],]
  return(res)
}

# Takes data matrix X as input and creates a DecisionTree based on it
decisionTree <- function(X, theta) {
  node = decisionNode()
  entropyX = entropy(X)
  if (entropyX$value < theta) {
    node$class = entropyX$majorityClass
    return(node)
  }
  else {
    j <- attributeDivision(X)
    subNode = decisionNode()
    subX = divideDataset(X, j)
    for (i in (1:length(subX))) {
      node$children[i] = decisionTree(subX[i], theta)
    }
    return(node)
  }
}
