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
entropy <- function(X) {
  print("TODO")
  return(list(
    value=0,
    majorityClass=0
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
  res = list()
  lenValues = length(values)
  res[[1]] = X[X[,attribute] < values[1],]
  for (i in (2:lenValues)) {
    res[[i]] = X[values[i-1] <= X[,attribute] & X[,attribute] < values[i],]
  }
  res[[lenValues+1]] = X[X[,attribute] >= values[lenValues],]
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
