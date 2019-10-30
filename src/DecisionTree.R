# Returns only a node made of the separation attribute and separation value
decisionNode <- function(attribute=NA, value=NA, quantitative=TRUE, class=NA, children=NA) {
  node <- list(
    attribute=attribute, # attribute we check
    value=value, # value(s) to compare to to choose the branch
    quantitative=quantitative, # is attribute to check discrete or continue
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
divideDataset <- function(X, values, attribute=1, quantitative=TRUE) {
  if (is.null(dim(X))) {
    X = matrix(X, ncol = 1)
  }
  attr = X[,attribute]
  res = list()
  lenValues = length(values)
  if (quantitative) {
    res[[1]] = X[attr < values[1],]
    for (i in (2:lenValues)) {
      res[[i]] = X[values[i-1] <= attr & attr < values[i],]
    }
    res[[lenValues+1]] = X[attr >= values[lenValues],]
  }
  else {
    for (i in 1:lenValues) {
      res[[i]] = X[values[i] == attr,]
    }
  }
  return(res)
}

# Takes 2 vectors, sorts the first one and applies the same
# sorting to the second one
orderVectorByOther <- function(Y, x) {
  x = cbind(x, (1:length(x)))
  x = x[order(x[,1]),]
  Y=Y[x[,2]]
  return(Y)
}

# gives all possible separations indices
separations <- function(X, n=2) {
  # n is the resulting number of categories so there will be (n-1) indices of separation
  len = length(X)
  last = X[1]
  res = list()
  count=0
  if (n > 1) {
    for (i in 2:len) {
      # Find where the class change to get the first separator
      if (X[i] != last) {
        last = X[i]
        
        # Case where we only need one separator
        if (n==2) {
          count = count + 1
          res[[count]] = i
        }
        
        # Case where we need more separators
        else {
          # Call the function recursively on the rest of the elements after the separator
          subSep = separations(X[i:len], n=n-1)
          for (s in subSep) {
            count = count+1
            
            # Concat the recursive result with the current one
            # by keeping track of the offset
            res[[count]] = c(i,s+i-1)
          }
        }
      }
    }
  }
  return(res)
}

# gives all possible separations
separate <- function(X, n=2) {
  
  # n is the resulting number of categories so there will be (n-1) indices of separation
  len = length(X)
  last = X[1]
  res = list()
  count=0
  if (n > 1 & len >= 2) {
    for (i in 2:len) {
      
      # Find where the class change to get the first separator
      if (X[i] != last) {
        last = X[i]
        
        # Case where we only need one separator
        if (n==2) {
          count = count + 1
          res[[count]] = list(X[1:(i-1)], X[i:len])
        }
        
        # Case where we need more separators
        else {
          
          # Call the function recursively on the rest of the elements after the separator
          subSep = separate(X[i:len], n=n-1)
          for (s in subSep) {
            
            # Concat the recursive result with the current one
            # by keeping track of the offset
            count = count+1
            res[[count]] = list(X[1:(i-1)])
            for (j in 1:length(s)) {
              res[[count]][[j+1]] = s[[j]]
            }
          }
        }
      }
    }
  }
  return(res)
}


# useless
partition <- function(collection){
  if (length(collection) == 1) {
    return(list(list(collection)))
  }
  res = list()
  count = 0
  first = collection[1]
  smalls = partition(collection[2:length(collection)])
  for (smaller in smalls){
    # insert `first` in each of the subpartition's subsets
    for (n in (1:length(smaller))) {
      count = count + 1
      res[[count]] = list()
      i=0
      
      if (n>1) {
        for (i in (1:(n-1)))
          res[[count]][[i]] = smaller[[i]]
      }
      
      res[[count]][[n]] = c(first, smaller[[n]])
      
      if (n<length(smaller)) {
        for (i in ((n+1):length(smaller)))
          res[[count]][[i]] = smaller[[i]]
        #i = i + 1
        #res[[count]][[i]] = smaller[[(n+1):length(smaller)]]
      }
      
    }
    # put `first` in its own subset 
    count = count + 1
    res[[count]] = smaller
    res[[count]][[length(res[[count]]) + 1]] = first
  }
  return(res)
}

# useless as well
splitClasses <- function(classes, n=2) {
  p = partition(classes)
  return(p[lapply(p, length) == n])
}

attributeDivision <- function(X, Y) {
  lenX = nrow(X)
  minE = .Machine$double.xmax
  j = list(attribute=NA, value=NA)
  
  # Loop through every attribute
  for (att in 1:ncol(X)) {
    XAtt = X[,att]
    
    # QUALITATIVE
    if (class(XAtt) != "numeric") {
      
      classes = levels(as.factor(XAtt))
      # Separation of values in different classes
      separated = divideDataset(cbind(X, Y), classes, attribute=att, quantitative=FALSE)
      E=0
      for (portion in separated) {
        E = E - (nrow(portion) / lenX) * entropy(portion[,ncol(portion)])
      }
      if (E < minE) {
        minE = E
        j$attribute = att
        j$value = classes
      }
    }
    # QUANTITATIVE
    else {
      
      # Y vector re-ordered according to X sorted by specific attribute
      orderedY = orderVectorByOther(Y, XAtt)
      
      # Every way to separate a dataset
      possibleSeparations = separate(orderedY, n=n)
      for (sep in possibleSeparations) {
        E = 0
        for (portion in sep) {
          E = E - (nrow(portion) / lenX) * entropy(portion) 
        }
        if (E < minE) {
          
          # Recover the corresponding x values for the frontier
          indices = cumsum(lapply(sep[1:n-1], length))+1
          values = X[indices]
          
          minE = E
          j$attribute = att
          j$value = values
        }
      }
    }
  }
  return(j)
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
    subX = divideDataset(X, j$value, j$attribute)
    for (i in (1:length(subX))) {
      node$children[i] = decisionTree(subX[i], theta)
    }
    return(node)
  }
}
