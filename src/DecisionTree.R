# Returns only a node made of the separation attribute and separation value
decisionNode <- function(attribute=NA, values=NA, quantitative=TRUE, prediction=NULL, children=list()) {
  node <- list(
    attribute=attribute, # attribute we check
    values=values, # value(s) to compare to to choose the branch
    quantitative=quantitative, # is attribute to check discrete or continue
    prediction=prediction, # class prediction if we are done
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
    if (ratio > 0)
      value = value - ratio*log(ratio)
    if (ratio > max) {
      max = ratio
      majority = l
    }
  }
  return(list(
    value=value,
    majorityClass=majority
  ))
}


# Returns a vector list of size (length(j) + 1) containing
# different subsets of X divided depending on their values
# for the attribute
divideDataset <- function(X, values=NA, indices=NA, attribute=1, quantitative=TRUE) {
  if (is.null(dim(X))) {
    X = matrix(X, ncol = 1)
  }
  attr = X[,attribute]
  res = list()
  if (all(!is.na(indices))) {
    res[[1]] = X[1:(indices[1]-1),]
    lenIndices = length(indices)
    if (lenIndices > 1) {
      for (i in 2:lenIndices) {
        res[[i]] = X[indices[i-1]:(indices[i]-1),]
      }
    }
    res[[lenIndices+1]] = X[indices[lenIndices]:nrow(X),]
  }
  else if (all(!is.na(values))) {
    lenValues = length(values)
    if (quantitative) {
      res[[1]] = X[attr < values[1],]
      if (lenValues > 1) {
        for (i in (2:lenValues)) {
          res[[i]] = X[values[i-1] <= attr & attr < values[i],]
        }
      }
      res[[lenValues+1]] = X[attr >= values[lenValues],]
    }
    else {
      for (i in 1:lenValues) {
        res[[i]] = X[values[i] == attr,]
      }
    }
  }
  return(res)
}

# Takes 2 vectors, sorts the first one and applies the same
# sorting to the second one
orderVectorByOther <- function(Y, x) {
  z = cbind(x, Y)
  z = z[order(z[,1]),]
  z = z[,2]
  return(z)
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

is.qualitative <- function(X) {
  return(class(X) != "numeric" && class(X) != "integer")
}

attributeDivision <- function(X, Y, r, validAttributes) {
  lenX = nrow(X)
  minE = .Machine$double.xmax
  j = list(attribute=NA, values=NA, indices=NA, quantitative=NA, validAttributes = NA)

  nbValidSampledAttributes = 0
  nbValidAttributes = length(validAttributes)
  removedAttributes = c()
  sampleVector = validAttributes
  while(nbValidSampledAttributes < r && nbValidSampledAttributes < nbValidAttributes ){
  # Loop through every attribute
    att = sample(sampleVector, 1)
    sampleVector = sampleVector[sampleVector != att]
    XAtt = X[,att]
    
    # QUALITATIVE
    if (is.qualitative(XAtt)) {
      
      classes = c(levels(factor(XAtt)))
      if(length(classes) == 1) {
        #If there is no separation, remove the attribute from node and don't sample it.
        removedAttributes <- c(removedAttributes, att)
        nbValidAttributes = nbValidAttributes - 1
        sampleVector = sampleVector[sampleVector != att]
      }
      else {
        nbValidSampledAttributes = nbValidSampledAttributes +1
        
        # Separation of values in different classes
        separated = divideDataset(cbind(X, Y), values=classes, attribute=att, quantitative=FALSE)

        E=0
        for (portion in separated) {
          
          if(nrow(portion) !=0)
            E = E + (NROW(portion) / lenX) * (entropy(portion[,NCOL(portion)])$value)
        }
        if (E < minE) {
          minE = E
          j$attribute = att
          j$values = classes
          j$quantitative=FALSE
        }
      }
    }
    # QUANTITATIVE
    else {
      
      # Every way to separate a dataset
      possibleSeparations = separations(sort(XAtt))
      orderedY = orderVectorByOther(Y, XAtt)
      
      if (length(possibleSeparations) > 0) nbValidSampledAttributes = nbValidSampledAttributes +1
      
      for (ind in possibleSeparations) {
        sep = divideDataset(orderedY, indices=ind)
        E = 0
        for (portion in sep) {
          E = E + (length(portion) / lenX) * entropy(portion)$value
        }
        if (E < minE) {
          
          # Recover the corresponding x values for the frontier
          values = XAtt[ind]
          
          minE = E
          j$attribute = att
          j$values = values
          j$quantitative=TRUE
        }
      }
    }
    #Remove sampled attribute to sample another one
    sampleVector = sampleVector[sampleVector != att]
  }
  
  if(nbValidSampledAttributes== 0) return(NULL)
  else {
    if(!j$quantitative) {
      removedAttributes <- c(removedAttributes, j$att)
    }
  }
  newValidAttributes = validAttributes[!(validAttributes %in% removedAttributes)]
  j$validAttributes = newValidAttributes
  
  return(j)
}

# Takes data matrix X as input and creates a DecisionTree based on it
decisionTree <- function(X, Y, theta,r, validAttributes= 1:ncol(X)) {
  node = NULL
  entropyY = entropy(Y)
  if (entropyY$value <= theta) {
    node = decisionNode(
      prediction=entropyY$majorityClass)
    return(node)
  }
  else {
    j <- attributeDivision(X, Y, r, validAttributes)
    if (is.null(j)) {
      return(decisionNode(prediction = entropyY$majorityClass))
    }
    else {
      node = decisionNode(
        attribute=j$attribute,
        values=j$values,
        quantitative=j$quantitative,
        prediction=entropyY$majorityClass
      )

      xy = (cbind(X, Y))

      sub = divideDataset(xy[order(xy[,node$attribute]),], values=j$values, attribute=node$attribute, quantitative=node$quantitative)

      for (i in (1:length(sub))) {
        subi = sub[[i]]
        if(nrow(subi) == 0) {
          node$children[[i]] =decisionNode(prediction=entropyY$majorityClass)
        }
        else {
          subTree = decisionTree(subi[,-ncol(subi)], subi[,ncol(subi)], theta, r, j$validAttributes)
          node$children[[i]] = subTree
        }
      }
      return(node)
    }
  }
}

print.decisionTree <- function(t, useS4 = FALSE) {
  if (!is.na(t$attribute))
    print(paste("Attribute : ", t$attribute))
  if (all(!is.na(t$values)))
    print(paste(c("Values : ", t$values)))
  len = length(t$children)
  if (len > 0)
    print(paste("Children : ", len))
  if (!is.null(t$prediction))
    print(paste("Prediction : ", t$prediction))
}

printTree <- function(tree) {
  print(tree)
  if (length(tree$children) > 0) {
    for (i in (1:length(tree$children))) {
      t = tree$children[[i]]
      print("")
      print(paste("vvv CHILD ", i, " vvv"))
      printTree(t)
      print("")
      print("--- GO UP ---")
      print("")
      print(tree)
    }
  }
  else
    print(tree)
}


#Classify "x" with the decision tree "node"
decisionTree.predict <- function(node, x) {
  
  if(length(node$children) == 0) return(node$prediction)
  else {
    attr <- x[,node$attribute]
    if(node$quantitative) {
      childIndex = 0
      for (c in 1:length(node$values)){
        if (attr < node$values[c]){
          childIndex = c
          break
        }
      }
      if(childIndex == 0) childIndex = length(node$children)
      return(decisionTree.predict(node$children[[childIndex]],x))
      
    }else{
      childIndex = which(node$values == attr)
      
      if (length(childIndex) == 1) {
        return(decisionTree.predict(node$children[[childIndex]], x))
      }
      else {
        return(node$prediction)
      }
    } 
  }
}
