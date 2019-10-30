# source the regression tree function
source("reg_tree.R")

# load plyr
require(plyr)

# define function to sprout a single tree
sprout_tree <- function(formula, feature_frac, data) {
  # extract features
  features <- all.vars(formula)[-1]
  
  # extract target
  target <- all.vars(formula)[1]
  
  # bag the data
  # - randomly sample the data with replacement (duplicate are possible)
  train <-
    data[sample(1:nrow(data), size = nrow(data), replace = TRUE)]
  
  # randomly sample features
  # - only fit the regression tree with feature_frac * 100 % of the features
  features_sample <- sample(features,
                            size = ceiling(length(features) * feature_frac),
                            replace = FALSE)
  
  # create new formula
  formula_new <-
    as.formula(paste0(target, " ~ ", paste0(features_sample,
                                            collapse =  " + ")))
  
  # fit the regression tree
  tree <- reg_tree_imp(formula = formula_new,
                       data = train,
                       minsize = ceiling(nrow(train) * 0.1))
  
  # save the fit and the importance
  return(list(tree$fit, tree$importance))
}

