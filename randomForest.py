class Node(object):
    # Constructor for tree node object
    def __init__(self, left, right, parent, split_idx, split_value, prediction):
        self.left = left
        self.right = right
        self.parent = parent
        self.split_idx = split_idx
        self.split_value = split_value
        self.prediction = prediction

import numpy as np


def entropy(y):
    # Calculates label entropy for given set of data 
    # Returns entropy value
    n = len(y)
    
    # Calculate probability given counts of unique labels
    values,counts = np.unique(y,return_counts=True)
    probs = counts / n
    
    # Calculate entropy with goal of max information gain
    entropy = -np.sum(probs*np.log(probs),axis=0)
    return entropy


def findSplit(X,y,metric):
    # Finds the best feature to split on when branching for decision tree creation
    # Can use Gini impurity (squared loss w/ weights) or entropy as metric
    # Returns feature index for cut, that feature's value, and the loss from the best split
    n,d = X.shape
    assert d > 0 # make sure more than one dimension
    assert n > 1 # make sure more than one data point

    bestloss = np.inf
    feature = np.inf
    split = np.inf
    
    # Use Gini impurity (squared loss) as loss function
    if metric is impurity:
        # Create matrix to store loss values for data
        loss = np.zeros((d,n-1))
        
        # Loop over all features (dimensions)
        for f in range(0,d):
            # Sort data, weights, labels in accordance with sorted features
            X_sorted = np.sort(X[:,f])            
            w_sorted = weights[np.argsort(X[:,f])]
            y_sorted = y[np.argsort(X[:,f])]

            # The first difference of x
            X_dif = np.diff(X_sorted)
            zero_ind = np.argwhere(X_dif==0).flatten()

            # Go over all inputs and calculate squared loss / label impurity
            Pl = np.cumsum(w_sorted*y_sorted)[:n-1]
            Ql = np.cumsum(w_sorted*(y_sorted**2))[:n-1]
            Wl = np.cumsum(w_sorted)[:n-1]

            Pr = np.dot(w_sorted,y_sorted)-Pl
            Qr = np.dot(w_sorted,(y_sorted**2))-Ql
            Wr = np.sum(w_sorted)-Wl

            # Calculate loss per feature
            L = (Ql-(Pl**2)/Wl)+(Qr-(Pr**2)/Wr)
            L[zero_ind] = np.inf
            
            # Store loss of feature 
            loss[f] = L
        
        # Find lowest label impurity / loss
        bestloss = np.min(loss)

        # Find feature and data split indices for lowest loss
        idx = np.unravel_index(np.argmin(loss, axis=None), loss.shape)

        feature = idx[0]
        split_idx = idx[1]

        # Calculate split value
        X_sorted = np.sort(X[:,feature])
        split = (X_sorted[split_idx]+X_sorted[split_idx+1])/2
    
    # Use entropy over tree as loss function
    if metric is entropy:
        # Loop over all features
        for f in range(0,d):
            # Sort data and labels according to feature values
            X_sorted = np.sort(X[:,f])
            y_sorted = y[np.argsort(X[:,f])]
            
            # Loop over all feature values
            for i in range(0,n):
                # Calculate left branch entropy
                left = y_sorted[0:i+1]
                pl = len(left)/n 
                Hl = entropy(left)
                
                # Calculate right branch entropy
                right = y_sorted[i+1:n]
                pr = len(right)/n
                Hr = entropy(right)
                
                # Calculate tree sum entropy
                H = pl*Hl + pr*Hr
                
                # If entropy is less than best loss
                if H < bestloss:
                    # Choose feature and split value for split
                    bestloss = H
                    feature = f
                    split = (X_sorted[i]+X_sorted[i+1])/2

    return feature, split, bestloss
    

def buildTree(X,y,maxDepth=np.inf,obj):
    # Builds a tree by recursively branching on each feature
    # Inputs: training data, labels, maximum depth, objective (classification or regression)
    # Uses treeSplit function to determine best feature and value to branch on
    # Stops at ID3 base cases and returns tree
    
    assert obj is "classification" or "regression" # make sure goal is defined
    n,d = X.shape

    # Initialize new node
    node = Node(None,None,None,None,None,None)

    # Prediction is mean label for regression
    if obj = "regression":
        node.prediction = np.mean(y)
    # Prediction is label mode for classification
    if obj = "classification":
        node.prediction = np.mode(y)
    
    # Base case: if leaves pure or insufficient features to partition or labels pure or max depth reached
    # Stop splitting and return tree
    if depth < 1 or n < 2 or len(np.unique(y)) == 1 or len(np.unique(X)) ==1:
        return node
    
    # Find best split to branch tree on
    split_idx,split_value,loss = treeSplit(X,y,w)
    
    # Isolate feature for split
    node.split_idx = split_idx
    
    # Use best split to assign threshold cut value
    node.split_value = split_value
    
    # Partition left and right branches given cut value    
    left_idx = X[:,split_idx] < split_value
    right_idx = X[:,split_idx] > split_value
    
    # Build left branch, values less than threshold
    y_left = y[left_idx]
    xT_left = X[left_idx,:]
    
    # Build right branch, values greater than threshold  
    y_right = y[right_idx]
    xT_right = X[right_idx,:]
        
    # Recursive calls to build tree: left and right
    node.left = cart(xT_left,y_left,depth-1,w_left)
    node.left.parent = node
    node.right = cart(xT_right,y_right,depth-1,w_right)
    node.right.parent = node

    return node
    

def evaluateTree(root,X_test):
    # Evaluates a tree given its root
    # Returns an array of predictions for each test point
    assert root is not None
    n = X_test.shape[0]
    pred = np.zeros(n)
  
    # Recursive inner function
    def recurEval(node,X_test,p):
        # Base case: if no more nodes i.e. data return prediction
        #if p == n or node is None:
        if node is None:
            return pred
        # If leaf reached its mean is prediction
        elif node.left is None and node.right is None:
            pred[p] = node.prediction
        # If data value smaller than threshold, continue along left branch
        elif X_test[p,node.split_idx]<node.split_value and node.left is not None:
            return recurEval(node.left,X_test,p)
        # If data value larger than threshold, continue right branch
        elif X_test[p,node.split_idx]>node.split_value and node.right is not None:
            return recurEval(node.right,X_test,p)
    
    # Evaluate each facet of tree
    for i in range(n):
        recurEval(root,X_test,i)
    
    return pred


def randomForest(X, y, m, k=None, maxdepth=np.inf, subsample=True):
    # Grows a random forest of decision trees by bootstrap sampling data
    # Input data, lablels, number of trees m, number of features in subsample k, maximum tree depth
    # Returns a list of decisions trees in forest
    n, d = X.shape
    forest = []
    
    # Number of features to subsample 
    if k is None:
        k = int(np.sqrt(d))
    
    # If no subsample, use all features
    if subsample is False:
        k = d
    
    # Matrix to store all sampled data
    x = np.zeros((n,k))
    
    # While number of trees in forest is fewer than capacity
    # Sample m data sets with replacement
    while len(trees)<m:
        # Generate random data bootstrap index sequence
        rand_data = np.random.choice(np.arange(0,n),size = n,replace=True)
        # Randomly subsample k <= d features w/o replacement
        rand_features = np.random.choice(np.arange(0,d), size = k,replace=False)
        
        # Select data and labels from random sample
        x = X[rand_data,rand_features]
        y = y[rand_data]

        # Build tree with randomly sampled data
        tree = cart(x,y,maxdepth,weights=None)
              
        # Add tree to forest
        forest.append(tree)
    
    return forest


def evaluateForest(forest, X_test, weights=None):
    # Predicts test data labels by evaluating each tree in a forest
    # Returns mean prediction for each data point
    
    # Number of trees in forest
    m = len(trees)
    
    # Number of data points and features
    n,d = X.shape
    
    # Weights for each tree in forest
    if weights is None:
        weights = np.ones(m) / len(trees)
            
    pred = np.zeros(n)
    preds = np.zeros((m,n))
    
    # Evaluate each tree in random forest
    for t in range(0,m):
        # Add predictions to a m x n matrix where m is # trees and n is # data points
        # Weighted by relative weight of each tree's predictions
        preds[t,:]=evaluateTree(trees[t],X_test)*weights[t]
    
    # Calculate mean of predictions across all trees
    pred = np.nanmean(preds,axis=0) 

    return pred
