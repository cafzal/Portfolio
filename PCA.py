# PCA function to map data X of dimensionality d into Z of lower dimensional space k given desired retention of variance 
def PCA(X,k=None,varRetained=None):
    # Ensure variance goal < 100%
    if varRetained is not None: assert varRetained < 1 
    # Ensure target dimensionality in a valid feature space
    if k is not None: assert k > 0
    # Must specify either target dimensionality or retained variance goal
    assert k is not None or varRetained is not None
    
    # X is a matrix of num samples x num dimensions
    n,d = X.shape
    
    # Standardize the dataset
    # Find feature mean 
    mu = np.mean(X,axis=0)
    # Calculate standard deviation
    stdev = np.std(X)
    # Subtract mean to center data, scale by standard dev
    Y = (X - mu) / stdev
    
    # Singular value decomposition
    U,s,V = np.linalg.svd(Y,full_matrices=False)
    
    # Compute total variance (principal components) in dataset
    totalVar = np.sum(s)

    # Helper function to determine degree of dimensional reduction d -> k
    # Given desired variance retention in reduced dim data 
    def minDimensions(k,varRetained):
        U,s,V = np.linalg.svd(Y,full_matrices=False)
        totalVar_reduced = np.sum(s[0:k])
        if (totalVar_reduced/totalVar) < varRetained:
            return minDimensions(k+1,varRetained)
        return k
    
    # Use helper function to find k if k is not specified
    if k is None and varRetained is not None:
        k = 1
        k = minDimensions(k,varRetained)
    
    # Derive principal axes
    U_reduced = U[:, 0:k]
    # Extract principal components
    s_reduced = s[0:k]
    
    # Reduced X is Z, n x k matrix of projected PCs mapped to lower dim  feature space
    Z = U_reduced*s_reduced
    return Z
