# k-Means Clustering

def findDists(X,Z=None):
    # Computes the Euclidian distance matrix between X and Z
    # Inputs: X is n x d matrix; Z is m x d matrix
    # Returns: D is n x m matrix
    n,d1 = X.shape
    m,d2 = Z.shape
    assert (d1==d2)
    
    # Calculate distances for each point to centroid ||X-Z||
    Dsq = np.sum(X**2,axis=1)[:,np.newaxis] + np.sum(Z**2,axis=1) - 2*np.dot(X,Z.T)
    Dsq = np.absolute(Dsq)
    D = np.sqrt(Dsq)
    
    return D

def initCentroids(X,k):
    # Initialize k random centroids from data set
    # Returns: centroids
    
    # Randomly permute data
    rand = np.random.permutation(X)
    # Select k centroids
    centroids = rand[0:k,:]
    
    return centroids

def findNearestCentroids(X, centroids):
    # Finds the nearest centroid to each data point
    # Returns: list of centroid indices
    n,d = X.shape
    indices = []
    
    # Find distance from data points to centroids
    dists = findDists(X,centroids)
    # Find indices of closest centroids
    indices = np.argmin(dists,axis=1)
    
    return indices

def calculateMeans(X, indices, k):
    # Calculates the means of each centroid's data points' dimensions
    # Returns: k x d matrix of centroid values
    n,d = X.shape
    
    centroids = np.zeros((k,d))
    idx = []
    
    # For each of the k clusters
    for j in range(0,k):
        # Find data points assigned to cluster (closest)
        idx = np.argwhere(indices==j)
        idx = idx.flatten()
        # Calculate mean of cluster data values
        c = np.mean(X[idx,:],axis=0)
        # Assign cluster value as mean
        centroids[j,:] = c
        
    return centroids

def kMeansClustering(X,k,runs):
    # Determines proximate groupings of data points for given number of clusters
    # Returns: matrix of cluster centroids and their indices
    centroids = initCentroids(X,k)
    indices = []
    
    i = 0
    while i < runs:
        # Find indices of data points for each centroid
        indices = findNearestCentroids(X,centroids)
        # Compute centroid values given cluster data
        centroids = calculateMeans(X,indices,k)
        i += 1
        
    return centroids,indices
