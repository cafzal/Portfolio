# Helper function for kernel function
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
 
# Computes kernel of specified type given data sets to map data into high dimensional space, capture feature interactions, useful when many dimensions i.e. d >> n
def computeKernel(kFunct, X, Z, kParam=0):
    # Assumes kernel function is linear, polynomial, or RBF
    # Inputs are data X, a n x d matrix, and Z, a m x d matrix, which may be the same
    assert kFunct is "linear" or "polynomial" or "poly" or "rbf"
    
    n,d1 = X.shape
    m,d2 = Z.shape
    assert d1 == d2
    
    if kFunct is "linear":
        K = np.dot(X,Z.T)
    if kFunct is "polynomial" or "poly":
        K = np.power((np.dot(X,Z.T)+1),kParam)
    if kFunct is "rbf":
        K = np.exp(-kParam*(findDists(X,Z)**2))
    
    return K

# Function for Gaussian process regression, a Bayesian algorithm notable for its representation of predictive uncertainty
def GPR(X_train,X_test,y_train,sigma,kFunct,kParam):
    # Assumes all data including noise and  labels y are Gaussian distributed
    # Inputs: training data and labels, test data, sigma squared for noise, kernel function type, kernel function parameter
    # Returns a posterior including predicted mean for y_test and covariance matrix as uncertainty of prediction
    
    n,d = X_train.shape
    m,k = X_test.shape
    
    # Compute training data kernels
    K_train = computeKernel(kFunct, X_train, X_train, kParam)
    
    # Compute test data kernels
    K_test = computeKernel(kFunct, X_test, X_test, kParam)
    
    # Compute kernel capturing training and test data relationship
    K_traintest = computeKernel(kFunct, X_train, X_test, kParam)
    
    # Identity matrix for train kernel
    I = np.eye(n)
    
    # Additive Gaussian noise for uncertainty / regularization 
    noise = sigma*I
    
    # Prediction = GPR mean = (X_train, X_test kernel) * (inverse X_train kernel + noise) * (y_train labels)
    y_test = np.dot(np.dot(K_traintest.T, np.linalg.inv(K_train + noise)),y_train)

    # Uncertainty = GPR variance = (X_test kernel) - (GPR mean)
    uncertainty = K_test - np.dot(np.dot(K_traintest.T, np.linalg.inv(K_train + noise)), K_traintest)
    
    # Ensure that there are predictions for each test point
    assert y_test.shape[0] is m
    
    return y_test,uncertainty


    
