# Naive Bayes function
# Inputs events= n events x with d dimensions, outcomes= n binary outcomes y (+1 or -1), x_test test point of d dimensions
# Returns log ratio value indicating probability outcomes that x_test occurs (y_test = +1) or doesn't (y_test = -1)
def naiveBayes(events,outcomes,test):
    # Number of samples and features
    n, d = events.shape
    
    # Plus-one smoothing for training data and labels
    events = np.concatenate([events, np.ones((2,d))])
    outcomes = np.concatenate([outcomes, [-1,1]])
    
    # Prior P(outcome occurs, = +1)
    P_occur = np.sum(outcomes > 0)/n
    
    # Prior P(outcomes doesn't occur, = -1)
    P_notoccur = np.sum(outcomes < 0)/n
    
    # Combine features and labels
    data = np.column_stack((events,outcomes))
    
    # Extract data pertaining to each outcome
    events_occur = data[np.where(data[:,d]>0),:d]
    events_notoccur = data[np.where(data[:,d]<0),:d]
    
    # Sum total count of each outcome
    total_occur = np.sum(events_occur[:,:d])
    total_notoccur = np.sum(events_notoccur[:,:d])
    
    # Sum event feature count of each outcome
    features_occur = np.sum(events_occur[:,:d],axis=1)
    features_notoccur = np.sum(events_notoccur[:,:d],axis=1)
    
    # Estimate P(events given outcomes occur)
    P_events_occur = features_occur/total_occur
    
    # Estimate P(events given outcomes don't occur)
    P_events_notoccur = features_notoccur/total_notoccur

    # Test log using NB calculates p(outcome|event) = p(event|outcome)*p(outcome) (disregard p(event) assuming conditional independence between features)
    log_test_occur = np.sum(test*P_events_occur) + np.log(P_occur)
    log_test_notoccur = np.sum(test*np.log(P_events_notoccur)) + np.log(P_notoccur)
    
    # Calculate log ratio for probability of occurance (positive if predicted)
    logratio_test = log_test_occur - log_test_notoccur
    
    return logratio_test
    
    
    
