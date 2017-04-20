# Takes in the current estimates, and a data point from a stratum returns the updated estimates
# This is done with the formula: eb_new<-var_new/(n*(n-1))
update_estimates<- function(estimates, stratum, x){
  n<-estimates[stratum,"num_samples"]+1
  mean_old<-estimates[stratum,2]
  var_old<-estimates[stratum,3]
  eb_old<-estimates[stratum,4]
  
  mean_new<-(1/n)*(x+(n-1)*mean_old)
  var_new<-((n-2)/(n-1))*var_old + (1/n)*(x-mean_old)^2  # Check this formula!!!!
  eb_new<-var_new/(n*(n+1))
  
  estimates[stratum,1]<-n
  estimates[stratum,2]<-mean_new
  estimates[stratum,3]<-var_new
  estimates[stratum,4]<-eb_new
  return(estimates)
}

################## Sampling for stratum with highest expected benefit ########################
# For finite population of size NxH, and a sample size of n
greedy<- function(pop, means, vars, n){
  # set.seed(101)
  init_samp_size<-2
  H<-length(means)
  
  estimates<-matrix(NA, H, 6) # row corresponds to a stratum
  colnames(estimates) <- c("num_samples", "mean_est", "variance_est", "expected_benefit", "true_mean", "true_var")
  estimates[,5]<-means
  estimates[,6]<-vars
  
  # first draw some samples for each strata, and get estimates
  for(i in 1:H){
    samp<-sample(pop, init_samp_size)
    estimates[i,1]<-init_samp_size
    estimates[i,2]<-mean(samp)
    estimates[i,3]<-var(samp)
    estimates[i,4]<-var(samp)/(2*(2-1))
  }

  for(i in (2*init_samp_size+1):n) {  # We have already sampled init_samp_size*H observations
    # First find the row (strata) with the maximum expected value
    stratum=which(estimates[,'expected_benefit'] == max(estimates[,'expected_benefit']))
    print(stratum)
    data<-pop[,stratum]
    x<-sample(data, 1)
    estimates<-update_estimates(estimates, stratum, x)
  }
  return(estimates)
}

################## Proportional Allocation Sampling ########################
prop_allocation<-function(pop, means, vars, n){
  # set.seed(101)
  H<-length(means)
  
  estimates<-matrix(NA, H, 5) # row corresponds to a stratum
  colnames(estimates) <- c("num_samples", "mean_est", "variance_est", "true_mean", "true_var")
  estimates[,4]<-means
  estimates[,5]<-vars
  
 # Proportional allocation:
  allocations<-rep(floor(n/H), H)
  
  # Randomly add samples to make number samples = n
  missed_samples=(n-sum(allocations))
  samp<-sample(1:H, missed_samples)
  for (i in samp){
    allocations[i]<-allocations[i]+1
  }
  
  # Now sample:
  for (stratum in 1:H){
    samp<-sample(pop[,stratum], allocations[i])
    estimates[stratum,"num_samples"]<-allocations[i]
    estimates[stratum,"mean_est"]<-mean(samp)
    estimates[stratum,"variance_est"]<-var(samp)
  }
  return(estimates)
}

####################### Neyman Allocation Sampling #############################
neyman_allocation<-function(pop, means, vars, n){
  # set.seed(101)
  H<-length(means)
  
  estimates<-matrix(NA, H, 5) # row corresponds to a stratum
  colnames(estimates) <- c("num_samples", "mean_est", "variance_est", "true_mean", "true_var")
  estimates[,4]<-means
  estimates[,5]<-vars
  
  # Neyman allocation:
  proportions<-estimates[,"true_var"]/sum(estimates[,"true_var"])
  allocations<-floor(proportions*n)
  
  # Randomly add samples to make number samples = n
  missed_samples=(n-sum(allocations))
  samp<-sample(1:H, missed_samples)
  for (i in samp){
    allocations[i]<-allocations[i]+1
  }
  
  # Now sample:
  for (stratum in 1:H){
    #data<-pop[,stratum]
    #samp<-sample(data, allocations[i])
    samp<-sample(pop[,stratum], allocations[i])
    estimates[stratum,"num_samples"]<-allocations[i]
    estimates[stratum,"mean_est"]<-mean(samp)
    estimates[stratum,"variance_est"]<-var(samp)
  }
  return(estimates)
}


