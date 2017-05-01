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

##################    Sampling for stratum with highest expected benefit    ########################
# For finite population of size NxH, and a sample size of n
greedy<- function(pop, means, vars, init_samp_size, n){
  H<-length(means)
  
  estimates<-matrix(NA, H, 6) # row corresponds to a stratum
  colnames(estimates) <- c("num_samples", "mean_est", "variance_est", "expected_benefit", "true_mean", "true_var")
  estimates[,5]<-means
  estimates[,6]<-vars
  
  # first draw some samples for each strata, and get estimates
  for(stratum in 1:H){
    samp<-sample(pop[,stratum], init_samp_size)
    estimates[stratum,1]<-init_samp_size
    estimates[stratum,2]<-mean(samp)
    estimates[stratum,3]<-var(samp)
    estimates[stratum,4]<-var(samp)/(2*(2-1))
  }

  for(i in (H*init_samp_size+1):n) {  # We have already sampled init_samp_size*H observations
    # First find the row (strata) with the maximum expected value
    stratum=which(estimates[,'expected_benefit'] == max(estimates[,'expected_benefit']))
    data<-pop[,stratum]
    x<-sample(data, 1)
    estimates<-update_estimates(estimates, stratum, x)
  }
  return(estimates)
}

##########################     e-greedy     #############################
# For finite population of size NxH, and a sample size of n
e_greedy<- function(pop, means, vars, init_samp_size, epsilon, n){
  H<-length(means)
  
  estimates<-matrix(NA, H, 6) # row corresponds to a stratum
  colnames(estimates) <- c("num_samples", "mean_est", "variance_est", "expected_benefit", "true_mean", "true_var")
  estimates[,5]<-means
  estimates[,6]<-vars
  
  # first draw some samples for each strata, and get estimates
  for(stratum in 1:H){
    samp<-sample(pop[,stratum], init_samp_size)
    estimates[stratum,1]<-init_samp_size
    estimates[stratum,2]<-mean(samp)
    estimates[stratum,3]<-var(samp)
    estimates[stratum,4]<-var(samp)/(2*(2-1))
  }
  
  for(i in (H*init_samp_size+1):n) {  # We have already sampled init_samp_size*H observations
    rand<-runif(1, 0, 1)
    if(rand<epsilon){
      # Randomly select a stratum
      stratum<-floor(runif(1, 1, H+1))
    }
    else{
      # Find the row (strata) with the maximum expected value
      stratum<-which(estimates[,'expected_benefit'] == max(estimates[,'expected_benefit']))
    }
    data<-pop[,stratum]
    x<-sample(data, 1)
    estimates<-update_estimates(estimates, stratum, x)
  }
  return(estimates)
}

################## Proportional Allocation Sampling ########################
prop_allocation<-function(pop, means, vars, n){
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
    samp<-sample(pop[,stratum], allocations[stratum])
    estimates[stratum,"num_samples"]<-allocations[stratum]
    estimates[stratum,"mean_est"]<-mean(samp)
    estimates[stratum,"variance_est"]<-var(samp)
  }
  return(estimates)
}

####################### Neyman Allocation Sampling #############################
neyman_allocation<-function(pop, means, vars, n){
  H<-length(means)
  
  estimates<-matrix(NA, H, 5) # row corresponds to a stratum
  colnames(estimates) <- c("num_samples", "mean_est", "variance_est", "true_mean", "true_var")
  estimates[,4]<-means
  estimates[,5]<-vars
  
  # Neyman allocation:
  proportions<-sqrt(estimates[,"true_var"])/sum(sqrt(estimates[,"true_var"]))
  allocations<-floor(proportions*n)
  
  # Randomly add samples to make number samples = n
  missed_samples=(n-sum(allocations))
  samp<-sample(1:H, missed_samples)
  for (i in samp){
    allocations[i]<-allocations[i]+1
  }
  
  # Now sample:
  for (stratum in 1:H){
    samp<-sample(pop[,stratum], allocations[stratum])
    estimates[stratum,"num_samples"]<-allocations[stratum]
    estimates[stratum,"mean_est"]<-mean(samp)
    estimates[stratum,"variance_est"]<-var(samp)
  }
  return(estimates)
}

####################### Two Stage Sampling #############################
two_stage_allocation<-function(pop, means, vars, init_prop, n){
  
  H<-length(means)
  init_samp_size<-ceiling(init_prop*n/H)

  estimates<-matrix(NA, H, 5) # row corresponds to a stratum
  colnames(estimates) <- c("num_samples", "mean_est", "variance_est", "true_mean", "true_var")
  estimates[,4]<-means
  estimates[,5]<-vars

  # first draw some samples for each strata, and get estimates
  for(stratum in 1:H){
    samp<-sample(pop[,stratum], init_samp_size)
    estimates[stratum,1]<-init_samp_size
    estimates[stratum,2]<-mean(samp)
    estimates[stratum,3]<-var(samp)
  }

  # Now compute estimated best Neyman allocation:
  proportions<-sqrt(estimates[,"variance_est"])/sum(sqrt(estimates[,"variance_est"]))

  # Check if too many samples nwere added initially, and adjust accordingly
  adding_strat<-which(proportions>=init_samp_size/n)

  two_allocations<-floor(proportions*n)-init_samp_size # basic way, then adjust
  # two_allocations is the number to add, not total number of samples 
  two_allocations[-adding_strat]<-0 # don't add any if you already added too many
  two_allocations[adding_strat]<-floor((estimates[adding_strat,"variance_est"]/sum(estimates[adding_strat,"variance_est"]))*(n-init_samp_size*H)) #reweight ney alloc

  # Randomly add samples to make number samples = n
  missed_samples=(n-init_samp_size*H-sum(two_allocations))
  samp<-sample(1:H, missed_samples)
  for (i in samp){
    two_allocations[i]<-two_allocations[i]+1
  }
  
  # Now get the samples, and update means & variances
  for (stratum in 1:H){
    if (two_allocations[stratum]>0){
      samp<-sample(pop[,stratum], two_allocations[stratum])
      samp_mean<-mean(samp)
      samp_var<-var(samp)

      estimates[stratum,"num_samples"]<-estimates[stratum,"num_samples"]+two_allocations[stratum]
      estimates[stratum,"mean_est"]<-(estimates[stratum,"mean_est"]*(init_samp_size)+samp_mean*two_allocations[stratum])/(init_samp_size+two_allocations[stratum])
      estimates[stratum,"variance_est"]<-(estimates[stratum,"variance_est"]*(init_samp_size)+samp_var*two_allocations[stratum])/(init_samp_size+two_allocations[stratum])
    }
  }
  return(estimates)
}