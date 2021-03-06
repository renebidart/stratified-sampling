# 1. Look at the preformance of this algo on a few different finite popuations
# with 3 strata, and different variances (all similiar, different, very different)

setwd("/Users/rb/Google_Drive/Waterloo/STAT854/stratified-sampling")
source("strat_samp_func_v2.R")

library(ggplot2)
library(reshape2)
library(lattice)

#out_file='rep1000_reinit100_4strat_n80.csv'

# Simulation parameters
reps<-20000
H<-3 # Number of strata
# n_method<-3 
n=100 # total sample size
N=10000 # Total population size per stratum

init_prop=.3 # for two stage
init_samp_size=3 # for greedy, e_greedy
epsilon = .1 # for e_greedy

# Population parameters
# means<-c(2, 1, 3)  # Do we need to try this over a set of different means? 
# true_mean<-mean(means)

# vars<-c(1,2,3)
# vars<-c(1,1,1)
vars<-c(1,1,8)


################    Evaluate performance (MSE and bias) ####################
results_greedy<-data.frame(matrix(NA, reps, 5))
colnames(results_greedy) <- c("MSE", "bias", "strat1", "strat2", "strat3")

results_neyman<-data.frame(matrix(NA, reps, 5))
colnames(results_neyman) <- c("MSE", "bias", "strat1", "strat2", "strat3")

results_prop<-data.frame(matrix(NA, reps, 5))
colnames(results_prop) <- c("MSE", "bias", "strat1", "strat2", "strat3")

results_two<-data.frame(matrix(NA, reps, 5))
colnames(results_two) <- c("MSE", "bias", "strat1", "strat2", "strat3")
 
results_egreedy<-data.frame(matrix(NA, reps, 5))
colnames(results_egreedy) <- c("MSE", "bias", "strat1", "strat2", "strat3")

# Weird set up is to do it for 20 differnt means
for (mean_iter in 0:19){
  means<-runif(3, 0, 5)
  true_mean<-mean(means)
  # create the population data
  pop<-matrix(NA, N, H)
  for (i in 1:H){
    pop[,i]<-rnorm(N, mean = means[i], sd = sqrt(vars[i]))
  }
  

  for (iter in 1:(reps/20)){
    rep<-iter+mean_iter*reps/20
    estimates_neyman<-neyman_allocation(pop=pop, means=means, vars=vars, n=n)
    results_neyman[rep,'MSE']<-(mean(estimates_neyman[,'mean_est'])-true_mean)^2
    results_neyman[rep,'bias']<-(mean(estimates_neyman[,'mean_est']-true_mean))/true_mean
    results_neyman[rep,'strat1']<-estimates_neyman[1,'num_samples']
    results_neyman[rep,'strat2']<-estimates_neyman[2,'num_samples']
    results_neyman[rep,'strat3']<-estimates_neyman[3,'num_samples']
    
    estimates_prop<-prop_allocation(pop=pop, means=means, vars=vars, n=n)
    results_prop[rep,'MSE']<-(mean(estimates_prop[,'mean_est'])-true_mean)^2
    results_prop[rep,'bias']<-(mean(estimates_prop[,'mean_est']-true_mean))/true_mean
    results_prop[rep,'strat1']<-estimates_prop[1,'num_samples']
    results_prop[rep,'strat2']<-estimates_prop[2,'num_samples']
    results_prop[rep,'strat3']<-estimates_prop[3,'num_samples']
    
    estimates_two<-two_stage_allocation(pop=pop, means=means, vars=vars, init_prop=init_prop, n=n)
    results_two[rep,'MSE']<-(mean(estimates_two[,'mean_est'])-true_mean)^2
    results_two[rep,'bias']<-(mean(estimates_two[,'mean_est']-true_mean))/true_mean
    results_two[rep,'strat1']<-estimates_two[1,'num_samples']
    results_two[rep,'strat2']<-estimates_two[2,'num_samples']
    results_two[rep,'strat3']<-estimates_two[3,'num_samples']
    
    estimates_greedy<-greedy(pop=pop, means=means, vars=vars, init_samp_size=init_samp_size, n=n)
    results_greedy[rep,'MSE']<-(mean(estimates_greedy[,'mean_est'])-true_mean)^2
    results_greedy[rep,'bias']<-(mean(estimates_greedy[,'mean_est']-true_mean))/true_mean
    results_greedy[rep,'strat1']<-estimates_greedy[1,'num_samples']
    results_greedy[rep,'strat2']<-estimates_greedy[2,'num_samples']
    results_greedy[rep,'strat3']<-estimates_greedy[3,'num_samples']
  
    estimates_egreedy<-e_greedy(pop=pop, means=means, vars=vars, init_samp_size=init_samp_size, epsilon=epsilon, n=n)
    results_egreedy[rep,'MSE']<-(mean(estimates_egreedy[,'mean_est'])-true_mean)^2
    results_egreedy[rep,'bias']<-(mean(estimates_egreedy[,'mean_est']-true_mean))/true_mean
    results_egreedy[rep,'strat1']<-estimates_egreedy[1,'num_samples']
    results_egreedy[rep,'strat2']<-estimates_egreedy[2,'num_samples']
    results_egreedy[rep,'strat3']<-estimates_egreedy[3,'num_samples']
    
  }
}

print(paste0("avg results_neyman: ", mean(results_neyman[,'MSE'])))
print(paste0("avg results_prop: ", mean(results_prop[,'MSE'])))
print(paste0("avg results_two: ", mean(results_two[,'MSE'])))
print(paste0("avg results_greedy: ", mean(results_greedy[,'MSE'])))
print(paste0("avg results_egreedy: ", mean(results_egreedy[,'MSE'])))

# Look at plot of distributions:
results_neyman$bias <- NULL
results_neyman <- melt(results_neyman,  id.vars = c("MSE"))
results_neyman$name <- "neyman"

results_two$bias <- NULL
results_two <- melt(results_two,  id.vars = c("MSE"))
results_two$name <- "two"

results_greedy$bias <- NULL
results_greedy <- melt(results_greedy,  id.vars = c("MSE"))
results_greedy$name <- "greedy"

results_egreedy$bias <- NULL
results_egreedy <- melt(results_egreedy,  id.vars = c("MSE"))
results_egreedy$name <- "egreedy"

data<-rbind(results_neyman, results_two, results_greedy)

bwplot(value ~ variable | name, data, main="Stratum Variances 1, 2, 3")


data<-rbind(results_neyman, results_two, results_greedy, results_egreedy)

bwplot(value ~ variable | name, data, main="Stratum Variances 1, 1, 8", panel=function(...) {
  # panel.abline(h=n*vars[1]/sum(vars), col="green")
  # panel.abline(h=n*vars[2]/sum(vars), col="red")
  # panel.abline(h=n*vars[3]/sum(vars), col="blue")
  panel.bwplot(...)
})



################# TWO SAMPLE CHANGING PROP ######################
setwd("/Users/rb/Google_Drive/Waterloo/STAT854/stratified-sampling")
source("strat_samp_func_v2.R")

library(ggplot2)
library(reshape2)
library(lattice)

# Simulation parameters
reps<-20000
H<-3 # Number of strata
# n_method<-3 
n=100 # total sample size
N=10000 # Total population size per stratum


vars<-c(1,2,3)

results_two<-data.frame(matrix(NA, reps, 5))
colnames(results_two) <- c("MSE", "bias", "strat1", "strat2", "strat3")
init_prop_list=c(.2, .3, .4, .6, .8)
prop_result<-c(0, 0, 0, 0, 0)
i=0
for (init_prop in init_prop_list){
  i=i+1
  # Weird set up is to do it for 20 differnt means
  for (mean_iter in 0:19){
    means<-runif(3, 0, 5)
    true_mean<-mean(means)
    # create the population data
    pop<-matrix(NA, N, H)
    for (i in 1:H){
      pop[,i]<-rnorm(N, mean = means[i], sd = sqrt(vars[i]))
    }
    
    for (iter in 1:(reps/20)){
      rep<-iter+mean_iter*reps/20
      
      estimates_two<-two_stage_allocation(pop=pop, means=means, vars=vars, init_prop=init_prop, n=n)
      results_two[rep,'MSE']<-(mean(estimates_two[,'mean_est'])-true_mean)^2
      results_two[rep,'bias']<-(mean(estimates_two[,'mean_est']-true_mean))/true_mean
      results_two[rep,'strat1']<-estimates_two[1,'num_samples']
      results_two[rep,'strat2']<-estimates_two[2,'num_samples']
      results_two[rep,'strat3']<-estimates_two[3,'num_samples']
    }
  }
  print(mean(results_two[,'MSE']))
  prop_result[i]<-mean(results_two[,'MSE'])
}


################# OVERALL EVALUATION    ######################
# Simulation parameters
reps<-100000 # do 1000 reps with 100 initializations
H<-3 # Number of strata
# n_method<-3 
n=100 # total sample size
N=10000 # Total population size per stratum

init_prop=.3 # for two stage
init_samp_size=3 # for greedy, e_greedy
epsilon = .1 # for e_greedy

results_greedy<-data.frame(matrix(NA, reps, 1))
colnames(results_greedy) <- c("MSE")

results_neyman<-data.frame(matrix(NA, reps, 1))
colnames(results_neyman) <- c("MSE")

results_prop<-data.frame(matrix(NA, reps, 1))
colnames(results_prop) <- c("MSE")

results_two<-data.frame(matrix(NA, reps, 1))
colnames(results_two) <- c("MSE")

results_egreedy<-data.frame(matrix(NA, reps, 1))
colnames(results_egreedy) <- c("MSE")

# Weird set up is to do it for 20 differnt means
for (mean_iter in 0:99){
  print(mean_iter)
  means<-runif(3, 0, 5)
  vars<-runif(3, 1, 5)
  
  true_mean<-mean(means)
  # create the population data
  pop<-matrix(NA, N, H)
  for (i in 1:H){
    pop[,i]<-rnorm(N, mean = means[i], sd = sqrt(vars[i]))
  }
  
  
  for (iter in 1:(reps/100)){
    rep<-iter+mean_iter*reps/100
    estimates_neyman<-neyman_allocation(pop=pop, means=means, vars=vars, n=n)
    results_neyman[rep,'MSE']<-(mean(estimates_neyman[,'mean_est'])-true_mean)^2
    
    estimates_prop<-prop_allocation(pop=pop, means=means, vars=vars, n=n)
    results_prop[rep,'MSE']<-(mean(estimates_prop[,'mean_est'])-true_mean)^2
    
    estimates_two<-two_stage_allocation(pop=pop, means=means, vars=vars, init_prop=init_prop, n=n)
    results_two[rep,'MSE']<-(mean(estimates_two[,'mean_est'])-true_mean)^2
    
    estimates_greedy<-greedy(pop=pop, means=means, vars=vars, init_samp_size=init_samp_size, n=n)
    results_greedy[rep,'MSE']<-(mean(estimates_greedy[,'mean_est'])-true_mean)^2
    
    estimates_egreedy<-e_greedy(pop=pop, means=means, vars=vars, init_samp_size=init_samp_size, epsilon=epsilon, n=n)
    results_egreedy[rep,'MSE']<-(mean(estimates_egreedy[,'mean_est'])-true_mean)^2
  }
}

print(paste0("avg results_neyman: ", mean(results_neyman[,'MSE'])))
print(paste0("avg results_prop: ", mean(results_prop[,'MSE'])))
print(paste0("avg results_two: ", mean(results_two[,'MSE'])))
print(paste0("avg results_greedy: ", mean(results_greedy[,'MSE'])))
print(paste0("avg results_egreedy: ", mean(results_egreedy[,'MSE'])))

result<-data.frame(matrix(NA, 1, 5))
colnames(result) <- c("neyman", "prop", "two", "greedy", "egreedy")
result[1,'neyman']<-mean(results_neyman[,'MSE'])
result[1,'prop']<-mean(results_prop[,'MSE'])
result[1,'two']<-mean(results_two[,'MSE'])
result[1,'greedy']<-mean(results_greedy[,'MSE'])
result[1,'egreedy']<-mean(results_egreedy[,'MSE'])
write.csv(result, file = "rand_mean_var_100k.csv")