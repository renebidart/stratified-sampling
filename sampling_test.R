# 1. Look at the preformance of this algo on a few different finite popuations
# with 3 strata, and different variances (all similiar, different, very different)
# 2. Try something with different populatiomn sizes
# 3. Get an overall idea of how these perform by using random populations. 
# 4. Try different sample sizes
# Try out the epsilon greedy one, 
# change the extra samples to be randomly selected

setwd("/Users/rb/Google_Drive/Waterloo/STAT854")
source("strat_samp_func_v2.R")

library(ggplot2)
#out_file='rep1000_reinit100_4strat_n80.csv'

# Simulation parameters
reps<-100
H<-3 # Number of strata
# n_method<-3 
n=100 # total sample size
N=10000 # Total population size


# Population parameters
means<-c(3,2,1)  # Do we need to try this over a set of different means? (bigger mean->higher var in ybar)
vars<-c(1,1,1)
# vars<-c(1,2,3)
# vars<-c(1,1,10)
true_mean<-mean(means)


# create the population data
pop<-matrix(NA, N, H)
for (i in 1:H){
  pop[,i]<-rnorm(N, mean = means[i], sd = vars[i])
}

print(pop)

################    Evaluate performance (MSE and bias) ####################
results_greedy<-data.frame(matrix(NA, reps, 5))
colnames(results_greedy) <- c("MSE", "bias", "strat1", "strat2", "strat3")

results_neyman<-data.frame(matrix(NA, reps, 5))
colnames(results_neyman) <- c("MSE", "bias", "strat1", "strat2", "strat3")

results_prop<-data.frame(matrix(NA, reps, 5))
colnames(results_prop) <- c("MSE", "bias", "strat1", "strat2", "strat3")

for (rep in 1:reps){
  estimates_greedy<-greedy(pop=pop, means=means, vars=vars, n=n)
  results_greedy[,'MSE']<-(mean(estimates_greedy[,'mean_est'])-true_mean)^2
  results_greedy[,'bias']<-(mean(estimates_greedy[,'mean_est']-true_mean))/true_mean
  results_greedy[,'strat1']<-estimates_greedy[1,'num_samples']
  results_greedy[,'strat2']<-estimates_greedy[2,'num_samples']
  results_greedy[,'strat3']<-estimates_greedy[3,'num_samples']
  print('hi')
  
  estimates_neyman<-neyman_allocation(pop=pop, means=means, vars=vars, n=n)
  results_neyman[,'MSE']<-(mean(estimates_neyman[,'mean_est'])-true_mean)^2
  results_neyman[,'bias']<-(mean(estimates_neyman[,'mean_est']-true_mean))/true_mean
  results_neyman[,'strat1']<-estimates_neyman[1,'num_samples']
  results_neyman[,'strat2']<-estimates_neyman[2,'num_samples']
  results_neyman[,'strat3']<-estimates_neyman[3,'num_samples']
  
  estimates_prop<-prop_allocation(pop=pop, means=means, vars=vars, n=n)
  results_prop[,'MSE']<-(mean(estimates_prop[,'mean_est'])-true_mean)^2
  results_prop[,'bias']<-(mean(estimates_prop[,'mean_est']-true_mean))/true_mean
  results_prop[,'strat1']<-estimates_prop[1,'num_samples']
  results_prop[,'strat2']<-estimates_prop[2,'num_samples']
  results_prop[,'strat3']<-estimates_prop[3,'num_samples']
  print('hi')
}
results_prop
results_neyman
results_greedy




################# Look at individual strata    ######################
results_greedy<-data.frame(matrix(NA, n_algo*reps*num_strat, 7))
colnames(results_greedy) <- c("method", "stratum", "num_samples","mean_est", "true_mean", "variance_est", "true_var")

results_neyman<-data.frame(matrix(NA, n_algo*reps*num_strat, 7))
colnames(results_neyman) <- c("method", "stratum", "num_samples","mean_est", "true_mean", "variance_est", "true_var")

results_prop<-data.frame(matrix(NA, n_algo*reps*num_strat, 7))
colnames(results_prop) <- c("method", "stratum", "num_samples","mean_est", "true_mean", "variance_est", "true_var")


for (rep in 1:reps){
  results_greedy<-greedy(pop=pop, means=means, vars=vars, n=n)
  results[num_strat*rep:num_strat*rep+3,'method']<-"greedy"
  results[num_strat*rep:num_strat*rep+3,'method']<- estimates_greedy[,"num_samples"]
  results[num_strat*rep:num_strat*rep+3,'mean_est']<- estimates_greedy[,"num_samples"]
  results[num_strat*rep:num_strat*rep+3,'true_mean']<- estimates_greedy[,"num_samples"]
  results[num_strat*rep:num_strat*rep+3,'variance_est']<- estimates_greedy[,"variance_est"]
  results[num_strat*rep:num_strat*rep+3,'true_var']<- estimates_greedy[,"true_var"] 
  
  estimates_neyman<-neyman_allocation(means, vars, N=N)
  estimates_two<-two_stage_adj(means, vars, init_prop=init_prop, N=N)
  
}
    