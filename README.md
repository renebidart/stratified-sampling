# Stratified Sampling with Unknown Variance

### Iterative method for stratified sampling with unknown variances. 
This is a method for estimating the mean of a sample using iterative stratifed sampling with unknown variance. This is done by sampling from the stratum that will cause the largest expected  decrease in variation at each iteration. This does not totally take into account our uncertainity in the variances, so some methods from multi-armed bandits can be used to improve it. It is contrasted with the proportional allocation, two-sample variance estimation method, and the optimal Neymamn allocation. 

Functions for the different strategies are included in strat_samp_func.R, while some rough tests are in sampling_test.R.
Details are included in the report.



