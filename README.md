# stratified-sampling

### Iterative method for stratified sampling with unknown variances. 
With known variances this is simple, you can compute the optimal Neyman allocation. Otherwise you must estimate the variance.

**Traditional methods** use either:
* Pilot studies to provide a variance estimate
* Estimate variance in other ways

**Iterative Method**
* Take samples one by one, allocating to the the stratum with the highest expected benefit
* Can use the greedy strategy, defined by a simple formula or with adjustments to lower the risk of a very bad result, similair to exploitation exploration in multi-arm bandit problems.

