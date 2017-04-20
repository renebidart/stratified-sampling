# stratified-sampling

Iterative sampling for stratified sampling with unknown stratum variance. 

Traditional methods use either:
* Known variance to compute the optimal Neyman allocation
* Pilot studies to provide a variance estimate

Instead, if variance is unknown, a better alternative to the pilot study is to take samples one by one, and at each sampling stage sample from the stratum with the highest expected benefit. Can use the greedy strategy, or with adjustments to lower the risk of a very bad result, similair to exploitation exploration in multi-arm bandit problems.

