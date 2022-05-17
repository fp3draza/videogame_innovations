
## Non-linear regression

In this case we wrote the function `stretched_exp_fit()` based on the `nlsLM` routine of the `minpack.lm` R-package, which is also contained in the file `code/analyse_data/functions_fit.R`. To suppress the intrinsic correlation between the 




## Data collapsing 

A standard procedure to verify (visually) to which extent a law is obeyed by a set of data is by replotting the training datasets as a so-called *master curve*, using the *a posteriori* knowledge of the predictors. Starting from Eq.(1) the master curve $F[x]$ is obtained with the following transformation 

...

Below we show the result of this transformation: 

![alt text](https://github.com/[username]/[reponame]/blob/main/collapse_all.png?raw=true)



