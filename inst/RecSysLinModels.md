#  Linear Models in Recommender Systems

**N. Matloff, UC Davis**

## Overview

In the collaborative filtering approach to recommender systems modeling,
a very simple but common model for the rating user i gives to item j is

Y<sub>ij</sub> = &mu; + U<sub>i</sub> + I<sub>j</sub> +
&epsilon;<sub>ij</sub>

where 

- &mu; is the overall mean rating over all users and items

- U<sub>i</sub> is the propensity of user i to rate items liberally or
  harshly 

- I<sub>j</sub> is the propensity of item j to be rated liberally or
  harshly 

- &epsilon;<sub>ij</sub> is an error term, incorporating all other
  factors

The form of the above model suggests using linear model software, e.g. 

``` r
library(dslabs)         
data(movielens)
ml <- movielens
ml <- ml[,c(5,1,6)]
lm(rating ~ .,data=ml)
```

