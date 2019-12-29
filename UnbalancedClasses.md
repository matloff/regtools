# Clearing the Confusion on Unbalanced Class Data 

Many resources on machine learning (ML) classification problems
recommend that if one's dataset has unbalanced class sizes, one
should change the data to have equal class counts.  Yet it is shown here
that this is both unnecessary and often harmful.

## Overview

Illustration of the (perceived) problem and
offered remedies appear in numerous parts of the ML literature, ranging
from [Web tutorials](https://www.datacamp.com/community/tutorials/diving-deep-imbalanced-data)
to [the research literature](https://link.springer.com/article/10.1186/s40537-018-0151-6#Sec2).  Major packages, such as
[caret](https://cran.r-project.org/package=caret) and
[mlr3](https://cran.r-project.org/package=mlr3), also offer remedies.


> All of these sources recommend that you artificially equalize the class
> counts in your data, via resampling.
> Upon closer inspection, though, one sees that **this
> is generally inadvisable, indeed harmful,** for several reasons:

* Undersampling is clearly problematic:  Why throw away data?
  **Discarding data weakens our ability to predict new cases.**

* The data may be unbalanced *for a reason*.  Thus the imbalance itself
  is useful information, again resulting in reduced predictive power if
it is ignored.

* There are **principled alternatives to
  resampling,** including an adjustment formula to be presented here.  (See also
[my book on regression, classification and ML](https://books.google.com/books?id=IHs2DwAAQBAJ&printsec=frontcover&dq=matloff&hl=en&newbks=1&newbks_redir=0&sa=X&ved=2ahUKEwje9LbA5dLmAhVJsZ4KHTvdADIQ6AEwAHoECAQQAg#v=onepage&q=matloff&f=false).)

In other words: 

> Resampling methods **are both harmful and unnecessay**.

## Motivating example:  Credit card fraud

This is a 
[Kaggle dataset](https://www.kaggle.com/mlg-ulb/creditcardfraud).
Quoting from the Kaggle site,

> The datasets contains transactions made by credit cards in September
> 2013 by european cardholders. This dataset presents transactions that
> occurred in two days, where we have 492 frauds out of 284,807
> transactions. The dataset is highly unbalanced, the positive class
> (frauds) account for 0.172% of all transactions.

Due to privacy concerns, PCA has been used to replace most of the
features.  So the PCs make up most of our new features, though two
original features have been retained.

## Motivating example:  Optical letter recognition

This is a well-known
[UCI Machine Learning Repository dataset](https://archive.ics.uci.edu/ml/datasets/Letter+Recognition).  Again quoting from the site:

> The objective is to identify each of a large number of black-and-white
> rectangular pixel displays as one of the 26 capital letters in the
> English alphabet. The character images were based on 20 different fonts
> and each letter within these 20 fonts was randomly distorted to produce
> a file of 20,000 unique stimuli. Each stimulus was converted into 16
> primitive numerical attributes (statistical moments and edge counts)
> which were then scaled to fit into a range of integer values from 0
> through 15. 

This dataset is close to balanced, with each letter appearing about 775
times.

## Key issue:  How were the data generated?

- The fraud data is *imbalanced*, but *naturally so*.  Assuming the
  two-day data collection period was typical, the population class
probability for fraud will be about what we see in the data, about 0.172%.

- The letters data is *balanced*, but only *artificially so*.  The
  curator of the dataset wanted the data to have about the same number
of instances of each letter.  But in general English usage, letter occur
with quite different frequencies:

>   E          12.02%
<br>
>   T           9.10%
<br>
>   A           8.12%
<br>
>   O           7.68%
<br>
>   I           7.31%
<br>
>   N           6.95%
<br>
...
<br>
...
<br>
>   Q           0.11
<br>
>   J           0.10
<br>
>   Z           0.07

([source](http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html)).

## What the ML algorithm is thinking

ML algorithms take your data literally.  Say you have a two-class
setting, for Classes 0 and 1.  If about 1/2 your data is Class 1, then
the algorithm, whether directly or indirectly, operate under the
assumption that the true population class probabilities are each about 0.5.

In the letter data, since the sampling was performed with the intention
of having about the same number of instances for each letter, the
algorithm you use will then assume the true probabilities of the letters
are about 1/26 each.  We know that is false, as the above table shows.

So, if you do resampling to make your data balanced, you are fooling
your ML algorithm.  Though that conceivably could be done responsibly, it
may actually undermine your ability to predict new cases well.

## Goals and perceptions of problems

As is often the case, clear, precise statements of problems and goals
are needed to come up with good, *effective* solutions.  Let's consider
the two datasets in turn.

It will be easier to discuss the second dataset first.

### Letter recognition data

Intuitively, if we ignore our knowledge of the class probabilities --
12.02% --- we are substantially reducing our predictive ability.

In an experiment in my book, I first fit a logistic model to a training
set of 14,000, then used it to predict a test set of 6,000.  **This
resulted in correct classification rate of 71.9%.**

I then sampled from the dataset according to the
realistic frequencies above, and fit the logit model to this new data.
**The correct classification rate jumped up to 83.6%!**

As you can see, **balanced data can be our enemy**.

But the good news is that using the adjustment formula below, one can
still use the correct class probabilities even if the data is balanced.

### Credit card fraud data

In the credit card fraud data, the perceived problem is that, if we use
the data to classify new cases, the extreme imbalance in the data wll
mean that we will always (or maybe nearly always, depending on what ML
method we use) guess that the new case is not fraudulent.

With this approach, we'd miss all the fraudulent cases.  There aren't
many of them, but even the small number of cases can cause big damage.
Yet **the solution is not to force the data to be balanced** (by
resampling).

Instead, we could formally assign loss values to the two kinds of error,
i.e. false positives and false negatives, from the fraud point of view.
But it's much easier to take an informal approach:  We simply calculate
the conditional probabilities of the classes, given the features, and
have our code flag any that are above a threshhold we specify.
(Actually, `mlr3` does list something similar to this as an alternative
to artificially balancing the data.)

In the credit card fraud case, we may decide, say, to flag any transaction
 with at least a 25% chance of being fraudulent.

In order to do this, we need to set up our code to extract the
probabilities, shown below.

## Note on terminology 

As seen above, we refer to the class probabilities for given feature
values as *conditional probabilities*.  The overall class probabilities,
e.g. the 0.000172 value above, are *unconditional probabilities*.

## Extracting conditional probabilities from various ML methods  

Here's how to do this with various ML methods in R.

- **logit:** Say `glmout` is the output you get from applying `glm()`
  to your training data.  In predicting a new case `newx`, do

``` r
predict(glmpit,newx,type='response')
```

- **random forests:** Say we use the `randomForest` package, and that we
  assign the output of `randomForest()` to `rfout`.  Then `rfout$votes`
will contain the probabilities.

- **boosting:** E.g. with the `gbm` package, the procedure is similar to
  that of `glm()` above.

- **neural nets:** E.g. with the `neuralnet` package, call `compute()`
  then take the `net.result` component.

Actually, both `caret` and `mlr3` allow one to extract probabilities in
this manner.  But again, this should be done instead of forcing balance,
as recommended by those packages.

## An adjustment formula

For convenience, we'll assume the two-class setting here, with Class 0
and Class 1. This is the code for adjustment:

``` r
classadjust <- function(condprobs,wrongratio,
      trueratio) {
   fratios <- (1 / condprobs - 1) * (1 / wrongratio)
   1 / (1 + trueratio * fratios)
}
```

where 

- `condprobs` is the vector of conditional probabilities for
  the new cases, reported by the software

- `wrongratio` is the ratio of the Class 0 to Class 1 datapoints in our
  dataset

- `trueratio` is the actual such ratio 

The return value is the set of adjustment probabilities for the new
cases.

For instance, suppose we are in a setting in which there are equal
numbers of the two classes in our dataset, yet we know the true
(unconditional) class probabilities are 0.2 and 0.8 for Classes 0 and 1.
Then `wrongratio` would be 0.5/0.5 = 1.0, and `trueratio` would be
0.2/0.8 = 0.25. 

What if the data are balanced but the true unconditional class
probabilities are unknown?  Other than creating various scenarios
involving the true values and exploring the results, there is not much
that we can do.  In essence, the result is a maximum-likelihood kind of
situation.  The class with highest (nominal) conditional probability
will be the one that makes our feature data most likely (see Appendix
below), which is very different from the question we want to ask, Which
class is most likely given the feature set. 

## Summary

- There's really no need to artificially balance your data, and doing so
  could be harmful.

- Please don't throw away data.

- Conversely, creating extra data by up-sampling distorts the analysis
  and may undermine our predictive power.

- If your data is "naturally" unbalanced, meaning it reflects population
  structure as in the credit card fraud example, don't artificially
force balance.  Instead, choose a threshhold for conditional
probabilities, and flag new cases that exceed it.

- If your data is unrealistically balanced, as in the letters example,
  and the true unconditional class probabilities are known, use the
adjustment formula to convert the reported unconditional probabilities to
realistic ones, and classify using them.

## Appendix: derivation of the adjustment formula

(For ease of notation etc., no distinction will be made here between
sample and population quantities.)

Say there are two classes, labeled 1 and 0.  Let Y denote the lable and
X denote the features, and say we have a new case with X = t.  Then

P(Y = 1 | X = t) = p f<sub>1</sub>(t) / [p f<sub>1</sub>t) + (1-p)
f<sub>0</sub>(t)]  (Eqn. 1)

where p is P(Y = 1), the unconditional probability of Class 1, and
f<sub>i</sub>(t) is the conditional density of X within Class i.

Rewrite the above as

P(Y = 1 | X = t) = 1 / [1 + {(1-p)/p} f<sub>0</sub>(t) / f<sub>1</sub>(t)]
 (Eqn. 2)

Now suppose the analyst artificially changes the class counts in the
data (or, as in the letters example, the data is artificially sampled by
design), with proportions q and 1-q for the two classes.  In the case of
artificially equalizing the class proportions, we have q = 0.5.  Then
the above becomes

P(Y = 1 | X = t) = 1 / [1 + {(1-q)/q} f<sub>0</sub>(t) / f<sub>1</sub>(t)]
 (Eqn. 3)

As noted earlier, what the ML algorithm is computing, directly or
indirectly, is P(Y = 1 | X = t).  Moreover, as also noted earlier,
even `caret` and `mlr3` do make these quantities available.  

So we can solve for f<sub>0</sub>(t) / f<sub>1</sub>(t):

f<sub>0</sub>(t) / f<sub>1</sub>(t) = (g - 1) q/(1-q)  (Eqn. 4)

where

g = 1/P(Y = 1 | X = t)

We can now substitute in (Eqn. 2) from (Eqn. 4) to get the proper
conditional probability.

In the general m-class case. classes 0,1,...,m-1, the equations become
complicated.  A simpler solution is to use AVA, the All vs. All approach
to multiclass prediction, which many analysts prefer over OVA, One vs.
All, anyway.  (Again, see my book, or for example 
[these MIT class
notes](https://www.mit.edu/~9.520/spring09/Classes/multiclass.pdf).)

AVA solves an m-class problem via a series of 2-class problems.  For
each pair of classes i and j, we pit i against j, running our ML
algorithm on the reduced dataset of cases involving only those two
classes.  In each such pitting, we record which class "won," i.e. was
predicted for X = t.  We then predict this case to be whichever class
garnered the most "votes."

The `regtools` package offers such analysis for the logit and k-NN
methods.  The code is easily modifiable for other algorithms.

(Computationally AVA runs the analysis more times than OVA, but on
smaller datasets.  Moreover, if the computational cost for the algorithm
is larger than O(n), AVA may actually be a computational win.)

## Appendix:  What is really happening if you use equal class probabilities?

Say we have balanced data, so the q<sub>i</sub> = 1/m for each i.  Then
in predicting the class of a new case having X = t, the above analysis
shows that our guessed class is

j = arg max<sub>i</sub> f<sub>i</sub>(t)

In other words, you are in effect asking, "Under which class j would our
data X = t be most likely?"  That's completely different from the
question we really are interested in, "Which class j is most likely,
given X = t?"  But it shows that if we artificially equalize the class
sizes, we are finding the Maximum Likelihood Estimate of j, **if the
p<sub>i</sub> are unknown.**

If we really don't know the true class probabilities p<sub>i</sub>, and
artificially equalize the class sizes, we are at least getting a kind of MLE.
However, what then is the practical meaning?  Unless you are a
subjective Bayesian (I am not), setting a flat prior, there is not much
here.




