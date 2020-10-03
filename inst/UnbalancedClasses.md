#  Clearing the Confusion: Unbalanced Class Data  

Many resources on machine learning (ML) classification problems
recommend that if one's dataset has unbalanced class sizes, one
should modify the data to have equal class counts.  Yet it is shown here
that this is both unnecessary and often harmful.  Alternatives are
presented.

## Overview

Illustrations of the (perceived) problems and
offered remedies appear in numerous parts of the ML literature, ranging
from [Web tutorials](https://www.datacamp.com/community/tutorials/diving-deep-imbalanced-data)
to [the research literature](https://link.springer.com/article/10.1186/s40537-018-0151-6#Sec2).  Major packages, such as
[caret](https://cran.r-project.org/package=caret),
[parsnip](https://cran.r-project.org/package=parsnip),
and
[mlr3](https://cran.r-project.org/package=mlr3), also offer remedies.

All of these sources recommend that you artificially equalize the class
counts in your data, via various resampling methods.  Say for instance
we are in the two-class case, and have class sizes of 20000 and 80000
for class 1 and class 0, respecitvely.  Here are some ways to rebalance.

* Randomly discard 60000 data points from class 0.

* Resample 80000 data points from the 20000, with replacement.

* Using distribution approximation methods such as SMOTE, resample 80000
  from the 20000.

Upon closer inspection, though, one sees that **this
is generally inadvisable, indeed harmful,** for several reasons:

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

## Motivating example:  Credit card fraud data

This is a 
[Kaggle dataset](https://www.kaggle.com/mlg-ulb/creditcardfraud).
Quoting from the Kaggle site,

> The datasets contains transactions made by credit cards in September
> 2013 by european cardholders. This dataset presents transactions that
> occurred in two days, where we have 492 frauds out of 284,807
> transactions. The dataset is highly unbalanced, the positive class
> (frauds) account for 0.172% of all transactions.

Note the phrase, "highly unbalanced."

Due to privacy concerns, PCA has been used to replace most of the
features,  though two original features have been retained.

## Motivating example:  Optical letter recognition data

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

The examples above illustrate two important cases:

- The fraud data is *imbalanced*, but *naturally so*.  Assuming the
two-day data collection period was typical, the population class
probability for fraud will be about what we see in the data, 0.172%.

- The Letters data is *balanced*, but only *artificially so*.  The
curator of the dataset wanted the data to have about the same number
of instances of each letter.  But in general English usage, letters occur
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

## Note on terminology 

We refer to the class probabilities for given feature
values as *conditional class probabilities*.  The overall class probabilities,
e.g. the 0.000172 value above, are *unconditional class probabilities*.

## What your ML algorithm is thinking

ML algorithms take your data literally.  Say you have a two-class
setting, for Classes 0 and 1.  If about 1/2 your data is Class 1, then
the algorithm, whether directly or indirectly, operates under the
assumption that the true population class probabilities are each about 0.5.

In the Letters data, since the sampling was actually *designed* to have
about the same number of instances for each letter, the algorithm you
use will then assume the true probabilities of the letters are about
1/26 each.  We know that is false, as the above table shows.

So, if your sampling scheme artificially creates balanced data, as in
the Letters data, or if you do resampling to make your data balanced, as
is commonly recommended, you are fooling your ML algorithm.  Though that
conceivably could be done responsibly, it may actually undermine your
ability to predict new cases well.

Note too the role of the *overall rate of correct classification*.  To
maximize that, say in the two-class case, your ML algorithm will guess
Class 1 for a new case if and only if the estimated conditional
probability of Class 1 is greater than 0.5.  This rule assumes, though,
that there are equal costs of the two types of misclassification, i.e.
that a false positive and false negative should be given equal weight.

In many applications this is not the case, and the overall rate is of
lesser interest.  This is especially true in situations in which the
data is substantially unbalanced, though it should be kept in mind that
these are two separate issues: imbalance of the data, and relative
weights of false positives and negatives.

We will return to this point later.

## Goals and perceptions of problems

As is often the case, clear, precise statements of problems and goals
are needed to come up with good, *effective* solutions.  Let's consider
the two datasets in turn.  It will be easier to discuss the second
dataset first.

### Letter recognition data

For now, we will assume the goal is to maximize the overall rate of
correct classification.

Intuitively, if we ignore our knowledge of the class probabilities --
12.02% etc. --- we are reducing our overall predictive ability, i.e. 
our overall rate of correct classification.  As noted

In an experiment in my book (updated here), I sampled from the dataset
according to the realistic frequencies above, thus producing realistic
data.  I then fit a logistic model to a training set of 14,000, and used
it to predict a test set of 6,000, producing 6,000 conditional
probabilities for each letter.  Finally, I predicted the 6,000 letters,
first without applying the adjustment formula to those probabilities and
then with it.

Without the adjustment formula, I got a **correct classification
rate of 69%.**  But then using the formula, **the correct
classification rate rose to 76%.**

One can actually do much better on this dataset, either by adding
quadratic terms to the logit model, or by using a nonparametric method.
(In my book, I get 97% accuracy using random forests.)  But that is not
the point; instead, the point is that for any given ML algorithm on
balanced data, **we can do better by using the adjustment formula** (if,
of course, the correct class probabilities are known).

As you can see, balanced data can be our enemy, if our goal is overall
rate of correct classification.

### Credit card fraud data

In the credit card fraud data, the perceived problem is that, if we use
the data to classify new cases, the extreme imbalance in the data wll
mean that we will always guess that the new case is not fraudulent.

With this approach, we'd miss all the fraudulent cases.  There aren't
many of them, but even the small number of cases can cause big damage,
i.e. we are very worried about false negatives (positive meaning we
guess the transaction is fraudulent).  Yet **the solution is not to
force the data to be balanced.** 

## First alternative

We *could* formally assign specific
numerical relative weights to the two kinds of error, i.e. false
positives and false negatives.  One could then trick our ML algorithm
into achieving those weights, via mathematically derivations we won't go
into here.

## Better alternative

**But it's much easier to take an informal approach:**  We simply calculate
the conditional probabilities of the classes, given the features, and
have our code flag any that are above a threshhold we specify.
(Actually, `mlr3` does mention something similar to this as an alternative
to artificially balancing the data.)

In the credit card fraud case, we may decide, say, to flag any transaction
with at least a 25% chance of being fraudulent.  We could then
investigate these further by hand.

**This is the natural, simple approach,** explainable to anyone
regardless of whether they have a good background in stat/ML.

The code would look like this
(using the same data to fit and predict, just an illustration), say for
**glm()**:

``` r

> glmout <- glm(Class ~ .,data=ccf,family=binomial)
> condprobs <- predict(glmout,ccf,type='response')
> tocheck <- which(condprobs > 0.25)
> names(tocheck) <- NULL
> head(tocheck)
[1]  542 6109 6332 6335 6337 6339

```

So we'd check cases 542, 6109 and so on by hand.

For the **randomForest** package, a bit more work (could write a wrapper
for it):

``` r

> ccf$Class <- as.factor(ccf$Class)
> rfout <- randomForest(Class ~ .,data=ccf)
> predout <- predict(rfout,ccf,type='response')
> treeguesses <- predout$individual  # class guesses for each tree
> tgs <- as.matrix(treeguesses)
# tgs[i,] has guesses for case i, '1's and '0's, from each tree
> probs <- apply(tgs,1,function(rw) mean(as.numeric(rw)))
> tocheck <- which(probs > 0.25)
> head(tocheck)
[1]   70  542  624 1747 4921 6109

```

Other ML algorithms/packages are similar.  E.g. for boosting, say with
the **gbm** package, the procedure is similar to that of **glm()** above.

For neural networks, e.g.  with the **neuralnet** package, call
**compute()** then take the **net.result** component.  The **keras**
case is a bit more complicated, but still very possible.

The SVM case is different, as we will explain later in this document.

Actually, both `caret` and `mlr3` allow one to extract probabilities in
this manner.  But again, this should be done instead of forcing balance,
which is recommended by those packages.

## The adjustment formula

Recall the Letters example.  The sampling design itself was balanced,
but artificially so.  Each letter had about the same frequency in the
data, in spite of the fact that the frequencies vary widely in actual
English.

The adjustment formula allows us to take the output of an ML fitting on
the Letters data, and convert it to the proper form for the correct
class probabilities.

In general, this adjustment method is useful in three kinds of settings:

* The data are artificially rebalance.

* The sampling scheme for the data is designed to have balanced class
  data.

* The class probabilities in some application later change.

For now, we'll assume the two-class setting here, with Class 0
and Class 1. This is the code for adjustment (the function is part of
the `regtools` package):

``` r

classadjust <- function(condprobs,wrongprob1,trueprob1) {
   wrongratio <- (1-wrongprob1) / wrongprob1
   fratios <- (1 / condprobs - 1) * (1 / wrongratio)
   trueratio <- (1-trueprob1) / trueprob1
   1 / (1 + trueratio * fratios)
}

```

where 

- `condprobs` is the vector of conditional class probabilities for
  the new cases, reported by the software

- `wrongratio` is the ratio of the numbers of Class 0 to Class 1
  datapoints in our dataset

- `trueratio` is the actual such ratio 

The return value is the set of adjusted conditional class  probabilities
for the new cases.

For instance, suppose we are in a setting in which there are equal
numbers of the two classes in our dataset, yet we know the true
(unconditional) class probabilities are 0.2 and 0.8 for Classes 0 and 1.
Then `wrongratio` would be 0.5/0.5 = 1.0, and `trueratio` would be
0.2/0.8 = 0.25. 

# The case of balanced data but unknown true class probabilities

What if the data are balanced but the true unconditional class
probabilities are unknown?  Other than creating various scenarios
involving the true values and exploring the results, there is not much
that we can do.  In essence, the result is a maximum-likelihood kind of
situation:  Our predicted class will be the one whose (nominal)
conditional probability makes our feature data most likely given the
class (see Appendix below), **which is very different from the question
we want to ask, Which class is most likely given the feature set?** 
We'll get answers ("Hey, the computer said such and such!"), but those
answers will be of questionable value unless the predictors have very
strong predictive power.

Note that setting utility values for "false positive," "false negative"
and so on does not solve the problem.  One still needs to factor in the
class probabilities in order to maximize expected utility.

Once again, **balancing the data will not help.**

Frank Harrell 
[says it well](https://www.fharrell.com/post/classification/):

> For this reason the odd practice of subsampling the controls is used in
> an attempt to balance the frequencies and get some variation that will
> lead to sensible looking classifiers (users of regression models would
> never exclude good data to get an answer). Then they have to, in some
> ill-defined way, construct the classifier to make up for biasing the
> sample. It is simply the case that a classifier trained to a 1⁄2 [q =
> 1/2] prevalence situation will not be applicable to a population with a
> 1⁄1000 [p = 1/1000] prevalence. 

## Estimating conditional probabilities with SVM

This document has recommended making classification decisions on the
basis of conditional probabilities rather than predicted classes.  But
some classification algorithms, notably SVM, do not provide these
probabilities.  How many they be obtained separately?

One popular method for doing this is *Platt scaling*. This involves
assuming (something like, there are variations) that conditional
probability of class 1 given the SVM score has the form of a logistic
function (sigmoid).

The `regtools` function `labelsToProbs` makes no assumptions.  It simply
does k-Nearest Neighbor analysis of the conditional probability of
(e.g. SVM-)predicted class 1 given the feature vector X.

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

- If your data is unrealistically balanced, as in the Letters example,
  and the true unconditional class probabilities are known, use the
adjustment formula to convert the reported unconditional probabilities to
realistic ones, and classify using them.

- If your data is unrealistically balanced but the true unconditional
  class probabilities are unknown, recognize that your ML analysis may
have only a very restricted interpretation and value.

## Appendix: derivation of the adjustment formula

(For ease of notation etc., no distinction will be made here between
sample and population quantities.)

Say there are two classes, labeled 1 and 0.  Let Y denote the label and
X denote the features, and say we have a new case with X = t.  Then

P(Y = 1 | X = t) = p f<sub>1</sub>(t) / [p f<sub>1</sub>t) + (1-p)
f<sub>0</sub>(t)]  
<br>
(Eqn. 1)

where p is P(Y = 1), the unconditional probability of Class 1, and
f<sub>i</sub>(t) is the conditional density of X within Class i.
(If X is a discete random variable, substitute a probability for f.)

Rewrite the above as

P(Y = 1 | X = t) = 1 / [1 + {(1-p)/p} f<sub>0</sub>(t) / f<sub>1</sub>(t)]
<br>
(Eqn. 2)

Now suppose the analyst artificially changes the class counts in the
data (or, as in the Letters example, the data is artificially sampled by
design), with proportions q and 1-q for the two classes.  In the case of
artificially equalizing the class proportions, we have q = 0.5.  Then
the above becomes, in the eyes of your ML algorithm,

P(Y = 1 | X = t) = 1 / [1 + {(1-q)/q} f<sub>0</sub>(t) / f<sub>1</sub>(t)]
<br>
(Eqn. 3)

As noted earlier, what the ML algorithm is computing, directly or
indirectly, is P(Y = 1 | X = t).  Moreover, as also noted earlier, even
`caret` and `mlr3` do make these quantities available for the various t,
so we can solve for f<sub>0</sub>(t) / f<sub>1</sub>(t):

f<sub>0</sub>(t) / f<sub>1</sub>(t) = (g - 1) q/(1-q)  
<br>
(Eqn. 4)

where

g = 1 / P(Y = 1 | X = t)

We can now substitute in (Eqn. 2) from (Eqn. 4) to get the proper
conditional probability.

The general m-class case. classes 0,1,...,m-1, actually reduces to the
2-class case, because 

P(Y = i | X = t)

can be viewed as the conditional probability of class i vs. all other
classes.

## Appendix:  What is really happening if you use equal class probabilities?

Say we have balanced data, so the q<sub>i</sub> = 1/m for each i.  Then
in predicting the class of a new case having X = t, Equation 1 becomes

P(Y = i | X = t) = f<sub>i</sub>(t) / 
[
&Sigma;<sub>j</sub> 
f<sub>j</sub>t) 
]

for i = 0,1,...,m-1, since all the 1/m factors cancel.

This shows that our guessed class is

j = arg max<sub>i</sub> f<sub>i</sub>(t)

In other words, you are in effect asking, "Within which class j would our
data X = t be most likely?"  That is, we are maximizing (say in the discrete
case)

P(X = t | Y = j)

over j. That's completely different from the question we really are
interested in, "Which class j is most likely, given X = t?", i.e.
maximizing

P(Y = j | X = t )
  
But it does show that if we artificially equalize the class sizes, we
are finding the Maximum Likelihood Estimate of j, **if the p<sub>i</sub>
are unknown.**

If we really don't know the true class probabilities p<sub>i</sub>, and
artificially equalize the class sizes, we are at least getting a kind of MLE.
However, what then is the practical meaning?  Unless you are a
subjective Bayesian (I am not), setting a flat prior, there is not much
here.




