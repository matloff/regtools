#  Clearing the Confusion:  Choosing K in K-Fold Cross Validation

N. Matloff, UC Davis

In model selection, a key goal is the avoidance of overfitting.
Typically that is done by running the various models on a *training*
dataset, then validating them on *test* data.  The two datasets are
usually obtaining by one of various kinds of partitioning of one's
original data.  This process is known as *cross-validation* (CV).

The most commonly used form of this is *K-fold cross-validation*.  Here
K is the number of "folds," to be explained below.  Note that K must be
chosen by the user. 

We also note in passing that in machine learning circles, it's common to
partition one's data into *three* sets:  One choose one's model,
often meaning choosing values of several tuning parameters, finally
selecting the one with the best performance on the test set.  The third
set, the *validation* set, is then used to get a realistic evaluation 
of the performance of the final model.

## Goals of this document

1.  Explain the real motivations behind K-fold CV, and implications for
    choosing K.

2.  Explain the severe limitations of the related theoretical work.

3.  Make a recommendation for a practical alternative to CV.

## Notation

Say we are using a vector of predictor variables/features X to predict
Y.  The latter may be continuous, categorical or whatever.  Let n denote
our number of data points, and let p be the number of predictors.

If say we are predicting human weight from height and age, with a
dataset consisting of 1000 people, then p = 2 and n = 1000.

## What is K-fold CV?

Let n denote the number of data points.  In the simplest form, we
randomly split our data into two groups of n/2 points each.  We fit each
of the candidate models to Group 1.  We then temporarily pretend
we don't know the Y values in Group 2, and do the following:

For each model, we take the fitted model from Group 1, and apply it on
the X values in Group 23 to predict the Y values in that group.  Since
we know the actual Y values We can then compare how the various models
fared.  We choose the one that did the best.

Or we could split the data into three groups ("folds"), of n/3 data
points each, say Groups 1, 2 and 3.  For each model, we would fit that
model on the 2n/3 data in Groups 1 and 2, and predict Group 3.  We would
next fit to Groups 1 and 3, predicting Group 2, then finally predict
Group 1 from Grups 2 and 3.

K-fold CV refers to this approach, in which we partition the data into K
groups.  An important special case is K = n, the "leaving one out"
method.

## How we might choose K: a first try

Why burden the user with choosing K?  She may already have many other
tuning paramers to worry about, e.g. node size in random forests.  Here
is the tradeoff:

1.  Consider K = 2, perhaps the most "natural" choice of K.
    A problem is that our results won't be very representative.  After
all, in the end, in our to use our final model in predicting future cases,
we will want to use all n points in our dataset.  But 2-fold CV will
only find the best model for n/2 amount of data.  In view of the fact
that, the larger our dataset, the more detailed a model we can form, K=2
is not really providing what we need.

2.  Now consider the other extreme, K = n-1. Since the best model for
    n-1 amount of data will essentially be the same as for n amount,
this choice of K seems best.  But there are two problems.

- Now we are facing a seriously large amount of computation -- for each
  of our candidate models, we do the computing n times, once for each
fold.

- Some theoretical work has indicated that this will not work for large
  n anyway, i.e. the probability of choosing the best model will
actually decrease as n grows.

Thus a "good" value of K is somewhere between 2 and n.  Well, then,
where?

## Role of the theory

There has been much theoretical math done in answering the question of
how one should choose K, beginning with Shao in 1993, most recently by
Lei (2019).  There has has been theoretical analysis of how large one
can have for the value of p, for a given n, such as work by Kruskal in
1984.  In this era of Big Data, there is special interest in the case p
>> n.  See for instance books by Clark, Fokoue and Zhang (2009) and
Anirban DasGupta (2008).

Though such work is impressive math, it is of rather little value in
practice.  Here's why:

1.  Consider a linear regression setting, so we have p+1 parameters. The
    theory typically assumes that some of the true regression
coefficients are 0, with model selection amounting to determining which
are non-0.  This is not how things work in the real world.  Even if
coefficients are not 0 are nearly so, using all our variables may result
in overfitting.  We thus must eliminate some, even with non-0 values.

2.  The conditions assumed by the theorems are impossible to verify in
    practice, and indeed rather meaningless.

## So, what CAN be done?

Unfortunately, there is no magic solution here.  But a reasonable
approach is to limit model complexity in the first place.  A good
(though conservative) rule of thumb is to keep p < sqrt(n).  This too is
based on theory, but at least with rather minimal assumptions.

In this manner, we do not need to split the sample at all, because
predicting on the same data that we fit on is not an issue when 
p << n.

We then can, for instance, do forward selection in linear regression,
analysis, limiting the number of steps to sqrt(n).  Again, not a fully
satisfying solution, but a reasonable one.

