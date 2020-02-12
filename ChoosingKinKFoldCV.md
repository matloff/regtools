#  Clearing the Confusion:  Choosing K in K-Fold Cross Validation

**N. Matloff, UC Davis**

In model selection, a key goal is the avoidance of overfitting.
Typically that is done by running the various models on a *training*
dataset, then validating them on *test* data.  The two datasets are
usually obtaining by one of various kinds of partitioning of one's
original data.  This process is known as *cross-validation* (CV).

The most commonly used form of this is *K-fold cross-validation*.  Here
K is the number of partitioned subsets.  Note that K must be chosen by
the user. 

We also note in passing that in machine learning circles, it's common to
partition one's data into *three* sets:  One chooses one's model by
fitting several to the training set, often meaning choosing values of
several tuning parameters, finally selecting the one with the best
performance on the test set.  The third set, the *validation* set, is
then used to get a realistic evaluation of the performance of the final
model, as even that test set stage will induce optimistic bias.

## Goals of this document

1.  Explain the real motivations behind K-fold CV, and implications for
    choosing K.

2.  Explain the severe limitations of the related theoretical work.

3.  There is no really good solution, but we will make suggestions for
    practical alternatives to CV.  

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
the X values in Group 2 to predict the Y values in that group.  Since
we know the actual Y values, we can then compare how the various models
fared.  We choose the one that did the best.

Or we could split the data into three groups of n/3 data
points each, say Groups 1, 2 and 3.  For each model, we would fit that
model on the 2n/3 data in Groups 1 and 2, and predict Group 3.  We would
next fit to Groups 1 and 3, predicting Group 2, then finally predict
Group 1 from Grups 2 and 3.  We would then select the model that had the
best overall accuracy in this process.

K-fold CV refers to this approach, in which we partition the data into K
groups.  In the descriptions above, we first explained K = 2, then K = 3.  An 
important special case is K = n, sometimes called the "leaving one out" method.

## How we might choose K: a first try

Why burden the user with choosing K?  She may already have many other
tuning paramers to worry about, e.g. node size in random forests.  Here
is the tradeoff:

1.  Consider K = 2, perhaps the most "natural" choice of K.
    A problem is that our results won't be very representative.  After
all, in the end, in order to use our final model in predicting future cases,
we will want to use all n points in our dataset.  But 2-fold CV will
only find the best model for n/2 amount of data.  In view of the fact
that, the larger our dataset, the more detailed a model we can form, K=2
is not really providing what we need.

2.  Now consider the other extreme, K = n.  During CV, we will be
    fitting models to data subsets of size n-1.  But since the best model for
    n-1 amount of data will essentially be the same as for n amount
(though possibly not the case if p is large), this choice of K seems best.  But 
there are two problems.

- Now we are facing a seriously large amount of computation -- for each
  of our candidate models, we do the computing n times, once for each
fold.

- Some theoretical work has indicated that this will not work for large
  n anyway, i.e. the probability of choosing the best model will
actually decrease as n grows.

Thus a "good" value of K would seem to be somewhere between 2 and n.
Well, then, where?

## Role of the theory

There has been much theoretical math done in answering the question of
how one should choose K, beginning with Shao in 1993.  A nice, updated
account is in the book by Clark, Fokoue and Zhang (2009).  See also the
recent paper by Lei (2019).  

There has has been theoretical work aimed at deciding how large
p can be, relative to n, for statistical estimators to have desired
properties.  The major work on this issue continues to be that of
Portnoy (1988); see for instance Anirban DasGupta (2008).

Though such work is impressive math, it is of rather little value in
practice.  Here's why:

-  The mathematical conditions assumed by the theorems are impossible to
   verify in practice, and indeed rather meaningless.

-  Even more important, consider a linear regression setting, so we have
   p+1 parameters. The theory typically assumes that most of the true
regression coefficients are 0, with model selection amounting to
determining which are non-0.  This is not how things work in the real
world.  First, some true coefficients may be non-0 but small.  Second,
it is not necesarily true that we should use all variables with non-0
coefficients, even if we knew which ones they are; there still may be
too many of them to avoid overfitting.  variables may result in
overfitting.  We thus may need to eliminate some, even with non-0
values, a very different setting than what is covered by the theory.

## So, what CAN be done?

Unfortunately, **there is no magic solution here**.  But a reasonable
approach is to limit model complexity (measured by p) in the first place.  

Here is the central issue:  We do CV because of the optimistic bias that
occurs when we assess a model by fitting and predicting on the same
dataset.  This is the motivation for partitioning.  But if p << n, the
amount of bias is negligible.  In such situations, there is no need for
CV.

A good rule of thumb is to keep p < sqrt(n).  This too is based on
theory, but at least with rather minimal assumptions.  Portnoy called
this a "safe" strategy, and indeed I've found it to be conservative in
practice, but one may consider that a virtue here. 

So, a reasonable approach to the CV question would be to keep p <
sqrt(n), obviating the need for CV in the first place.  We then would
choose the richest model, i.e. the one that consists of all p candidate
predictors.

But the old saying, "Easier said than done," does apply.  If our number
of candidate features is a sizable portion of n, or even larger than n,
we still must do some kind of preliminary dimension reduction to attain
p < sqrt(n) before we begin model fitting.  Here are a few possible
approaches:

- Apply PCA to the original candidate features, then use the first
  sqrt(n) principal components.  

- Do some kind of forward selection in linear regression, analysis,
  limiting the number of steps to sqrt(n).  

So for example in the PCA approach, say we have 100 candidate
predictors.  We would run PCA, then fit our model to the first 10
components, and that would be our final model.

Again, none of these is a fully satisfying solution.  For instance, PCA
has its own problems if p >> n, and anyway it is always possible that a
small PC can have a large regression coefficient (Zumel, 2016).  But
they are reasonable solutions worth trying in what remains to be one of
the top knotty problems in statistics.

## References

Bertrand Clarke, Ernest Fokoue, Hao Helen Zhang.  *Principles and Theory
for Data Mining and Machine Learning*, Springer, 2009.

Anirban DasGupta.  *Asymptotic Theory of Statistics and Probability*,
Springer, 2008

Jing Lei.  Cross-Validation with Confidence, arXiv:1703.07904, 2017

Stephen Portnoy.  Asymptotic Behavior of Likelihood Methods for
Exponential Families when the Number of Parameters Tends to Infinity,
*Ann. Statist.*, Volume 16, Number 1 (1988), 356-366.

Jun Shao. Linear Model Selection by Cross-Validation. *Journal of the
American Statistical Association*, 88(422):486–494, 1993

Nina Zumel blog, https://ninazumel.com/tag/principal-components-analysis/,
2016

