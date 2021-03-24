#  Clearing the Confusion: Poisson regression

One of the most commonly used statistical methods is the *general linear
model*, implemented in R as the **glm()** function.  The most common
usage of that function is for *logistic regression*, but it's also
popular for *Poisson regression* (PR), the subject of this entry in our
Clearing the Confusion series.  PR is often used as a regression model
in which the response variable Y consists of counts, typically in the
one- or two-digit range.

This is not a tutorial on Poisson regression (PR).  It is assumed that
the reader already has some familiarity with the model, and the
treatment here is somewhat critical.  There are two main themes:

* Unlike the linear and logistic cases, there is no theoretical or
  modeling justification for PR.

* PR changes the nature of the &beta; coefficients in major ways that
  may not be desirable.

## Notation

Y: the response variable

X<sub>1</sub>, X<sub>2</sub>,...,X<sub>p</sub>:  the predictor
variables

X: (X<sub>1</sub>, X<sub>2</sub>,...,X<sub>p</sub>)

n: the sample size, i.e. number of data points

## The linear and logistic models: theoretical justifications

It will be helpful to first take a brief look at the theory behind the
assumptions of standard linear and logistic models.

**linear model:**  The classic linear model assumes that:  mean Y
given X is a linear combination of the X<sub>i</sub>; the conditional
distribution of Y given X is Gaussian; and the conditional variance of Y
given X is constant in X (homoscedasticity).

Key point:  *All the assumptions of this model hold if (X,Y) has a
multivariate normal distribution.*

In other words, the multivariate normal model implies the classic linear
model.

**logistic model:** For binary Y, the logistic model is

P(Y = 1 | X) = 1 / [1 + exp{-(
&beta;<sub>0</sub> +
&beta;<sub>1</sub> X<sub>1</sub> +
...
&beta;<sub>p</sub> X<sub>p</sub> 
)}]

Key point:  *This assumption holds if X | Y = i is multivariate normal
with covariance matrix independent of i.* 

Those assumptions, by the way, are exactly those of Fisher linear
discriminant analysis.  In other words, LDA settings imply the logistic
model.

-----------------

Of course, models are always approximations, and the linear and logistic
models are no exception.  But multivariate data is indeed often roughly
mound-shaped, i.e. multivariate Gaussian-like, making the above
theoretical models reasonable choices for consideration. 

## Reasons cited for using "exponential" PR

When Y has the form of counts, a Poisson model naturally comes to mind.
However, unlike the linear and logistic cases, *there is no theoretical
justification for the standard PR model*,

mean Y =
exp[&beta;<sub>0</sub> +
&beta;<sub>1</sub> X<sub>1</sub> +
...
&beta;<sub>p</sub> X<sub>p</sub>]

Let's call this the Exponential Poisson model (EP).

Since most parametric regression analyses use linear models, a more
natural model would be the standard linear one,

mean Y = 
&beta;<sub>0</sub> +
&beta;<sub>1</sub> X<sub>1</sub> +
...
&beta;<sub>p</sub> X<sub>p</sub> 

Let's call this the Linear Poisson model (LP).

Advocates of EP are uncomfortable with LP.  Under the linear model, mean
Y could be negative in some instances, contrary to its being a mean of
counts.  Thus they use **exp()** to force the mean to be nonnegative.

## A closer look

A fundamental problem, often overlooked, is this:

-----------------

With use of EP instead of LP, the predictor effects &beta;<sub>i</sub>
change from **additive** to **multiplicative**.

-----------------

Say X<sub>2</sub> is age.  Under LP, 1 extra year of age adds
&beta;<sub>2</sub> to mean Y.  Under EP, 1 extra year of age
*multiplies* mean Y by exp(&beta;<sub>2</sub>).

In some applications, a multiplicative model is appropriate.  But users
should be aware of this major difference in models, and thus this major
difference in interpretations of the coefficients.

It must be noted that factor effects are not additive in logistic models
either.  However, the "data is often mound-shaped" argument at least
gives a theoretical basis for considering a logistic model.  EP has no
such basis, and if the application at hand does not have a qualitative
reason to assume multiplicativity, EP may not be justified.

## Issues with assumptions in LP

Count data, at least for small mean, is not approximately normal, and in
most cases it is not homoscedastic either.  However, neither of these is
a major problem.

For large n, the Central Limit Theorem (applied in large-sample theory)
shows that non-normality of the distribution of Y given X is not
relevant.  For small n, Gaussian linear model theory is not reliable,
since no variable in the real world is normally distributed.  One can
and should still do inference, of course, but not take it so literally. 

As to heteroscedastic variance of Y given X, one can use the *sandwich
estimator*.  See for instance the **car** and **sandwich** packages in
CRAN.

## Recommendations

One can of course try both models, LP and EP, doing model fit assessment
if the goal is Description or doing cross-validated assessment of
predictive ability if the goal is Prediction.  But again, in the
Description case, one must take care in interpeting the coefficients of
the two models.
