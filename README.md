# regtools

## Novel tools tools for linear, nonlinear and nonparametric regression.

These tools are associated with my forthcoming book, <i>From Linear
Models to Machine Learning: Statistical Regresison and Classification</i>,
CRC, 2017.  However, <i>the tools are useful in general, independently
of the book</i>.

## FEATURES:

* Nonparametric regression for general dimensions in predictor and
response variables, using k-NN.  Local-linear option.  Allows for
user-specified smoothing method.  Allows for accelerated exploration of
multiple values of <i>k</i> at once.  Tool to aid in choosing <i>k</i>.

* Innovative tools for assessing fit in linear and nonlinear parametric
models, via nonparametric methods.

* Tools for multiclass classification.  One vs. All and All vs. All.
Novel adjustment for artificially balanced data.

* Linear regression, PCA and log-linear model estimation in missing-data
setting , via the Available Cases method.

* Nicer implementation of ridge regression, with more meaningful scaling
and better plotting.

* Extension to nonlinear parametric regression with of Eickert-White
technique to handle heteroscedasticity.

* Misc. tools, e.g. Method of Moments estimation (including for
nonregression settings).

## EXAMPLES:

Let's take a look at the data set <b>prgeng</b>, some Census data for
California engineers and programmers in the year 2000.  (Some data
wrangling was performed first; type <b>?knnest</b> for the details.)

The fit assessment techniques in <b>regtools</b> gauge the fit of
parametric models by comparing to nonparametric ones.  Since the latter
are free of model bias, they are very useful in assessing the parametric
models.

The function <b>nonparvsxplot()</b> plots the nonparametric fits against
each predictor variable, for instance to explore nonlinear effects.
Here is the plot for wage versus (scaled) age:

<img src = "wagevsage.png">

Of course, the effects of the other predictors don't show up here, but
there does seem to be a quadratic effect, and the same was true for the
predictor measuring the number of weeks worked.  In our linear
parametric model, then, we included squared terms for these two
predictors.

Now run <b>parvsnonparplot()</b>, which plots the fit of the
parametric model against the nonparametric one.  Here is the result:

<img src = "parvsnonpar.png">

There seems to be some overfitting near the low end, and underfitting at
the high end.  This may be due to interaction effects, which should be
investigated.

Finally, let's check the classical assumption of homoscedasticity,
meaning that the conditional variance of Y given X is constant.  The
function <b>nonparvarplot()</b> plots the estimated conditional variance
against the estimated conditional mean:

<img src = "varvsmean.png">

Wow, a hockey stick!  Though there is a mild rise in <i>coefficient of
determination</i>, i.e.  standard deviation relative to the mean up to
about $80K, the slope increases sharply after that.

What to do?  As long as our linear regression model assumption holds,
violation of the homoscedasticity assumption won't invalidate our
estimates; they still will be <i>statistically consistent</i>.  But the
standard errors we compute, and thus the statistical inference we
perform, will be affected.  This is correctible using the  Eickert-White
procedure, which for linear models is available in the <b>car</b>
package, which is included in <b>regtools</b>.  Our package also extends
this to nonlinear parametric models, in our function <b>nlshc()</b> (the
validity of this extension is shown in the book).

Of course, the "hockey stick" form is another indication that we should
further investigate the model itself.  It may well be useful to fit two
separate models, one for incomes below $80K and the other for the higher
incomes.

But note carefully that the above graph is unaffected by the validity of
the parametric model; it is based purely on nonparametric analysis.
This is in contrast to classic regression fit methods.

