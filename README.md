# regtools 

## Novel tools tools for linear, nonlinear and nonparametric regression and classification

These tools are associated with my book, <i>From Linear
Models to Machine Learning: Statistical Regression and
Classification</i>, N. Matloff, CRC, 2017 (recipient of the
*Technometrics* Eric Ziegel Award for Best Book Reviewed in 2017).

The tools are useful in general, **independently of the book**.

## FEATURES:

* Innovative graphical tools for assessing fit in linear and nonlinear
  parametric models, via nonparametric methods.  Model evaluation,
examination of quadratic effects, investigation of nonhomogeneity of
variance.

* Tools for multiclass classification, parametric and nonparametric, for
  any number of classes.  One vs. All and All vs. All paradigms.  Novel
adjustment for artificially balanced (or undesirably imbalanced) data.

* Nonparametric regression for general dimensions in predictor and
  response variables, using k-Nearest Neighbors (k-NN).  Local-linear
option to deal with edge aliasing.  Allows for user-specified smoothing
method.  Allows for accelerated exploration of multiple values of **k**
at once.  Tool to aid in choosing **k**.

* Extension to nonlinear parametric regression of Eicker-White
technique to handle heteroscedasticity.

* Utilities for conversion of time series data to rectangular form,
  enabling lagged prediction by **lm()** or other regression model.

* Linear regression, PCA and log-linear model estimation in missing-data
setting, via the Available Cases method.  (For handling missing values
in Prediction contexts, see
[our toweranNA package](http://github.com/matloff/toweranNA).)

* Utilities for conversion between factor and dummy variable forms,
  useful since among various regression packages, some use factors while
some others use dummies.  (The **lars** package is an example of the
latter case.)

* Misc. tools, e.g. to reverse the effects of an earlier call to
  **scale()**.

* Nicer implementation of ridge regression, with more meaningful scaling
and better plotting.

* Interesting datasets.

A number of examples of use follow below.

## EXAMPLE:  PARAMETRIC MODEL FIT ASSESSMENT

The fit assessment techniques in **regtools** gauge the fit of
parametric models by comparing to nonparametric ones.  Since the latter
are free of model bias, they are very useful in assessing the parametric
models.

Let's take a look at the included dataset **prgeng**, some Census data
for California engineers and programmers in the year 2000. The response
variable in this example is wage income, and the predictors are age,
gender, number of weeks worked, and dummy variables for MS and PhD
degrees.  You can read the details of the data by typing

``` r
> ?prgeng
```

One of the package's graphical functions for model fit assessment plots the
parametric (e.g. **lm()**) values against the nonparametric fit via
k-NN.  Let's try this on the Census data.

The package includes three versions of the dataset:  The original; a
version with categorical variables in dummy form; and a version with
categorical variables in R factor form.  Since the k-NN routines require
dummies, we'll use that first version, **peDumms**.

We need to generate the parametric and nonparametric fits, then call
**parvsnonparplot()**:

``` r
data(peDumms)
pe1 <- peDumms[c('age','educ.14','educ.16','sex.1','wageinc','wkswrkd')]
lmout <- lm(wageinc ~ .,data=pe1)
xd <- preprocessx(pe1[,-5],10)  # prep for k-NN, k <= 10
knnout <- knnest(pe1$wageinc,xd,10)
parvsnonparplot(lmout,knnout)
```

![result](inst/images/ParVsNonpar.png)

We see above how the k-NN code is used.  We first call **preprocessx()** 
to determine the nearest neighbors of each data point.  Here **k** is
10, so we can later compute various k-NN fits for **k** anywhere from 1
to 10.  (A somewhat simpler function, **basicKNN()**, is also
available.) The actual fit is done by **knnest()**.  Then
**parvsnonparplot()** plots the linear model fit against the
nonparametric one..  Again, since the latter is model-free, it serves as
a good assessment of the fit of the linear model.

There is quite a bit suggested in this picture:

* There seems to be some overestimating near the low end, and quite
substantial underestimating at the high end.  

* There are intriguing "streaks" or "tails" of points, suggesting the
  possible existence of small but important subpopulations arising
  from the dummy variables.  Moreover, the plot suggests two separate large 
  subpopulations, for wages less than or greater than about $40,000, 
  possibly related to full- vs. part-time employment.

* There appear to be a number of people with 0 wage income. Depending on
the goals of our analysis, we might consider removing them.

Let's now check the classical assumption of homoscedasticity,
meaning that the conditional variance of Y given X is constant.  The
function <b>nonparvarplot()</b> plots the estimated conditional variance
against the estimated conditional mean, both computed nonparametrically:

![result](inst/images/PrgengVar.png)

Though we ran the plot thinking of the homoscedasticity assumption, and
indeed we see larger variance at large mean values, this
is much more remarkable, confirming that there are interesting
subpopulations within this data.  These may correspond to different
occupations, something to be investigated.

The package includes various other graphical diagnostic functions.

By the way, violation of the homoscedasticity assumption won't
invalidate the estimates in our linear model.  They still will be
*statistically consistent*.  But the standard errors we compute, and
thus the statistical inference we perform, will be affected.  This is
correctible using the Eicker-White procedure, which for linear models is
available in the **car** and **sandwich** packages.  Our package here
also extends this to nonlinear parametric models, in our function
<b>nlshc()</b> (the validity of this extension is shown in the book).

## EXAMPLE; OVA VS. AVA IN MULTICLASS PROBLEMS

A very popular prediction method in 2-class problems is to use logistic
(logit) regression. In analyzing click-through patterns of Web users,
for instance, we have 2 classes, Click and Nonclick.  We might fit a
logistic model for Click, given user Web history, demographics and so
on.  Note that logit actually models probabilities, e.g. the probability
of Click given the predictor variables.

But the situation is much less simple in multiclass settings. Suppose
our application is recognition of hand-written digits (a famous machine
learning example). The predictor variables are pixel patterns in images.
There are two schools of thought on this:

* *One vs. All (OVA):*  We would run 10 logistic regression models, one
  for predicting '0' vs. non-'0', one for '1' vs. non-'1', and so on.
For a particular new image to be classified, we would thus obtain 10
estimated conditional probabilities.  We would then guess the digit for
this image to be the digit with the highest estimated conditional
probability.

* *All vs. All (AVA):*  Here we would run C(10,2) = 45 logit
analyses, one for each pair of digits.  There would be one for '0' vs.
'1', one for '0' vs. '2', etc., all the way up through '8' vs. '9'.
In each case there is a "winner" for our new image to be predicted, and
in the end we predict the new image to be whichever digit has the most
winners.

Many in the machine learning literature recommend AVA over OVA, on the
grounds that there might be linear separability (in the statistical
sense) in pairs but not otherwise.  My book counters by noting that
such a situation could be remedied under OVA by adding quadratic terms
to the logit models.

At any rate, the **regtools** package gives you a choice, OVA or AVA,
for both parametric and nonparametric methods.  For example,
**avalogtrn()** and **avalogpred()** do training and prediction
operations for logit with AVA.

Let's look at an example, again using the Census data from above.  We'll
predict occupation from age, sex, education (MS, PhD, other) wage income
and weeks worked.

``` r
data(peFactors) 
pef <- peFactors 
pef1 <- pef[,c('age','educ','sex','wageinc','wkswrkd','occ')] 
# "Y" must be in last column, class ID 0,1,2,...; convert from factor
pef1$occ <- as.numeric(pef1$occ) 
pef1$occ <- pef1$occ - 1
pef2 <- pef1 
# create the education, gender dummy varibles
pef2$ms <- as.integer(pef2$educ == 14) 
pef2$phd <- as.integer(pef2$educ == 16) 
pef2$educ <- NULL 
pef2$sex <- as.integer(pef2$sex == 1) 
pef2 <- pef2[,c(1,2,3,4,6,7,5)] 
ovaout <- ovalogtrn(6,pef2) 
# estimated coefficients, one set ofr each of the 6 classes
ovaout  
# prints
                        0             1             2
(Intercept) -9.411834e-01 -6.381329e-01 -2.579483e-01
xage         9.090437e-03 -3.302790e-03 -2.205695e-02
xsex        -5.187912e-01 -1.122531e-02 -9.802006e-03
xwageinc    -6.741141e-06 -4.609168e-06  5.132813e-06
xwkswrkd     5.058947e-03 -2.247113e-03  2.623924e-04
xms         -5.201286e-01 -4.272846e-01  5.280520e-01
xphd        -3.302821e-01 -8.035287e-01  3.531951e-01
                        3             4             5
(Intercept) -3.370758e+00 -3.322356e+00 -4.456788e+00
xage        -2.193359e-03 -1.206640e-02  3.323948e-02
xsex        -7.856923e-01  5.173516e-01  1.175657e+00
xwageinc    -4.076872e-06  2.033175e-06  1.831774e-06
xwkswrkd     1.311084e-02  5.517912e-04  2.794453e-03
xms         -1.797544e-01  9.947253e-02  2.705293e-01
xphd        -3.883463e-01  4.967115e-01  4.633907e-01
# predict the occupation of a woman, age 35, no MS/PhD, inc 60000, 52
# weeks worked
ovalogpred(ovaout,matrix(c(35,0,60000,52,0,0),nrow=1))
# outputs class 2, Census occupation code 102
[1] 2
```

With the optional argument **probs=TRUE**, the call to **ovalogpred()**
will also return the conditional probabilities of the classes, given the
predictor values, in the R attribute 'probs'.

Here is the AVA version:

``` r
avaout <- avalogtrn(6,pef2) 
avaout
# prints
                      1,2           1,3           1,4           1,5
(Intercept) -1.914000e-01 -4.457460e-01  2.086223e+00  2.182711e+00
xijage       8.551176e-03  2.199740e-02  1.017490e-02  1.772913e-02
xijsex      -3.643608e-01 -3.758687e-01  3.804932e-01 -8.982992e-01
xijwageinc  -1.207755e-06 -9.679473e-06 -6.967489e-07 -4.273828e-06
xijwkswrkd   4.517229e-03  4.395890e-03 -9.535784e-03 -1.543710e-03
xijms       -9.460392e-02 -7.509925e-01 -2.702961e-01 -5.466462e-01
xijphd       3.983077e-01 -5.389224e-01  7.503942e-02 -7.424787e-01
                      1,6           2,3           2,4           2,5
(Intercept)  3.115845e+00 -2.834012e-01  2.276943e+00  2.280739e+00
xijage      -2.139193e-02  1.466992e-02  1.950032e-03  1.084527e-02
xijsex      -1.458056e+00  3.720012e-03  7.569766e-01 -5.130827e-01
xijwageinc  -5.424842e-06 -9.709168e-06 -1.838009e-07 -4.908563e-06
xijwkswrkd  -2.526987e-03  9.884673e-04 -1.382032e-02 -3.290367e-03
xijms       -6.399600e-01 -6.710261e-01 -1.448368e-01 -4.818512e-01
xijphd      -6.404008e-01 -9.576587e-01 -2.988396e-01 -1.174245e+00
                      2,6           3,4           3,5           3,6
(Intercept)  3.172786e+00  2.619465e+00  2.516647e+00  3.486811e+00
xijage      -2.908482e-02 -1.312368e-02 -3.051624e-03 -4.236516e-02
xijsex      -1.052226e+00  7.455830e-01 -5.051875e-01 -1.010688e+00
xijwageinc  -5.336828e-06  1.157401e-05  1.131685e-06  1.329288e-06
xijwkswrkd  -3.792371e-03 -1.804920e-02  5.606399e-04 -3.217069e-03
xijms       -5.987265e-01  4.873494e-01  2.227347e-01  5.247488e-02
xijphd      -1.140915e+00  6.522510e-01 -2.470988e-01 -1.971213e-01
                      4,5           4,6           5,6
(Intercept) -9.998252e-02  6.822355e-01  9.537969e-01
xijage       1.055143e-02 -2.273444e-02 -3.906653e-02
xijsex      -1.248663e+00 -1.702186e+00 -4.195561e-01
xijwageinc  -4.986472e-06 -7.237963e-06  6.807733e-07
xijwkswrkd   1.070949e-02  8.097722e-03 -5.808361e-03
xijms       -1.911361e-01 -3.957808e-01 -1.919405e-01
xijphd      -8.398231e-01 -8.940497e-01 -2.745368e-02
# predict the occupation of a woman, age 35, no MS/PhD, inc 60000, 52
# weeks worked
avalogpred(6,ovaout,matrix(c(35,0,60000,52,0,0),nrow=1))
# outputs class 2, Census occupation code 102
```

## EXAMPLE:  MULTICLASS k-NN CLASSIFICATION WITH knnest

In addition to use in linear regression graphical diagnostics, k-NN can
be very effective as a nonparametric regression/machine learning tool.
I would recommend it in cases in which the number of predictors is
moderate and there are nonmonotonic relations.  (See also 
[our polyreg package](http://github.com/matloff/polyreg).)
Let's continue the above example on predicting occupation, using k-NN.

The "industrial strength" k-NN functions in **regtools**, described
below.  (See also the simpler **basicKNN()**, described later in this
document.)

The three components are:

1. **preprocessx()**:  This finds the sets of nearest neighbors in the
   training set, for all values of **k** up to a user-specified maximum.
This facilitates the user's trying various values of **k**.

2.  **knnest()**:  This fits the regression model.

3.  **knnpred()**:  This does prediction on the user's desired set of
    points of new cases.

Since k-NN involves finding distances between points, our data must be
numeric, not factors.  This means that in **pef2**, we'll need to
replace the **occ** column by a matrix of dummy variables.  Utilities in
the **regtools** package make this convenient:

``` r
occDumms <- factorToDummies(as.factor(pef2$occ),'occ',omitLast=FALSE)
pef3 <- cbind(pef2[,-7],occDumms)
```

Note that in cases in which "Y" is multivariate, **knnest()** requires
it in multivariate form.  Here "Y" is 6-variate, so we've set the last 6
columns of **pef3** to the corresponding dummies.

Many popular regression packages, e.g. **lars** for the LASSO, require
data in numeric form, so the **regtools**' conversion utilities are quite
handy.

Now fit the regression model:

``` r
kout <- knnest(pef3[, -(1:6)],xd,10) 
```

One of the components of **kout** is the matrix of fitted values:

``` r
> head(kout$regest) 
     occ.0 occ.1 occ.2 occ.3 occ.4 occ.5
[1,]   0.2   0.4   0.2     0   0.0   0.2
[2,]   0.2   0.5   0.2     0   0.0   0.1
[3,]   0.5   0.1   0.3     0   0.1   0.0
[4,]   0.3   0.4   0.1     0   0.0   0.2
[5,]   1.0   0.0   0.0     0   0.0   0.0
[6,]   0.2   0.4   0.2     0   0.0   0.2
```

So for example the conditional probability of Occupation 4 for the
third observation is 0.1.

Now let's do the same prediction as above:

``` r
> predict(kout,matrix(c(35,0,60000,52,0,0),nrow=1),TRUE)
occ.0 occ.1 occ.2 occ.3 occ.4 occ.5 
  0.1   0.4   0.5   0.0   0.0   0.0 
```

These are conditional probabilities.  The most likely one is Occupation
2.

The TRUE argument was to specify that we need to scale the new cases in
the same way the original data were scaled.

By default, our k-NN routines find the mean Y in the neighborhood.
Another option is to do local linear smoothing.  Among other things,
this may remedy aliasing at the edges of the data.  This should be done
with a value of **k** much larger than the number of predictor
variables.

## EXAMPLE:  REGRESSION ANALYSIS WITH basicKNN()

The k-NN function **knnest()** and its related unctions are rather
complex.  Those who are new to k-NN should use the simpler **basicKNN()**.
Again, it can be used for both regression and classification (viewing
the latter as a special case of the former).

Our example here will be **day1** from the famous bike sharing dataset 
from the [UC Irvine Machine Learning Repository](https://archive.ics.uci.edu).
It is included in **regtools** by permission of the data
curator.  A detailed description of the data is available on the 
[UCI site](https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset).

(The original data is in **day**.  It has some transformed values for
the weather variables; here we'll use **day1**, which retains the
original scale.)

We'll predict total ridership.  Let's start out using as features just
the dummy for working day, and the numeric weather variables, columns 8,
10-13 and 16:

``` r
> data(day1)
> day1 <- day1[,c(8,10:13,16)]
> head(day1)
  workingday     temp     atemp      hum windspeed  tot
1          0 8.175849  7.999250 0.805833 10.749882  985
2          0 9.083466  7.346774 0.696087 16.652113  801
3          1 1.229108 -3.499270 0.437273 16.636703 1349
4          1 1.400000 -1.999948 0.590435 10.739832 1562
5          1 2.666979 -0.868180 0.436957 12.522300 1600
6          1 1.604356 -0.608206 0.518261  6.000868 1606
```

In its simplest version, the call form is

``` r
basicKNN(x,y,newx,k)
```

where the arguments are:

* **x:** the "X" matrix, i.e. matrix of predictor values,
for the training set (it cannot be a data frame, as nearest-neighbor 
distances between rows must be computed); in this case, it's the entire
data except for the **tot** column

* **y:** the "Y" vector for the training set, i.e. the response values,
in this case the **tot** column

* **newx:** a vector of feature values for a new case to be predicted

* **k:** the number of nearest neighbors we wish to use

So, say you are the manager this morning, and the day is a working day,
with temperature 12.0, atemp 11.8, humidity 23% and wind at 5 miles per
hour.  What is your prediction for the ridership?

``` r
> day1x <- day1[,1:5]
> tot <- day1$tot
> knnout <- basicKNN(day1x,tot,c(1,12.0,11.8,0.23,5),5)
> knnout
$whichClosest
     [,1] [,2] [,3] [,4] [,5]
[1,]  459  481  469  452   67

$regests
[1] 5320.2
```

The output shows which rows in the training set were closest to the
point to be predicted --- rows 459, 481 and so on --- and the prediction
itself.  Our prediction would then be about 5320 riders. 

## EXAMPLE: USING basicKNN() IN MULTICLASS PROBLEMS

Let's redo the occupation-prediction example from above, using
**basicknn()**.  

Again, the arguments in the simple version ae:

``` r
function (x,y,newx,kmax)
```

Here **x** and **y** must be matrices.

``` r
> basicKNN(pef3[,1:6],as.matrix(pef3[7:12]),c(35,0,60000,52,0,0),10)
$whichClosest
      [,1]  [,2]  [,3]  [,4]  [,5]  [,6] [,7]  [,8] [,9] [,10]
[1,] 14261 15552 16514 13592 13973 13709 1527 12579 5900 10914

$regests
      [,1]
occ.0  0.1
occ.1  0.4
occ.2  0.5
occ.3  0.0
occ.4  0.0
occ.5  0.0

```

Same answer as before, guessing Occupation 2.

## EXAMPLE:  ADJUSTMENT OF CLASS PROBABILITIES IN CLASSIFICATION PROBLEMS

The **LetterRecognition** dataset in the **mlbench** package lists
various geometric measurements of capital English letters, thus another
image recognition problem.  One problem is that the frequencies of the
letters in the dataset are not similar to those in actual English texts.
The correct frequencies are given in the **ltrfreqs** dataset included
here in the **regtools** package.

In order to adjust the analysis accordingly, the **ovalogtrn()**
function has an optional **truepriors** argument.  For the letters
example, we could set this argument to **ltrfreqs**.  The function
**knntrn()** also has such an argument.  (The term *priors*
here does NOT refer to a subjective Bayesian analysis. It is merely a
standard term for the class probabilities.)

In the book, it is shown that the use of correct priors increased the
rate of correct classification from 75 to 88%.

## EXAMPLE:  RECTANGULARIZATION OF TIME SERIES

This allows use of ordinary tools like **lm()** for prediction in time
series data.  Since the goal here is prediction rather than inference,
an informal model can be quite effective, as well as convenient.

The basic idea is that **x[i]** is predicted by
**x[i-lg],
x[i-lg+1],
x[i-lg+2],
i...
x[i-1]**, 
where **lg** is the lag.

``` r
xy <- TStoX(Nile,5)
head(xy)
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,] 1120 1160  963 1210 1160 1160
# [2,] 1160  963 1210 1160 1160  813
# [3,]  963 1210 1160 1160  813 1230
# [4,] 1210 1160 1160  813 1230 1370
# [5,] 1160 1160  813 1230 1370 1140
# [6,] 1160  813 1230 1370 1140  995
head(Nile,36)
#  [1] 1120 1160  963 1210 1160 1160  813 1230 1370 1140  995  935 1110  994 1020
# [16]  960 1180  799  958 1140 1100 1210 1150 1250 1260 1220 1030 1100  774  840
# [31]  874  694  940  833  701  916
```

Try **lm()**:

``` r
lmout <- lm(xy[,6] ~ xy[,1:5])
lmout
...
Coefficients:
Coefficients:
(Intercept)   xy[, 1:5]1   xy[, 1:5]2   xy[, 1:5]3   xy[, 1:5]4   xy[, 1:5]5  
  307.84354      0.08833     -0.02009      0.08385      0.13171      0.37160  
```

Predict the 101st observation:

``` r
cfs <- coef(lmout)
cfs %*% c(1,Nile[96:100])
#          [,1]
# [1,] 784.4925
```
