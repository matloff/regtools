
#  Overview of Machine Learning Methods

Here we give an overview of the most widely used predictive methods in
statistical/machine learning.  For each one, we present

* background

* overview of how it works

* **qe***-series function

## Notation

For convenience, we'll let Y denote the variable to be predicted, i.e.
the response variable, and let X denote the set of predictor
variables/features.

Our training data are (X<sub>1</sub>, Y<sub>1</sub>),.., (X<sub>n</sub>,
Y<sub>n</sub>).  We wish to predict new cases in the future, in which X
is known but Y needs to be predicted.

## Running example

The package's built-in dataset **mlb** consists of data on major league
baseball players.  

``` r
> data(mlb)
> head(mlb)
             Name Team       Position Height Weight   Age PosCategory
1   Adam_Donachie  BAL        Catcher     74    180 22.99     Catcher
2       Paul_Bako  BAL        Catcher     74    215 34.69     Catcher
3 Ramon_Hernandez  BAL        Catcher     72    210 30.78     Catcher
4    Kevin_Millar  BAL  First_Baseman     72    210 35.43   Infielder
5     Chris_Gomez  BAL  First_Baseman     73    188 35.71   Infielder
6   Brian_Roberts  BAL Second_Baseman     69    176 29.39   Infielder
```

## The  R package's **qe***-series functions

Here "qe" stanWds for "quick and easy."  The functions have a simple,
uniform interface, and most importantly, require no setup.  The call
form is

``` r
model fit <- qe<model name>(<data name>,<Y name>)
```

where Y is the name of the response variable.  No prior calls are needed
to define the model, etc.

## Example

Let's predict weight from height and age, using two methods, k-Nearest
Neighbor and random forests.

``` r
mlb <- mlb[,4:6]  # columns for height, weight and age
knnout <- qeKNN(mlb,'Weight')  # fit k-Nearest Neighbor model
rfout <- qeRF(mlb,'Weight')  # fit random forests model
```

Default values of hyperparameters are used but can be overridden.

Prediction of new cases is equally easy, in the form

``` r
predict(<model fit>, <new X value>)
```

Each **qe** function sets default values for the *tuning parameters*
or *hyperparameters* for the given ML method.  Nondefault values,
specific to the given ML, can optionally be specified.

## Regression and classification problems, regression functions

Prediction applications in which Y is a continuous variable, say weight,
or at least ordinal, are called *regression settings*.  Applications in
which Y is categorical, i.e. Y is a factor variable in R, say predicting
the player's position (e.g.  Pitcher) are *classification settings*.

Somewhat confusingly, both settings make use of the *regression function*,
m(t) = E(Y | X = t), the mean value of Y in the subpopulation defined by
X = t.  If say we are predicting weight in the **mlb** data, then for instance
m(71,28) would be the mean weight among all players of height 71 inches
and 28 years old.  To predict the weight of a new player, say height 77
age 19, we use m(77,19).

In classification problems, Y is converted to a set of indicator
variables.  For the position Pitcher in the **mlb** data, we would have
Y = 1 or 0, depending on whether the player is a pitcher or not.
(Position is in column 3 of the orig
Then  E(Y | X = t) reduces to P(Y = 1 | X = t), the probability that the
player is a pitcher given the player's height, weight and age, say.

In other words, the regression function m(t) is central to both regression
and classification settings.  The statistical/machine learning methods
presented here amount to ways to estimate m(t).  The methods are
presented below in an order that shows connection between them.

Full description of the methods will be available in my forthcoming
book, *The Art of Machine Learning: Algorithms+Data+R*.

## ML predictive methods

### k-Nearest Neighbors

This method was originally developed by statisticians, starting in the 1950s
and 60s.

It's very intuitive.  To predict, say, the weight of a new
player of height 72 and age 25, we find the k closest players in our 
training data to (72,25), and average their weights.  This is our
estimate of m(72,25), and we use it as our prediction.

The **qeKNN()** function wraps **kNN()** in **regtools**.  The main
hyperparameter is the number of neighbors.

### Random forests

This method was developed mainly by statisticians, starting in the
1980s.

This is a natural extension of k-NN, in that it too creates a
neighborhood and averages Y values within the neighborhood.  However, it
does so in a different way, creating trees.

Say we are predicting player position in the **mlb** data, from height,
weight and age.  (E.g. catchers tend to be heavier and older.)  We first
ask whether the height is above or below a certain threshold.  After
that, we ask whether weight is above or below a certain (different)
threshold.  This creates a "box" in height-weight space.  We then might
subdivide the box according to whether age is above or below a
threshold.

The word *might* in the last sentence alludes to the fact that the
process may stop early, if the current subdivision is judged fine enough
to produce good accuracy.  And one generally wants to avoid having
neighborhoods (*nodes* in the tree) that don't have many data points;
this is controlled by a hyperparameter.

Clearly, the order in which the predictor variables are evaluated
(height, weight and age above) can matter a lot.  So, more than one tree
is constructed, with random orders.  The number of trees is another
hyperparameter.

The **qeRF()** function wraps the function of the same name in the
**randomForests** package.

## Boosting

This method has been developed both by CS and statistics people.  The
latter have been involved mainly in gradient boosting, the technique
used here.

The **qeGBoost()** wraps **gbm()** in the package of the same name.  It
is tree-based, with hyperparameter similar to the random forests case,
plus a *learning rate*.  The latter controls the size of iteration
steps.

